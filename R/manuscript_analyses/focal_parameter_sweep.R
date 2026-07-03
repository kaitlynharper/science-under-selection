##############################################################################
# Focal parameter sweep (manuscript)
#
# Description: n_batches independent batches over the three focal parameters
# (sample size, publication bias, base null probability). Each batch runs
# n_sims_per_batch simulations.
#
# Output folder (created manually before first run):
#   R/manuscript_analyses/output/focal_parameter_sweep/
#
# Files in that folder:
#   lhs_design.rds              — all param rows + global seeds; written once
#   batch_log.txt               — append-only log of completed batch numbers
#   batch_01.rds … batch_20.rds — one per completed batch (renamed from run_sweep output)
#   focal_sweep_combined.rds    — combined once all batches are present
#
# Workflow summary:
#   1. Load or create LHS design
#   2. Read batch_log (+ reconcile with batch_XX.rds on disk) to find missing batches
#   3. For each missing batch: set params → source(run_sweep.R) → rename output → append log
#   4. When all batches done: combine batch files → focal_sweep_combined.rds
##############################################################################

# ---- Load packages ----
library(here)
library(dplyr)
library(lhs)

# ---- Source simulation code ----
# Source all functions once before parallel execution
function_files <- list.files(here("R", "functions"), full.names = TRUE)
sapply(function_files, source, .GlobalEnv)
source(here("R", "model.R"))

##############################################################################
#### FROZEN MANUSCRIPT CONFIG ####
# Copied from set_sweep_parameters.R at time of manuscript analysis.
##############################################################################

n_sims_per_batch <- 200L
n_batches <- 10L
n_cores <- parallel::detectCores() - 1
max_sweep_topups <- 3L # re-run missing seeds within a batch if parallel jobs fail

sweep_param_names <- c(
  "hold_samples_constant_at",
  "nonsig_logistic_midpoint",
  "base_null_probability"
)

base_params <- list(
  n_agents = 1000,
  n_timesteps = 350,
  n_timesteps_per_career_step = 35,
  duration_per_observation = 0.1,
  duration_original_intercept = 1,
  n_effects = 500000,
  base_null_probability = 0.9,
  effect_size_mean = 0.3,
  effect_size_variance = 0.1,
  uninformed_prior_mean = 0,
  uninformed_prior_variance = 1,
  initial_selection_condition = 0,
  switch_conditions_at = NA,
  career_turnover_selection_rate = 0.5,
  innovation_sd = 0,
  mutation_rate = 0.1,
  initial_replication_rate = 0,
  hold_samples_constant_at = 50,
  replications_dynamic_sample_sizes = 1,
  publication_bias = 2,
  nonsig_logistic_midpoint = NA,
  all_replications_published = 0,
  burn_in_period = 35,
  truth_contribution_method = "savage_dickey"
)

# Only the focal parameters (ranges frozen for this analysis)
sweepable_params <- list(
  hold_samples_constant_at = list(
    min = 10,
    max = 200,
    label = "Sample Size",
    color = "#E69F00",
    log_scale = TRUE
  ),
  nonsig_logistic_midpoint = list(
    min = -0.5,
    max = 3,
    label = "Publication Bias",
    color = "#56B4E9"
  ),
  base_null_probability = list(
    min = 0,
    max = 1,
    label = "Base Null Probability",
    color = "#009E73"
  )
)

stopifnot(all(sweep_param_names %in% names(sweepable_params)))
param_config <- sweepable_params[sweep_param_names]

# Latin hypercube sampling (frozen copy)
draw_params <- function(n = 1) {
  k <- length(param_config)
  h <- randomLHS(n, k)
  df <- data.frame(row.names = seq_len(n))
  for (i in seq_len(k)) {
    nm <- names(param_config)[i]
    spec <- param_config[[i]]
    a <- spec$min
    b <- spec$max
    if (isTRUE(spec$log_scale)) {
      val <- exp(qunif(h[, i], log(a), log(b)))
      if (nm == "hold_samples_constant_at") {
        val <- round(val)
      }
      df[[nm]] <- val
    } else {
      df[[nm]] <- qunif(h[, i], a, b)
    }
  }
  df
}

##############################################################################
#### PATHS ####
##############################################################################

analysis_dir <- here(
  "R",
  "manuscript_analyses",
  "output",
  "focal_parameter_sweep"
)
lhs_path <- file.path(analysis_dir, "lhs_design.rds")
batch_log_path <- file.path(analysis_dir, "batch_log.txt")
combined_path <- file.path(analysis_dir, "focal_sweep_combined.rds")

batch_path <- function(batch_id) {
  file.path(analysis_dir, sprintf("batch_%02d.rds", batch_id))
}

if (!dir.exists(analysis_dir)) {
  stop(
    "Output folder not found: ",
    analysis_dir,
    "\nCreate it manually before running this script."
  )
}

##############################################################################
#### LHS DESIGN (write once, load thereafter) ####
##############################################################################

if (!file.exists(lhs_path)) {
  message(
    "Drawing LHS design (",
    n_batches,
    " batches x ",
    n_sims_per_batch,
    ") ..."
  )

  batch_dfs <- vector("list", n_batches)
  for (batch_id in seq_len(n_batches)) {
    # Independent LHS per batch; seed offsets keep batches reproducible
    set.seed(1000L + batch_id)
    batch_df <- draw_params(n_sims_per_batch)

    seed_start <- (batch_id - 1L) * n_sims_per_batch
    batch_df$seed <- seed_start + seq_len(n_sims_per_batch)
    batch_df$batch_id <- batch_id
    batch_dfs[[batch_id]] <- batch_df
  }

  lhs_design <- bind_rows(batch_dfs)
  saveRDS(lhs_design, lhs_path)
  message("Saved LHS design to ", lhs_path)
} else {
  lhs_design <- readRDS(lhs_path)
  message("Loaded existing LHS design from ", lhs_path)
}

expected_n <- n_batches * n_sims_per_batch
if (nrow(lhs_design) != expected_n) {
  stop(
    "lhs_design.rds has ",
    nrow(lhs_design),
    " rows; expected ",
    expected_n,
    ". Delete lhs_design.rds only if you intend to restart this analysis."
  )
}

##############################################################################
#### PROGRESS: batch_log + reconciliation with batch files on disk ####
##############################################################################

# Batch numbers logged on previous runs (one integer per line)
completed_from_log <- integer(0)
if (file.exists(batch_log_path)) {
  log_lines <- readLines(batch_log_path, warn = FALSE)
  log_lines <- trimws(log_lines)
  log_lines <- log_lines[nzchar(log_lines)]
  if (length(log_lines) > 0) {
    completed_from_log <- as.integer(sub("^([0-9]+).*$", "\\1", log_lines))
  }
}

# Batch files on disk also count as complete (covers crash after rename, before log)
batch_files_on_disk <- list.files(
  analysis_dir,
  pattern = "^batch_\\d{2}\\.rds$",
  full.names = FALSE
)
completed_from_disk <- as.integer(
  sub("^batch_(\\d{2})\\.rds$", "\\1", batch_files_on_disk)
)

completed_batches <- sort(unique(c(completed_from_log, completed_from_disk)))

# Backfill log for any batch files missing from the log
not_in_log <- setdiff(completed_from_disk, completed_from_log)
if (length(not_in_log) > 0) {
  message(
    "Backfilling batch_log for batch(s): ",
    paste(not_in_log, collapse = ", ")
  )
  cat(
    paste(not_in_log, collapse = "\n"),
    "\n",
    file = batch_log_path,
    append = TRUE
  )
}

missing_batches <- setdiff(seq_len(n_batches), completed_batches)

if (length(missing_batches) == 0) {
  message("All ", n_batches, " batches already complete.")
} else {
  message(
    "Batches to run: ",
    length(missing_batches),
    " (",
    paste(missing_batches, collapse = ", "),
    ")"
  )
}

##############################################################################
#### BATCH LOOP (one run_sweep call per missing batch) ####
##############################################################################

for (batch_id in missing_batches) {
  message(
    "\n--- Batch ",
    batch_id,
    " / ",
    n_batches,
    " (seeds ",
    (batch_id - 1L) * n_sims_per_batch + 1L,
    "-",
    batch_id * n_sims_per_batch,
    ") ---"
  )

  # Subset LHS rows for this batch; run_sweep expects these objects in the env
  sweep_params_full <- lhs_design[
    lhs_design$batch_id == batch_id,
    ,
    drop = FALSE
  ]
  n_sims <- n_sims_per_batch

  # Runs parallel sims and saves output/sweep_results_<timestamp>.rds
  source(here("R", "run_sweep.R"), local = FALSE)

  # Move timestamped output to stable batch file in the analysis folder
  dest <- batch_path(batch_id)
  src <- here(sweep_path)
  if (!file.rename(src, dest)) {
    stop("Failed to move ", src, " to ", dest)
  }

  # Append batch number to log (append-only progress tracker)
  cat(batch_id, "\n", file = batch_log_path, append = TRUE)
  message("Batch ", batch_id, " saved to ", dest)
}

##############################################################################
#### COMBINE AT END (only when all batches present) ####
##############################################################################

all_batch_files <- batch_path(seq_len(n_batches))
all_batches_exist <- all(file.exists(all_batch_files))

if (!all_batches_exist) {
  n_done <- sum(file.exists(all_batch_files))
  message(
    "\nCombine skipped: ",
    n_done,
    " / ",
    n_batches,
    " batch files present. Re-run this script to continue."
  )
} else if (file.exists(combined_path)) {
  message("\nCombined file already exists: ", combined_path)
} else {
  message("\nCombining ", n_batches, " batch files ...")

  batches <- lapply(all_batch_files, readRDS)
  combined_meta <- batches[[1]]$meta
  combined_meta$manuscript_analysis <- "focal_parameter_sweep"
  combined_meta$n_batches <- n_batches
  combined_meta$n_sims_per_batch <- n_sims_per_batch
  combined_meta$n_total_sims <- expected_n
  combined_meta$combined_at <- Sys.time()

  focal_sweep_combined <- list(
    meta = combined_meta,
    results = bind_rows(lapply(batches, `[[`, "results"))
  )

  saveRDS(focal_sweep_combined, combined_path)
  message(
    "Saved combined results (",
    nrow(focal_sweep_combined$results),
    " rows) to ",
    combined_path
  )
}
