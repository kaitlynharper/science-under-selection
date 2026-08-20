##############################################################################
# Focal parameter sweep (for the manuscript)
#
# Description: n_batches independent batches over the three focal parameters
# (sample size, publication bias, base null probability). Each batch runs
# n_sims_per_batch simulations.
#
# Output folder:
#   R/manuscript_analyses/output/focal_parameter_sweep/
#
# Files in that folder:
#   lhs_design.rds              — all param rows + global seeds; written once
#   batch_log.txt               — append-only log of completed batch numbers
#   batch_01.rds … batch_20.rds — one per completed batch (renamed from 04_run_sweep output)
#   focal_sweep_combined.rds    — combined once all batches are present
#
# Workflow summary:
#   1. Load or create LHS design
#   2. Read batch_log (+ reconcile with batch_XX.rds on disk) to find missing batches
#   3. For each missing batch: set params → source(04_run_sweep.R) → rename output → append log
#   4. When all batches done: combine batch files → focal_sweep_combined.rds
##############################################################################

# Load packages
library(here)
library(dplyr)
library(lhs)

# Source simulation code
function_files <- list.files(here("R", "functions"), full.names = TRUE)
sapply(function_files, source, .GlobalEnv)
source(here("R", "00_model.R"))

##############################################################################
#### FROZEN MANUSCRIPT CONFIG ####
##############################################################################

n_sims_per_batch <- 2000L
n_batches <- 5L
n_cores <- parallel::detectCores() - 1
max_sweep_topups <- 3L # re-run missing seeds within a batch if any parallel jobs fail

# Focal parameters to sweep
sweep_param_names <- c(
  "hold_samples_constant_at",
  "nonsig_logistic_midpoint",
  "base_null_probability"
)

# Default parameters
base_params <- list(
  n_agents = 1000,
  n_timesteps = 1000,
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
  mutation_rate = 0.02,
  initial_replication_rate = 0,
  hold_samples_constant_at = 50,
  replications_dynamic_sample_sizes = 1,
  publication_bias = 1, # 0 = no publication bias, 1 = publication bias on
  sig_lower_asymptote = 0, # minimum publication probability for p < .05 results
  sig_logistic_midpoint = -0.5, # novelty midpoint for significant results
  sig_logistic_steepness = 3, # steepness of logistic curve for significant results
  nonsig_logistic_midpoint = 3, # novelty midpoint for non-significant results (swept)
  nonsig_logistic_steepness = 3, # steepness of logistic curve for non-significant results
  all_replications_published = 0,
  burn_in_period = 35,
  truth_contribution_method = "savage_dickey"
)

# Focal parameters that are swept
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

# Check that all focal parameters are in the sweepable_params list
stopifnot(all(sweep_param_names %in% names(sweepable_params)))
param_config <- sweepable_params[sweep_param_names]

# Latin hypercube sampling: n points in the unit cube, then mapped onto each
# parameter's [min, max] range (log-uniform if log_scale is TRUE).
draw_params <- function(n = 1) {
  n_params <- length(param_config)
  lhs_unit <- randomLHS(n, n_params) # each column is Uniform(0, 1)
  param_draws <- data.frame(row.names = seq_len(n))
  for (i in seq_len(n_params)) {
    param_name <- names(param_config)[i]
    param_spec <- param_config[[i]]
    param_min <- param_spec$min
    param_max <- param_spec$max
    if (isTRUE(param_spec$log_scale)) {
      # log-uniform: denser sampling at smaller values
      param_value <- exp(qunif(lhs_unit[, i], log(param_min), log(param_max)))
      if (param_name == "hold_samples_constant_at") {
        param_value <- round(param_value) # sample size must be an integer
      }
      param_draws[[param_name]] <- param_value
    } else {
      param_draws[[param_name]] <- qunif(lhs_unit[, i], param_min, param_max)
    }
  }
  param_draws
}

##############################################################################
#### PATHS ####
##############################################################################

# Output paths
analysis_dir <- here(
  "R",
  "manuscript_analyses",
  "output",
  "focal_parameter_sweep"
)
# LHS design: all param rows + global seeds
lhs_path <- file.path(analysis_dir, "lhs_design.rds")
# Batch log: append-only progress tracker
batch_log_path <- file.path(analysis_dir, "batch_log.txt")
# Combined results: once all batches are present
combined_path <- file.path(analysis_dir, "focal_sweep_combined.rds")

# Helper function to construct batch file paths
batch_path <- function(batch_id) {
  file.path(analysis_dir, sprintf("batch_%02d.rds", batch_id))
}

# Create the output directory if it doesn't exist
dir.create(analysis_dir, recursive = TRUE, showWarnings = FALSE)

##############################################################################
#### LHS DESIGN (write once, load thereafter) ####
##############################################################################

# If the LHS design file doesn't exist, draw a new one
if (!file.exists(lhs_path)) {
  message(
    "Drawing LHS design (",
    n_batches,
    " batches x ",
    n_sims_per_batch,
    ") ..."
  )

  # Draw LHS for each batch
  batch_dfs <- vector("list", n_batches)
  for (batch_id in seq_len(n_batches)) {
    # Independent LHS per batch; seed offsets keep batches reproducible
    set.seed(1000L + batch_id)
    # Draw LHS for each batch, set seeds and batch IDs
    batch_df <- draw_params(n_sims_per_batch)
    seed_start <- (batch_id - 1L) * n_sims_per_batch
    batch_df$seed <- seed_start + seq_len(n_sims_per_batch)
    batch_df$batch_id <- batch_id
    batch_dfs[[batch_id]] <- batch_df
  }

  # Combine the LHS designs for each batch into a single data frame and save
  lhs_design <- bind_rows(batch_dfs)
  saveRDS(lhs_design, lhs_path)
  message("Saved LHS design to ", lhs_path)
} else {
  # Otherwise, if the LHS design file exists, load it
  lhs_design <- readRDS(lhs_path)
  message("Loaded existing LHS design from ", lhs_path)
}

# Check that the LHS design has the expected number of rows
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

# Read the batch log and extract the completed batch numbers
completed_from_log <- integer(0)
if (file.exists(batch_log_path)) {
  log_lines <- readLines(batch_log_path, warn = FALSE)
  log_lines <- trimws(log_lines)
  log_lines <- log_lines[nzchar(log_lines)]
  if (length(log_lines) > 0) {
    completed_from_log <- as.integer(sub("^([0-9]+).*$", "\\1", log_lines))
  }
}

# List the batch files already present on disk and extract the completed batch numbers
batch_files_on_disk <- list.files(
  analysis_dir,
  pattern = "^batch_\\d{2}\\.rds$",
  full.names = FALSE
)
completed_from_disk <- as.integer(
  sub("^batch_(\\d{2})\\.rds$", "\\1", batch_files_on_disk)
)

completed_batches <- sort(unique(c(completed_from_log, completed_from_disk)))

# Backfill log for any existing batch files missing from the log
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

# Identify batches that are missing from the log
missing_batches <- setdiff(seq_len(n_batches), completed_batches)

# If all batches are complete, print a message
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
#### BATCH LOOP (one 04_run_sweep call per "missing" batch) ####
##############################################################################

for (batch_id in missing_batches) {
  # Print the batch number and the range of seeds for this batch
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

  # Subset LHS rows for this batch (04_run_sweep.R expects these objects in the env)
  sweep_params_full <- lhs_design[
    lhs_design$batch_id == batch_id,
    ,
    drop = FALSE
  ]
  n_sims <- n_sims_per_batch

  # Run parallel sims and save output/sweep_results_<timestamp>.rds
  # *This is the main function that runs the simulations*
  source(here("R", "04_run_sweep.R"), local = FALSE)

  # Move timestamped output to stable batch file in the analysis folder
  batch_file <- batch_path(batch_id)
  timestamped_output <- here(sweep_path)
  # file.rename() is what actually moves and renames the file
  if (!file.rename(timestamped_output, batch_file)) {
    stop("Failed to move ", timestamped_output, " to ", batch_file)
  }

  # Append batch number to log (append-only progress tracker)
  cat(batch_id, "\n", file = batch_log_path, append = TRUE)
  message("Batch ", batch_id, " saved to ", batch_file)
}

##############################################################################
#### COMBINE AT END (only when all batches present) ####
##############################################################################

# List all batch files and check that they all exist
all_batch_files <- batch_path(seq_len(n_batches))
all_batches_exist <- all(file.exists(all_batch_files))

# If not all batches exist, print a message
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
  # If the combined file already exists, print a message
  message("\nCombined file already exists: ", combined_path)
} else {
  # If all batches exist, combine them
  message("\nCombining ", n_batches, " batch files ...")

  # Read each batch file and extract the metadata
  batches <- lapply(all_batch_files, readRDS)
  combined_meta <- batches[[1]]$meta
  combined_meta$manuscript_analysis <- "focal_parameter_sweep"
  combined_meta$n_batches <- n_batches
  combined_meta$n_sims_per_batch <- n_sims_per_batch
  combined_meta$n_total_sims <- expected_n
  combined_meta$combined_at <- Sys.time()

  # Combine the results into a single list and save
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
