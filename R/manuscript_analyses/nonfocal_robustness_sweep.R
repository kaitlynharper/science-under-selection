##############################################################################
# Non-focal robustness sweep (manuscript)
#
# Description: Same focal LHS as focal_parameter_sweep.R (batch 1 only), but
# with one non-focal parameter deviation per scenario.
#
# Prerequisite: run focal_parameter_sweep.R first (creates lhs_design.rds).
# Baseline results are in focal_parameter_sweep/ (not rerun here).
#
# Output folder:
#   R/manuscript_analyses/output/nonfocal_robustness/
#
# Files:
#   scenario_log.txt
#   batch_<scenario_id>.rds          — one per scenario (same format as focal batches)
#   nonfocal_combined.rds             — all scenarios, written when every batch exists
#
# Re-run safe: skips scenarios in scenario_log / on disk; combines when all done.
##############################################################################

library(here)
library(dplyr)

# ---- Source simulation code ----
function_files <- list.files(here("R", "functions"), full.names = TRUE)
sapply(function_files, source, .GlobalEnv)
source(here("R", "model.R"))

##############################################################################
#### FROZEN MANUSCRIPT CONFIG ####
# Same as focal_parameter_sweep.R; scenario overrides applied in the loop.
##############################################################################

n_sims_per_batch <- 200L #make sure it matches LHS_design from focal_parameter_sweep
focal_lhs_batch <- 1L # which batch from focal_parameter_sweep lhs_design.rds
n_cores <- parallel::detectCores() - 1
max_sweep_topups <- 3L

sweep_param_names <- c(
  "hold_samples_constant_at",
  "nonsig_logistic_midpoint",
  "base_null_probability"
)

frozen_base_params <- list(
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

##############################################################################
#### PATHS ####
##############################################################################

focal_lhs_path <- here(
  "R",
  "manuscript_analyses",
  "output",
  "focal_parameter_sweep",
  "lhs_design.rds"
)
output_dir <- here(
  "R",
  "manuscript_analyses",
  "output",
  "nonfocal_robustness"
)
scenario_log_path <- file.path(output_dir, "scenario_log.txt")
combined_path <- file.path(output_dir, "nonfocal_combined.rds")

batch_path <- function(scenario_id) {
  file.path(output_dir, paste0("batch_", scenario_id, ".rds"))
}

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(focal_lhs_path)) {
  stop(
    "Shared LHS not found: ",
    focal_lhs_path,
    "\nRun R/manuscript_analyses/focal_parameter_sweep.R first."
  )
}

# subset shared LHS; same focal param rows and seeds across all scenarios
lhs_design <- readRDS(focal_lhs_path)
sweep_params_full <- lhs_design[
  lhs_design$batch_id == focal_lhs_batch,
  ,
  drop = FALSE
]
if (nrow(sweep_params_full) != n_sims_per_batch) {
  stop(
    "LHS batch ",
    focal_lhs_batch,
    " has ",
    nrow(sweep_params_full),
    " rows; expected ",
    n_sims_per_batch,
    "."
  )
}
n_sims <- n_sims_per_batch
message(
  "Loaded shared LHS from focal sweep (batch ",
  focal_lhs_batch,
  ", ",
  nrow(sweep_params_full),
  " rows)"
)

##############################################################################
#### SCENARIOS ####
##############################################################################

scenarios <- list(
  optimistic_prior = list(
    label = "Optimistic prior",
    overrides = list(uninformed_prior_mean = 0.5)
  ),
  tight_prior = list(
    label = "Tight prior",
    overrides = list(uninformed_prior_variance = 0.25)
  ),
  large_effects = list(
    label = "Large effects",
    overrides = list(effect_size_mean = 0.8)
  ),
  all_reps_published = list(
    label = "All replications published",
    overrides = list(all_replications_published = 1)
  ),
  strong_selection = list(
    label = "Strong selection",
    overrides = list(career_turnover_selection_rate = 0.8)
  ),
  slow_originals = list(
    label = "Slow originals",
    overrides = list(duration_original_intercept = 5)
  ),
  long_career_window = list(
    label = "Long career window",
    overrides = list(
      n_timesteps_per_career_step = 100L,
      n_timesteps = 1000L,
      burn_in_period = 100L
    )
  )
)

##############################################################################
#### PROGRESS: scenario_log + batch files on disk ####
##############################################################################

completed_from_log <- character(0)
if (file.exists(scenario_log_path)) {
  log_lines <- readLines(scenario_log_path, warn = FALSE)
  log_lines <- trimws(log_lines)
  completed_from_log <- log_lines[nzchar(log_lines)]
}

batch_files_on_disk <- list.files(
  output_dir,
  pattern = "^batch_.+\\.rds$",
  full.names = FALSE
)
completed_from_disk <- sub("^batch_(.+)\\.rds$", "\\1", batch_files_on_disk)

completed_scenarios <- sort(unique(c(completed_from_log, completed_from_disk)))
completed_scenarios <- intersect(completed_scenarios, names(scenarios))

not_in_log <- setdiff(completed_from_disk, completed_from_log)
not_in_log <- intersect(not_in_log, names(scenarios))
if (length(not_in_log) > 0) {
  cat(
    paste(not_in_log, collapse = "\n"),
    "\n",
    file = scenario_log_path,
    append = TRUE
  )
}

missing_scenarios <- setdiff(names(scenarios), completed_scenarios)

if (length(completed_scenarios) > 0L) {
  message(
    "Scenarios already complete (",
    length(completed_scenarios),
    "): ",
    paste(completed_scenarios, collapse = ", ")
  )
}

if (length(missing_scenarios) == 0L) {
  message("All ", length(scenarios), " scenarios already complete.")
} else {
  message(
    "Scenarios to run: ",
    length(missing_scenarios),
    " (",
    paste(missing_scenarios, collapse = ", "),
    ")"
  )
}

##############################################################################
#### SCENARIO LOOP (one run per scenario) ####
##############################################################################

for (scenario_id in missing_scenarios) {
  scenario <- scenarios[[scenario_id]]
  message("\n========== Scenario: ", scenario_id, " ==========")

  base_params <- modifyList(frozen_base_params, scenario$overrides)

  source(here("R", "run_sweep.R"), local = FALSE)

  dest <- batch_path(scenario_id)
  src <- here(sweep_path)
  if (!file.rename(src, dest)) {
    stop("Failed to move ", src, " to ", dest)
  }

  # tag output with scenario metadata (same object shape as a focal batch file)
  batch_output <- readRDS(dest)
  batch_output$meta$manuscript_analysis <- "nonfocal_robustness"
  batch_output$meta$scenario_id <- scenario_id
  batch_output$meta$scenario_label <- scenario$label
  batch_output$meta$scenario_overrides <- scenario$overrides
  batch_output$meta$lhs_source <- focal_lhs_path
  batch_output$meta$lhs_subset <- paste0("batch_id == ", focal_lhs_batch)
  batch_output$meta$focal_lhs_batch <- focal_lhs_batch
  batch_output$meta$n_sims_per_batch <- n_sims_per_batch
  batch_output$meta$base_params <- base_params
  saveRDS(batch_output, dest)

  cat(scenario_id, "\n", file = scenario_log_path, append = TRUE)
  message("Saved to ", dest)
}

##############################################################################
#### COMBINE (when every scenario batch is present) ####
##############################################################################

all_batch_files <- batch_path(names(scenarios))
all_scenarios_exist <- all(file.exists(all_batch_files))

if (!all_scenarios_exist) {
  n_done <- sum(file.exists(all_batch_files))
  message(
    "\nCombine skipped: ",
    n_done,
    " / ",
    length(scenarios),
    " scenario batch files present."
  )
} else if (file.exists(combined_path)) {
  message("\nCombined file already exists: ", combined_path)
} else {
  message("\nCombining ", length(scenarios), " scenario batch files ...")

  batches <- lapply(names(scenarios), function(id) {
    batch_output <- readRDS(batch_path(id))
    batch_output$results$scenario_id <- id
    batch_output
  })

  combined_meta <- batches[[1]]$meta
  combined_meta$manuscript_analysis <- "nonfocal_robustness"
  combined_meta$scenarios <- lapply(scenarios, function(s) {
    list(label = s$label, overrides = s$overrides)
  })
  combined_meta$lhs_source <- focal_lhs_path
  combined_meta$lhs_subset <- paste0("batch_id == ", focal_lhs_batch)
  combined_meta$focal_lhs_batch <- focal_lhs_batch
  combined_meta$n_sims_per_batch <- n_sims_per_batch
  combined_meta$n_scenarios <- length(scenarios)
  combined_meta$n_total_sims <- length(scenarios) * n_sims_per_batch
  combined_meta$combined_at <- Sys.time()

  nonfocal_combined <- list(
    meta = combined_meta,
    results = bind_rows(lapply(batches, `[[`, "results"))
  )

  saveRDS(nonfocal_combined, combined_path)
  message(
    "Saved combined results (",
    nrow(nonfocal_combined$results),
    " rows) to ",
    combined_path
  )
}
