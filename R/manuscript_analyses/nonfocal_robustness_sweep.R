##############################################################################
# Non-focal robustness sweep (for the manuscript)
#
# Description: Same focal LHS as focal_parameter_sweep.R (batch 1 only), but
# with one non-focal parameter deviation per scenario. Baseline results are
# in focal_parameter_sweep/ (not rerun here).
#
# Prerequisite: run focal_parameter_sweep.R first (creates lhs_design.rds).
# Baseline results are in focal_parameter_sweep/ (not rerun here).
# Note: n_sims_per_batch must match focal_parameter_sweep.R
#
# Output folder:
#   R/manuscript_analyses/output/nonfocal_robustness/
#
# Files in that folder:
#   scenario_log.txt                 — append-only log of completed scenario ids
#   batch_<scenario_id>.rds          — one per completed scenario (same format as focal batches)
#   nonfocal_combined.rds            — combined once all scenario batches are present
#
# Workflow summary:
#   1. Load shared LHS from focal_parameter_sweep (run that script first)
#   2. Read scenario_log (+ reconcile with batch_*.rds on disk) to find missing scenarios
#   3. For each missing scenario: apply overrides → source(04_run_sweep.R) → rename output → append log
#   4. When all scenarios done: combine batch files → nonfocal_combined.rds
##############################################################################

# Load packages
library(here)
library(dplyr)

# Source simulation code
function_files <- list.files(here("R", "functions"), full.names = TRUE)
sapply(function_files, source, .GlobalEnv)
source(here("R", "00_model.R"))

##############################################################################
#### FROZEN MANUSCRIPT CONFIG ####
##############################################################################

n_sims_per_batch <- 2000L #make sure it matches LHS_design from focal_parameter_sweep
focal_lhs_batch <- 1L # which batch from focal_parameter_sweep lhs_design.rds
n_cores <- parallel::detectCores() - 1
max_sweep_topups <- 3L

# Focal parameters that are swept
sweep_param_names <- c(
  "hold_samples_constant_at",
  "nonsig_logistic_midpoint",
  "base_null_probability"
)

# Default parameters
frozen_base_params <- list(
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

##############################################################################
#### PATHS ####
##############################################################################

# Path to the shared LHS design from focal_parameter_sweep
focal_lhs_path <- here(
  "R",
  "manuscript_analyses",
  "output",
  "focal_parameter_sweep",
  "lhs_design.rds"
)
# Path to the output directory
output_dir <- here(
  "R",
  "manuscript_analyses",
  "output",
  "nonfocal_robustness"
)
# Path to the scenario log
scenario_log_path <- file.path(output_dir, "scenario_log.txt")
# Path to the combined results once all scenario batches are present
combined_path <- file.path(output_dir, "nonfocal_combined.rds")

# Helper function to construct batch file paths
batch_path <- function(scenario_id) {
  file.path(output_dir, paste0("batch_", scenario_id, ".rds"))
}

# Create the output directory if it doesn't exist
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Check the shared LHS design exists and throw an error if it doesn't
if (!file.exists(focal_lhs_path)) {
  stop(
    "Shared LHS not found: ",
    focal_lhs_path,
    "\nRun R/manuscript_analyses/focal_parameter_sweep.R first."
  )
}

# Read the shared LHS design and subset it to the focal parameters and seeds
lhs_design <- readRDS(focal_lhs_path)
sweep_params_full <- lhs_design[
  lhs_design$batch_id == focal_lhs_batch,
  ,
  drop = FALSE
]
# Check that the number of rows matches the number of simulations per batch
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
# Set the number of simulations to the number of simulations per batch
n_sims <- n_sims_per_batch
# Print a message with the number of rows in the shared LHS design
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

# List of scenarios to sweep
scenarios <- list(
  optimistic_prior = list(
    label = "Optimistic prior",
    overrides = list(
      uninformed_prior_mean = 0.3,
      uninformed_prior_variance = 0.25
    )
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
      n_timesteps_per_career_step = 70L,
      n_timesteps = 2000L,
      burn_in_period = 70L
    )
  )
)

##############################################################################
#### PROGRESS: scenario_log + reconciliation with batch files on disk ####
##############################################################################

# Read the scenario log and extract the completed scenario ids
completed_from_log <- character(0)
if (file.exists(scenario_log_path)) {
  log_lines <- readLines(scenario_log_path, warn = FALSE)
  log_lines <- trimws(log_lines)
  completed_from_log <- log_lines[nzchar(log_lines)]
}

# List the batch files already present on disk and extract the completed scenario ids
batch_files_on_disk <- list.files(
  output_dir,
  pattern = "^batch_.+\\.rds$",
  full.names = FALSE
)
completed_from_disk <- sub("^batch_(.+)\\.rds$", "\\1", batch_files_on_disk)

completed_scenarios <- sort(unique(c(completed_from_log, completed_from_disk)))
completed_scenarios <- intersect(completed_scenarios, names(scenarios))

# Backfill log for any existing batch files missing from the log
not_in_log <- setdiff(completed_from_disk, completed_from_log)
not_in_log <- intersect(not_in_log, names(scenarios))
if (length(not_in_log) > 0) {
  message(
    "Backfilling scenario_log for scenario(s): ",
    paste(not_in_log, collapse = ", ")
  )
  cat(
    paste(not_in_log, collapse = "\n"),
    "\n",
    file = scenario_log_path,
    append = TRUE
  )
}

# Identify scenarios that are missing from the log
missing_scenarios <- setdiff(names(scenarios), completed_scenarios)

# If some scenarios are already complete, print a message
if (length(completed_scenarios) > 0) {
  message(
    "Scenarios already complete (",
    length(completed_scenarios),
    "): ",
    paste(completed_scenarios, collapse = ", ")
  )
}

# If all scenarios are complete, print a message
if (length(missing_scenarios) == 0) {
  message("All ", length(scenarios), " scenarios already complete.")
} else {
  # Otherwise print the scenarios that are missing and will be run
  message(
    "Scenarios to run: ",
    length(missing_scenarios),
    " (",
    paste(missing_scenarios, collapse = ", "),
    ")"
  )
}

##############################################################################
#### SCENARIO LOOP (one 04_run_sweep call per "missing" scenario) ####
##############################################################################

for (scenario_id in missing_scenarios) {
  # Print the scenario id
  scenario <- scenarios[[scenario_id]]
  message("\n========== Scenario: ", scenario_id, " ==========")

  # Apply this scenario's overrides on top of frozen base params
  # (04_run_sweep.R expects base_params in the env)
  base_params <- modifyList(frozen_base_params, scenario$overrides)

  # Run parallel sims and save output/sweep_results_<timestamp>.rds
  # *This is the main function that runs the simulations*
  source(here("R", "04_run_sweep.R"), local = FALSE)

  # Move timestamped output to stable batch file in the analysis folder
  batch_file <- batch_path(scenario_id)
  timestamped_output <- here(sweep_path)
  # file.rename() is what actually moves and renames the file
  if (!file.rename(timestamped_output, batch_file)) {
    stop("Failed to move ", timestamped_output, " to ", batch_file)
  }

  # Tag output with scenario metadata (same object shape as a focal batch file)
  batch_output <- readRDS(batch_file)
  batch_output$meta$manuscript_analysis <- "nonfocal_robustness"
  batch_output$meta$scenario_id <- scenario_id
  batch_output$meta$scenario_label <- scenario$label
  batch_output$meta$scenario_overrides <- scenario$overrides
  batch_output$meta$lhs_source <- focal_lhs_path
  batch_output$meta$lhs_subset <- paste0("batch_id == ", focal_lhs_batch)
  batch_output$meta$focal_lhs_batch <- focal_lhs_batch
  batch_output$meta$n_sims_per_batch <- n_sims_per_batch
  batch_output$meta$base_params <- base_params
  saveRDS(batch_output, batch_file)

  # Append scenario id to log (append-only progress tracker)
  cat(scenario_id, "\n", file = scenario_log_path, append = TRUE)
  message("Scenario ", scenario_id, " saved to ", batch_file)
}

##############################################################################
#### COMBINE AT END (only when all scenario batches present) ####
##############################################################################

# List all batch files and check that they all exist
all_batch_files <- batch_path(names(scenarios))
all_scenarios_exist <- all(file.exists(all_batch_files))

# If not all scenarios exist, print a message
if (!all_scenarios_exist) {
  n_done <- sum(file.exists(all_batch_files))
  message(
    "\nCombine skipped: ",
    n_done,
    " / ",
    length(scenarios),
    " scenario batch files present. Re-run this script to continue."
  )
} else if (file.exists(combined_path)) {
  # If the combined file already exists, print a message
  message("\nCombined file already exists: ", combined_path)
} else {
  # If all scenarios exist, combine them
  message("\nCombining ", length(scenarios), " scenario batch files ...")

  # Read each batch file, tag rows with scenario_id, and extract the metadata
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

  # Combine the results into a single list and save
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
