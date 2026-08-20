##############################################################################
# Publication bias sweep (for the manuscript)
#
# Description: n_sims simulations with nonsig_logistic_midpoint drawn
# uniformly across its range (publication bias for non-significant results).
#
# Output folder:
#   R/manuscript_analyses/output/publication_bias_sweep/
#
# Files in that folder:
#   publication_bias_sweep.rds       — sweep results; skipped if already valid
#
# Workflow summary:
#   1. Check whether publication_bias_sweep.rds exists and has required columns
#   2. If not: draw LHS → source(04_run_sweep.R) → save to publication_bias_sweep.rds
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

n_sims <- 200L
n_cores <- parallel::detectCores() - 1
max_sweep_topups <- 3L

# Focal parameter that is swept (only publication bias is swept here)
sweep_param_names <- "nonsig_logistic_midpoint"

# Default parameters
base_params <- list(
  n_agents = 1000,
  n_timesteps = 100,
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
  career_turnover_selection_rate = 0,
  innovation_sd = 0,
  mutation_rate = 0,
  initial_replication_rate = 0.01,
  hold_samples_constant_at = 20,
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

# Focal parameter that is swept (only publication bias is swept here)
sweepable_params <- list(
  nonsig_logistic_midpoint = list(
    min = -0.5,
    max = 3,
    label = "Publication Bias",
    color = "#56B4E9"
  )
)

# Check that all focal parameters are in the sweepable_params list
stopifnot(all(sweep_param_names %in% names(sweepable_params)))
param_config <- sweepable_params[sweep_param_names]

# Helper function to draw parameter values
draw_params <- function(n = 1) {
  spec <- param_config[[1]]
  data.frame(
    nonsig_logistic_midpoint = qunif(randomLHS(n, 1), spec$min, spec$max)
  )
}

##############################################################################
#### PATHS ####
##############################################################################

# Path to the output directory
analysis_dir <- here(
  "R",
  "manuscript_analyses",
  "output",
  "publication_bias_sweep"
)
results_path <- file.path(analysis_dir, "publication_bias_sweep.rds")

# Create the output directory if it doesn't exist
dir.create(analysis_dir, recursive = TRUE, showWarnings = FALSE)

# Required outcome columns (special values needed for the publication bias calibration)
required_outcome_cols <- c(
  "pct_published_originals_sig",
  "pct_published_replications_sig",
  "pct_published_are_replications"
)

# Helper function to check if the results are valid
results_are_valid <- function(path) {
  # Check if the file exists
  if (!file.exists(path)) {
    return(FALSE)
  }
  # Read the results and make sure they are a data frame with the required columns
  output <- readRDS(path)
  res <- output$results
  is.data.frame(res) &&
    nrow(res) > 0L &&
    all(required_outcome_cols %in% names(res))
}

##############################################################################
#### RUN ####
##############################################################################

# Check if an existing results file exists and is valid and skip if it is
if (results_are_valid(results_path)) {
  message("Results already exist: ", results_path)
} else {
  if (file.exists(results_path)) {
    message(
      "Existing results are empty or missing required columns; re-running sweep."
    )
  }

  # Draw parameters
  sweep_params <- draw_params(n_sims)
  sweep_params$seed <- seq_len(n_sims)
  sweep_params_full <- sweep_params

  # Run the sweep
  # *This is the main function that runs the simulations*
  source(here("R", "04_run_sweep.R"), local = FALSE)

  # Read the timestamped output from 04_run_sweep.R
  timestamped_output <- here(sweep_path)
  sweep_output <- readRDS(timestamped_output)
  # Check that the results are not empty
  if (nrow(sweep_output$results) == 0L) {
    stop(
      "Sweep produced no results. All parallel jobs likely failed."
    )
  }
  # Check that the results have the required columns
  missing_cols <- setdiff(required_outcome_cols, names(sweep_output$results))
  if (length(missing_cols) > 0L) {
    stop(
      "Sweep results are missing columns: ",
      paste(missing_cols, collapse = ", "),
      "\nRe-source 04_run_sweep.R with the publication composition metrics."
    )
  }

  # Add metadata and save the results
  sweep_output$meta$manuscript_analysis <- "publication_bias_sweep"
  sweep_output$meta$n_sims <- n_sims
  saveRDS(sweep_output, results_path)
  # Remove the temporary file
  file.remove(timestamped_output)
  message("Saved results to ", results_path)
}
