##############################################################################
# Realistic condition Monte Carlo (for the manuscript)
#
# Description: Many stochastic replicates at a single realistic parameter
# setting (no parameter sweep). Varies only the random seed across runs.
#
# Output folder:
#   R/manuscript_analyses/output/realistic_condition_montecarlo/
#
# Files in that folder:
#   realistic_condition_montecarlo.rds — results; skipped if the file already exists
#
# Workflow summary:
#   1. If results file exists, skip (delete it to re-run)
#   2. Otherwise: source(04_run_sweep.R) → rename output to realistic_condition_montecarlo.rds
#   3. Summary statistics are printed by make_figures.R
##############################################################################

# Load packages
library(here)

# Source simulation code
function_files <- list.files(here("R", "functions"), full.names = TRUE)
sapply(function_files, source, .GlobalEnv)
source(here("R", "00_model.R"))

##############################################################################
#### FROZEN MANUSCRIPT CONFIG ####
##############################################################################

n_sims <- 1000L
n_cores <- parallel::detectCores() - 1
max_sweep_topups <- 3L # re-run missing seeds if any parallel jobs fail

# No parameters are swept here but 04_run_sweep.R still expects these objects in the env
sweep_param_names <- character(0)
param_config <- list()
sweep_params_full <- data.frame(seed = seq_len(n_sims))

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
  hold_samples_constant_at = 20,
  replications_dynamic_sample_sizes = 1,
  publication_bias = 1, # 0 = no publication bias, 1 = publication bias on
  sig_lower_asymptote = 0, # minimum publication probability for p < .05 results
  sig_logistic_midpoint = -0.5, # novelty midpoint for significant results
  sig_logistic_steepness = 3, # steepness of logistic curve for significant results
  nonsig_logistic_midpoint = 3, # novelty midpoint for non-significant results
  nonsig_logistic_steepness = 3, # steepness of logistic curve for non-significant results
  all_replications_published = 0,
  burn_in_period = 35,
  truth_contribution_method = "savage_dickey"
)

##############################################################################
#### PATHS ####
##############################################################################

# Output paths
analysis_dir <- here(
  "R",
  "manuscript_analyses",
  "output",
  "realistic_condition_montecarlo"
)
results_path <- file.path(analysis_dir, "realistic_condition_montecarlo.rds")

# Create the output directory if it doesn't exist
dir.create(analysis_dir, recursive = TRUE, showWarnings = FALSE)

##############################################################################
#### RUN ####
##############################################################################

# If the results file already exists, print a message
if (file.exists(results_path)) {
  message("Results already exist: ", results_path)
} else {
  # Run parallel sims and save output/sweep_results_<timestamp>.rds
  # *This is the main function that runs the simulations*
  source(here("R", "04_run_sweep.R"), local = FALSE)

  # Move timestamped output to a stable file in the analysis folder
  timestamped_output <- here(sweep_path)
  # file.rename() is what actually moves and renames the file
  if (!file.rename(timestamped_output, results_path)) {
    stop("Failed to move ", timestamped_output, " to ", results_path)
  }
  message("Saved results to ", results_path)
}
