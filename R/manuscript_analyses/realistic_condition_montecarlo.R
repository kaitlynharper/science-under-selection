##############################################################################
# Realistic condition Monte Carlo (manuscript)
#
# Description: Many stochastic replicates at a single realistic parameter
# setting (no parameter sweep). Varies only the random seed across runs.
#
# If a results file already exists, this script will only present the results.
# Old results file must be deleted to run a new set of simulations.
# Summary statistics are printed by make_figures.R.
#
# Output folder:
#   R/manuscript_analyses/output/realistic_condition_montecarlo/
#
# Files in that folder:
#   realistic_condition_montecarlo.rds
##############################################################################

library(here)

# Source simulation code once before parallel execution
function_files <- list.files(here("R", "functions"), full.names = TRUE)
sapply(function_files, source, .GlobalEnv)
source(here("R", "model.R"))

##############################################################################
#### FROZEN MANUSCRIPT CONFIG ####
##############################################################################

n_sims <- 1000L
n_cores <- parallel::detectCores() - 1
max_sweep_topups <- 3L

sweep_param_names <- character(0)
param_config <- list()
sweep_params_full <- data.frame(seed = seq_len(n_sims))

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
  truth_contribution_method = "kl"
)

##############################################################################
#### PATHS ####
##############################################################################

analysis_dir <- here(
  "R",
  "manuscript_analyses",
  "output",
  "realistic_condition_montecarlo"
)
results_path <- file.path(analysis_dir, "realistic_condition_montecarlo.rds")

if (!dir.exists(analysis_dir)) {
  stop(
    "Output folder not found: ",
    analysis_dir,
    "\nCreate it manually before running this script."
  )
}

##############################################################################
#### RUN ####
##############################################################################

if (file.exists(results_path)) {
  message("Results already exist: ", results_path)
} else {
  source(here("R", "run_sweep.R"), local = FALSE)

  src <- here(sweep_path)
  if (!file.rename(src, results_path)) {
    stop("Failed to move ", src, " to ", results_path)
  }
  message("Saved results to ", results_path)
}
