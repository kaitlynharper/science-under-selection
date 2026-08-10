##############################################################################
# Realistic condition single simulation (manuscript)
#
# Description: One full simulation at the frozen realistic manuscript
# parameters (same setting as realistic_condition_montecarlo.R). Uses seed 1,
# matching the first replicate in the Monte Carlo run. Saves the full
# timestep-by-timestep model output for figure scripts.
#
# If a results file already exists, this script will only report that fact.
# Delete the file to re-run.
#
# Output folder:
#   R/manuscript_analyses/output/realistic_condition_single_simulation/
#
# Files in that folder:
#   realistic_condition_single_simulation.rds
##############################################################################

library(here)
library(dplyr)

function_files <- list.files(here("R", "functions"), full.names = TRUE)
sapply(function_files, source, .GlobalEnv)
source(here("R", "model.R"))

##############################################################################
#### FROZEN MANUSCRIPT CONFIG (matches realistic_condition_montecarlo.R) ####
##############################################################################

sim_seed <- 1L

base_params <- list(
  n_agents = 1000,
  n_timesteps = 2000,
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
  hold_samples_constant_at = 30,
  replications_dynamic_sample_sizes = 1,
  publication_bias = 1, # 0 = no publication bias, 1 = publication bias on
  sig_lower_asymptote = 0, # minimum publication probability for p < .05 results
  sig_logistic_midpoint = -0.5, # novelty midpoint for significant results
  sig_logistic_steepness = 3, # steepness of logistic curve for significant results
  nonsig_logistic_midpoint = 2, # novelty midpoint for non-significant results
  nonsig_logistic_steepness = 3, # steepness of logistic curve for non-significant results
  all_replications_published = 0, # 0 = normal publication bias, 1 = all replications published
  burn_in_period = 300,
  truth_contribution_method = "savage_dickey"
)

##############################################################################
#### PATHS ####
##############################################################################

analysis_dir <- here(
  "R",
  "manuscript_analyses",
  "output",
  "realistic_condition_single_simulation"
)
results_path <- file.path(
  analysis_dir,
  "realistic_condition_single_simulation.rds"
)

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
  set.seed(sim_seed)
  results <- run_simulation(base_params, verbose = 0)

  sim_output <- list(
    meta = list(
      base_params = base_params,
      seed = sim_seed,
      timestamp = Sys.time()
    ),
    results = results
  )
  saveRDS(sim_output, results_path)
  message("Saved results to ", results_path)
}
