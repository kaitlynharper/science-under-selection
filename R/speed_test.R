##############################################################################
# Speed test
#
# Description: Runs a single simulation under profvis to profile runtime.
##############################################################################

# ---- Load packages ----
library(here)
library(dplyr)
library(lhs)

# ---- Source simulation code ----
# This repo has no .Rproj; anchor here() from this script's path (RStudio + Positron).
# Run via Source / Run script — not by pasting chunks into the console.
here::i_am("R/speed_test.R")

sapply(list.files(here("R", "functions"), full.names = TRUE), source, .GlobalEnv)
source(here("R", "00_model.R"))

# ---- Set params ----
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

# ---- Run and track ----
profvis_profile <- profvis::profvis({
  run_simulation(base_params, verbose = 1)
})

profvis_profile
