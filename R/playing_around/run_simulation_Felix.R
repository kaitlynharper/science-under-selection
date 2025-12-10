##############################################################################
# Run simulation
#
# Description: This file sources the necessary files and provides code to run
# academiABM2 simulations
##############################################################################

# Load required packages
library(here) # relative paths
library(pryr) # For memory usage tracking
library(profvis) # For performance profiling
library(tidyr)
library(dplyr)

# Source model files (incl all function files)
function_files <- list.files(here("R", "functions"), full.names = TRUE)
sapply(function_files, source, .GlobalEnv)
source(here("R", "model.R"))

# Set seed for reproducibility
# set.seed(123)

# Define simulation parameters
params <- list(
  # Parameters for agents and study design
  n_agents = 1000, # number of agents
  n_timesteps = 400, # number of timesteps
  n_timesteps_per_career_step = 10, # number of timesteps per career phase
  duration_per_observation = 0.1, # TODO calibration required # timesteps per observations
  duration_original_intercept = 1, # TODO calibration required # base timesteps for original studies

  # Parameters for true effects
  n_effects = 10000, # number of effects
  base_null_probability = .9, # base probability of a null effect
  effect_size_mean = .3, # mean effect size
  effect_size_variance = 0.1, # variance of effect sizes

  # Parameters for collective belief updating
  uninformed_prior_mean = 0, # mean of uninformed prior
  uninformed_prior_variance = 1, # variance of uninformed prior

  # Publication bias parameters # TODO calibration required
  sig_y_intercept = 0.5, # minimum publication probability for p < .05 results
  sig_logistic_midpoint = 0.5, # novelty midpoint for significant results
  sig_logistic_steepness = 3, # steepness of logistic curve for significant results
  nonsig_logistic_midpoint = 1.5, # novelty midpoint for non-significant results
  nonsig_logistic_steepness = 3, # steepness of logistic curve for non-significant results

  # Career turnover parameters
  initial_selection_condition = 0, # 0 = selection based on truth, 1 = selection based on novelty
  switch_conditions_at = NA, # if NA, no switch; if a value, condition switches at that timestep
  career_turnover_selection_rate = 0.5, # proportion of agents to retire each career step
  innovation_sd = 0.05, # standard deviation of innovation noise added to new agents
  hold_samples_constant_at = 30, # if NA, sample sizes are calculated; if a value, all studies use that sample size
  publication_bias = 1 #0 = no publication bias, 1 = publication bias
)

system.time({
  results <- run_simulation(params, verbose=2)}
)
