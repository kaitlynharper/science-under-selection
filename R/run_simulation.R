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

# Define simulation parameters
params <- list(
  n_agents = 1000, # number of agents
  n_timesteps = 500, # number of timesteps
  n_timesteps_per_career_step = 10, # number of timesteps per career phase
  n_effects = 20000, # number of effects
  base_null_probability = 0.9, # base probability of a null effect
  effect_size_mean = 0.3, # mean effect size
  effect_size_variance = 0.1, # variance of effect sizes
  uninformed_prior_mean = 0, # mean of uninformed prior
  uninformed_prior_variance = 1, # variance of uninformed prior
  duration_per_observation = 0.1, # TODO calibration required # timesteps per observations
  duration_original_intercept = 1, # TODO calibration required # base timesteps for original studies
  # Publication bias parameters # TODO calibration required
  sig_y_intercept = 0.2, # minimum publication probability for p < .05 results
  sig_logistic_midpoint = .5, # novelty midpoint for significant results
  sig_logistic_steepness = 3, # steepness of logistic curve for significant results
  nonsig_logistic_midpoint = .5, # novelty midpoint for non-significant results
  nonsig_logistic_steepness = 3, # steepness of logistic curve for non-significant results
  # Career turnover parameters
  selection_condition = 0, # 0 = selection based on truth, 1 = selection based on novelty
  career_turnover_selection_rate = 0.5, # proportion of agents to retire each career step
  innovation_sd = 0.05, # standard deviation of innovation noise added to new agents
  hold_samples_constant_at = 30, # if NA, sample sizes are calculated; if a value, all studies use that sample size
  publication_bias = 0 #0 = no publication bias, 1 = publication bias
)

# Run simulation and track memory and runtime
# profvis_profile <- profvis({
results <- run_simulation(params)
# })

# Memory usage of results
# cat("=== MEMORY USAGE ===\n")
# cat("Studies matrix:", object_size(results$studies), "bytes\n")
# cat("Agents matrix:", object_size(results$agents), "bytes\n")
# cat("Effects matrix:", object_size(results$effects), "bytes\n")
# Display model output
# View(results$agents)
# View(results$studies)
# View(results$effects)
# Open profvis profile
# print(profvis_profile)
