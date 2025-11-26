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
  n_agents = 10,
  n_timesteps = 10,
  n_timesteps_per_career_step = 10,
  n_effects = 50,
  base_null_probability = 0.1,
  effect_size_mean = 0.3,
  effect_size_variance = 0.1,
  uninformed_prior_mean = 0,
  uninformed_prior_variance = 1,
  mean_studies_per_agent_per_timestep = 2, # TODO remove once real studies are implemented
  duration_per_observation = 0.1, # TODO calibration required
  duration_original_intercept = 1 # TODO calibration required
)

# Run simulation and track memory and runtime
#profvis_profile <- profvis({
  results <- run_simulation(params)
#})

# Memory usage of results
cat("=== MEMORY USAGE ===\n")
cat("Studies matrix:", object_size(results$studies), "bytes\n")
cat("Agents matrix:", object_size(results$agents), "bytes\n")
# Display model output
View(results$agents)
View(results$studies)
View(results$effects)
# Open profvis profile
print(profvis_profile)

# Positron debugging option
# debugonce(run_simulation)
