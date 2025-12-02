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
  # Publication bias parameters
  sig_y_intercept = 0.5, # minimum publication probability for p < .05 results
  sig_logistic_midpoint = 1, # novelty midpoint for significant results
  sig_logistic_steepness = 5, # steepness of logistic curve for significant results
  nonsig_logistic_midpoint = 1, # novelty midpoint for non-significant results
  nonsig_logistic_steepness = 5, # steepness of logistic curve for non-significant results
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
