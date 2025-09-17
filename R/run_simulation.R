##############################################################################
# Run simulation
#
# Description: This file sources the necessary files and provides code to run
# academiABM2 simulations
##############################################################################

# Source model files
function_files = list.files("R/functions/", full.names = TRUE)
sapply(function_files, source, .GlobalEnv)
source("R/model.R")

# Load required packages
library(pryr) # For memory usage tracking
library(profvis) # For performance profiling

# Run simulation and track memory and runtime
profvis_profile <- profvis({
  results <<- run_simulation(
    n_agents = 10000,
    n_timesteps = 100,
    n_timesteps_per_career_step = 10,
    mean_papers_per_agent_per_timestep = 2
  )
})

# Memory usage of results
cat("=== MEMORY USAGE ===\n")
cat("Papers matrix:", object_size(results$papers), "bytes\n")
cat("Agents matrix:", object_size(results$agents), "bytes\n")

# Open profvis profile
print(profvis_profile)

# Debugging option
# debugonce(run_simulation)
