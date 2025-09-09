##############################################################################
# Run simulation
#
# Description: This file sources the necessary files and provides code to run
# academiABM2 simulations
##############################################################################

# Source model files
source("R/functions.R")
source("R/model.R")

# Load required packages
library(pryr) # For memory usage tracking
library(profvis) # For performance profiling

# Run simulation and track memory and runtime
profvis_profile <- profvis({
  results <<- run_simulation(
    n_agents = 10000,
    n_timesteps = 100,
    papers_per_agent_per_timestep = 2
  )
})

# Memory usage of results
cat("=== MEMORY USAGE ===\n")
cat("Papers matrix:", object_size(results), "bytes\n")

print(profvis_profile)
