##############################################################################
# Model
#
# Description: This script establishes the run_simulation function
##############################################################################

# Simulation function
run_simulation <- function(params) {
  # Create simulation environment
  sim_env <- new.env()
  
  # TODO consider keeping the params bundled for easy export
  # Store parameters in environment
  for (param_name in names(params)) {
    sim_env[[param_name]] <- params[[param_name]]
  }
  
  #### Initialize model ####

  # Initialize effects matrix
  initialize_effects_matrix(sim_env)

  # Initialize empty studies matrix
  initialize_studies_matrix(sim_env)

  # Initialize empty agents matrix
  initialize_agents_matrix(sim_env)

  # Initialize timestep tracker
  sim_env$timestep <- 0

  # Generate initial population of agents
  add_agents(
    sim_env = sim_env,
    n_agents = sim_env$n_agents,
    timestep_active = 0,
    replication_probabilities = runif(sim_env$n_agents, 0, 1),
    target_powers = runif(sim_env$n_agents, 0, 1),
    timestep_next_papers = rep(0, sim_env$n_agents)
  )

  #### Timestep loop ####
  for (timestep in 0:sim_env$n_timesteps) {
    
    # Update timestep tracker in the environment
    # (R doesn't like to use an environment variable as a loop index variable)
    sim_env$timestep <- timestep
    
    # Run actual studies
    run_studies(sim_env)

    # Career turnover phase (skip first career step)
    if (sim_env$timestep %% sim_env$n_timesteps_per_career_step == 0 &&
        sim_env$timestep > 0) {
      career_turnover(sim_env)
    }
  }

  # Return the env
  return(sim_env)
}
