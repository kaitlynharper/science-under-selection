##############################################################################
# Model
#
# Description: This script establishes the run_simulation function
##############################################################################

# Simulation function
run_simulation <- function(params) {
  # Create simulation environment
  sim_env <- new.env()
  
  # Store parameters in environment
  for (param_name in names(params)) {
    sim_env[[param_name]] <- params[[param_name]]
  }
  
  #### Initialize model ####

  # Initialize effects matrix
  sim_env$effects <- initialize_effects_matrix(
    sim_env$n_effects,
    sim_env$base_null_probability,
    sim_env$effect_size_mean,
    sim_env$effect_size_variance,
    sim_env$uninformed_prior_mean,
    sim_env$uninformed_prior_variance
  )

  # Initialize empty studies matrix
  sim_env$studies <- initialize_studies_matrix(
    sim_env$n_agents,
    sim_env$n_timesteps,
    sim_env$mean_studies_per_agent_per_timestep
  )

  # Initialize empty agents matrix
  sim_env$agents <- initialize_agents_matrix(
    sim_env$n_agents,
    sim_env$n_timesteps,
    sim_env$n_timesteps_per_career_step
  )

  # Initialize trackers
  sim_env$next_study_id <- 1
  sim_env$next_agent_id <- 1
  sim_env$next_agent_index <- 1
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
    sim_env$timestep <- timestep
    
    # Run actual studies
    output <- run_studies(sim_env)

    # Career turnover phase
    if (sim_env$timestep %% sim_env$n_timesteps_per_career_step == 0) {
      # TODO Don't select on the first career phase studies, wait out the weird effects
      # Retire set % of agents based on novelty or truth contributions
      # Generate new agents to fill lowest level (sample existing trait values + noise)
      # ^ Remember to update next_agent_id when doing this
    }
  }

  # Return list of results (eventually will also output effects matrix)
  return(list(
    studies = sim_env$studies,
    agents = sim_env$agents,
    effects = sim_env$effects
  ))
}
