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

  # Generate initial population of agents
  sim_env$current_agents <- generate_initial_agents(
    sim_env$n_agents
  )
  
  # Initialize trackers (TODO move to dynamic id/index allocation system)
  sim_env$next_study_id <- 1
  sim_env$next_agent_id <- sim_env$n_agents + 1
  sim_env$next_agent_index <- 1
  sim_env$timestep <- 0

  #### Timestep loop ####
  for (timestep in 0:sim_env$n_timesteps) {
    # Update timestep tracker in the environment
    sim_env$timestep <- timestep
    
    # Update timestep for current_agents
    sim_env$current_agents[, "timestep"] <- sim_env$timestep

    # Save current agent info for easy access later
    current_n_agents <- nrow(sim_env$current_agents)
    current_researcher_ids <- sim_env$current_agents[, "researcher_id"]
    
    # Run actual studies
    output <- run_studies(sim_env)

    # Career turnover phase
    if (sim_env$timestep %% sim_env$n_timesteps_per_career_step == 0) {
      # TODO Don't select on the first career phase studies, wait out the weird effects
      # Save current agents to the agents matrix
      sim_env$agents[
        sim_env$next_agent_index:(sim_env$next_agent_index + current_n_agents - 1),
      ] <- sim_env$current_agents
      # Update agents matrix index tracker
      sim_env$next_agent_index <- sim_env$next_agent_index + current_n_agents
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
