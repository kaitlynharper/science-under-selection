#### Function: add_agents ####

add_agents <- function(
  sim_env,
  n_agents,
  timestep_active,
  replication_probabilities,
  target_powers,
  timestep_next_papers
) {
  # Generate new researcher IDs
  researcher_ids <- sim_env$next_agent_id:(sim_env$next_agent_id + n_agents - 1)
  
  # Set timestep_inactive (NA for active agents)
  timesteps_inactive <- rep(NA, n_agents)
  
  # Set timestep_active (all agents have same entry timestep)
  timesteps_active <- rep(timestep_active, n_agents)
  
  # Calculate row indices to fill
  start_index <- sim_env$next_agent_index
  end_index <- sim_env$next_agent_index + n_agents - 1
  
  # Fill agents matrix with new agents
  sim_env$agents[start_index:end_index, ] <- cbind(
    researcher_ids,
    replication_probabilities,
    target_powers,
    timesteps_active,
    timesteps_inactive,
    timestep_next_papers
  )
  
  # Update trackers in sim_env
  sim_env$next_agent_index <- end_index + 1
  sim_env$next_agent_id <- sim_env$next_agent_id + n_agents
  
}

