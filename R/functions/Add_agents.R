##########################################################################
# Add agents
##########################################################################

#### Function: add_agents ####

add_agents <- function(
  sim_env,
  n_agents,
  timestep_active,
  replication_probabilities,
  target_powers,
  timestep_next_papers
) {
  
  # Check if no agents to add
  if (n_agents == 0) {
    warning("n_agents = 0; no agents to add. Skipping add_agents().")
    return()
  }

  # Generate new researcher IDs (max + 1)
  existing_ids <- sim_env$agents[, "researcher_id"]
  if (length(existing_ids) == 0) {
    next_agent_id <- 1
  } else {
    next_agent_id <- max(existing_ids, na.rm = TRUE) + 1
  }
  researcher_ids <- next_agent_id:(next_agent_id + n_agents - 1)
  
  # Set timestep_active
  timesteps_active <- rep(timestep_active, n_agents)

    # Set timestep_inactive (NA for new active agents)
  timesteps_inactive <- rep(NA, n_agents)

  # Create new agents matrix
  new_agents <- cbind(
    researcher_id = researcher_ids,
    replication_probability = replication_probabilities,
    target_power = target_powers,
    timestep_active = timesteps_active,
    timestep_inactive = timesteps_inactive,
    timestep_next_paper = timestep_next_papers
  )
  # Add new agents to the existingagents matrix
  sim_env$agents <- rbind(sim_env$agents, new_agents)
}
