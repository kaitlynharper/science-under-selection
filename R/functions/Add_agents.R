#### Function: add_agents ####

add_agents <- function(
  sim_env,
  n_agents,
  timestep_active,
  replication_probabilities,
  target_powers,
  timestep_next_papers
) {
  
  if (n_agents == 0) {
    warning("n_agents = 0; no agents to add. Skipping add_agents().")
    return()
  }

  # Generate new researcher IDs (max + 1)
  existing_ids <- sim_env$agents[, "researcher_id"]
  if (all(is.na(existing_ids))) {
    next_agent_id <- 1
  } else {
    next_agent_id <- max(existing_ids, na.rm = TRUE) + 1
  }
  researcher_ids <- next_agent_id:(next_agent_id + n_agents - 1)
  
  # Set timestep_inactive (NA for active agents)
  timesteps_inactive <- rep(NA, n_agents)
  
  # Set timestep_active (all agents have same entry timestep)
  timesteps_active <- rep(timestep_active, n_agents)
  
  # Grow if needed
  #Assess how many empty rows there are (rows where all columns are NA)
  empty_rows <- which(rowSums(!is.na(sim_env$agents)) == 0)
  n_empty_rows <- length(empty_rows)
  #Assess how many rows we want to add
  n_rows_wanted <- n_agents
  #If we want to add more than we have, add an empty block first (same formula as initialize_agents_matrix)
  if (n_rows_wanted > n_empty_rows) {
    block_size <- sim_env$n_agents * sim_env$n_timesteps
    new_block <- matrix(NA, nrow = block_size, ncol = ncol(sim_env$agents))
    colnames(new_block) <- colnames(sim_env$agents)
    sim_env$agents <- rbind(sim_env$agents, new_block)
    # Recompute empty rows after growing
    empty_rows <- which(rowSums(!is.na(sim_env$agents)) == 0)
  }
  #Find first empty row and fill
  start_index <- empty_rows[1]
  end_index <- start_index + n_agents - 1

  # Fill agents matrix with new agents
  sim_env$agents[start_index:end_index, ] <- cbind(
    researcher_ids,
    replication_probabilities,
    target_powers,
    timesteps_active,
    timesteps_inactive,
    timestep_next_papers
  )
}

