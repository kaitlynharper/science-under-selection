#### Function: generate_initial_agents ####

generate_initial_agents <- function(n_agents) {
  # Set up initial current_agents
  current_agents <- matrix(0, nrow = n_agents, ncol = 5)
  # Set column names
  colnames(current_agents) <- c(
    "researcher_id",
    "timestep",
    "replication_probability",
    "target_power",
    "career_status"
  )
  # Generate IDs
  researcher_ids <- next_agent_ID:(next_agent_ID + n_agents - 1)
  # Set timestep
  timesteps <- rep(0, n_agents)
  # Generate replication_probability (between 0 and 1)
  replication_probabilities <- pmax(0, pmin(1, rnorm(n_agents, 0.5, 0.5)))
  # Generate target_power (between 0 and 1)
  target_powers <- pmax(0, pmin(1, rnorm(n_agents, 0.8, 0.1)))
  # Initialize career_status (0 = active, 1 = inactive)
  career_statuses <- rep(0, n_agents)

  # Fill agents matrix with these values
  # Add to matrix
  current_agents[
    next_agent_index:(next_agent_index +
      (length(researcher_ids)) -
      1),
  ] <-
    cbind(
      researcher_ids,
      timesteps,
      replication_probabilities,
      target_powers,
      career_statuses
    )
  # Update next agent ID
  next_agent_ID <<- next_agent_ID + length(researcher_ids)
  return(current_agents)
}
