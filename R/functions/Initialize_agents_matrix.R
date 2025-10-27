#### Function: initialize_agents_matrix ####

initialize_agents_matrix <- function(
  n_agents,
  n_timesteps,
  n_timesteps_per_career_step
) {
  #Initialize matrix of agents
  # Calculate total number of agents
  total_agents <- n_agents *
    (floor(n_timesteps / n_timesteps_per_career_step) + 1)
  agents <- matrix(0, nrow = total_agents, ncol = 5)
  # Set column names
  colnames(agents) <- c(
    "researcher_id",
    "timestep",
    "replication_probability",
    "target_power",
    "career_status"
  )
  # Track current agent
  next_agent_ID <<- 1
  next_agent_index <<- 1
  return(agents)
}
