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
  agents <- matrix(0, nrow = total_agents, ncol = 6)
  # Set column names
  colnames(agents) <- c(
    "researcher_ID",
    "timestep",
    "prob_replicate",
    "career_level",
    "timesteps_in_career_level",
    "total_papers"
  )
  # Track current agent
  next_agent_ID <<- 1
  next_agent_index <<- 1
  return(agents)
}
