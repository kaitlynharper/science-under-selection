#### Function: initialize_agents_matrix ####

initialize_agents_matrix <- function(
  n_agents,
  n_timesteps,
  n_timesteps_per_career_step
) {
  # Initialize matrix of agents
  # Calculate total number of agents (recording agents each career step)
  total_agents <- n_agents *
    (floor(n_timesteps / n_timesteps_per_career_step) + 1)
  agents <- matrix(0, nrow = total_agents, ncol = 6)
  # Set column names
  colnames(agents) <- c(
    "researcher_id",
    "replication_probability",
    "target_power",
    "timestep_active",
    "timestep_inactive",
    "timestep_next_paper"
  )
  return(agents)
}
