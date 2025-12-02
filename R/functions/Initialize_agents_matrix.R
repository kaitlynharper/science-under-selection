#### Function: initialize_agents_matrix ####

initialize_agents_matrix <- function(sim_env) {
  # Initialize matrix of agents
  # Calculate total number of agents anticipated
  total_agents <- sim_env$n_agents * sim_env$n_timesteps # TODO update once career phases are implemented
  sim_env$agents <- matrix(NA, nrow = total_agents, ncol = 6)
  colnames(sim_env$agents) <- c(
    "researcher_id",
    "replication_probability",
    "target_power",
    "timestep_active",
    "timestep_inactive",
    "timestep_next_paper"
  )
}
