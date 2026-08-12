#### Function: initialize_agents_matrix ####

initialize_agents_matrix <- function(sim_env) {
  # Initialize empty agents matrix (grows via rbind as agents are added)
  sim_env$agents <- matrix(NA, nrow = 0, ncol = 6)
  colnames(sim_env$agents) <- c(
    "researcher_id",
    "replication_probability",
    "target_power",
    "timestep_active",
    "timestep_inactive",
    "timestep_next_paper"
  )
}
