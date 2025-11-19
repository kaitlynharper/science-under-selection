#### Function: generate_initial_agents ####

generate_initial_agents <- function(n_agents) {
  # Set up initial current_agents
  current_agents <- matrix(0, nrow = n_agents, ncol = 6)
  # Set column names
  colnames(current_agents) <- c(
    "researcher_id",
    "replication_probability",
    "target_power",
    "timestep_active",
    "timestep_inactive",
    "timestep_next_paper"
  )
  # Generate ids (starts at 1 for initial population)
  researcher_ids <- 1:n_agents
  # Set timestep_active (0 for initial population)
  timesteps_active <- rep(0, n_agents)
  # Set timestep_inactive (NA for active agents)
  timesteps_inactive <- rep(NA, n_agents)
  # Generate replication_probability (between 0 and 1)
  replication_probabilities <- pmax(0, pmin(1, runif(n_agents, 0, 1)))
  # Generate target_power (between 0 and 1)
  target_powers <- pmax(0, pmin(1, runif(n_agents, 0, 1)))
  # Initialize timestep_next_paper (0 = ready to start a new paper)
  timestep_next_papers <- rep(0, n_agents)
  # Fill agents matrix with these values
  current_agents[] <- cbind(
    researcher_ids,
    replication_probabilities,
    target_powers,
    timesteps_active,
    timesteps_inactive,
    timestep_next_papers
  )
  return(current_agents)
}
