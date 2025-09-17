#### Function: generate_initial_agents ####

generate_initial_agents <- function(n_agents) {
  # Set up initial current_agents
  current_agents <- matrix(0, nrow = n_agents, ncol = 6)
  # Set column names
  colnames(current_agents) <- c(
    "researcher_ID",
    "timestep",
    "prob_replicate",
    "career_level",
    "timesteps_in_career_level",
    "total_papers"
  )
  # Generate IDs
  researcher_IDs <- next_agent_ID:(next_agent_ID + n_agents - 1)
  # Set timestep
  timesteps <- rep(0, n_agents)
  # Generate prob_replicate (between 0 and 1)
  prob_replicates <- pmax(0, pmin(1, rnorm(n_agents, 0.5, 0.5)))
  # Assign career level
  career_levels <- rep(1:5, length.out = n_agents)
  # Assign timesteps agents have been in current career level
  timesteps_in_career_levels <- sample(1:10, n_agents, replace = TRUE)
  # Assign 0 papers for initial agents
  total_papers <- rep(0, n_agents)

  # Fill agents matrix with these values
  # Add to matrix
  current_agents[
    next_agent_index:(next_agent_index +
      (length(researcher_IDs)) -
      1),
  ] <-
    cbind(
      researcher_IDs,
      timesteps,
      prob_replicates,
      career_levels,
      timesteps_in_career_levels,
      total_papers
    )
  # Update next agent ID
  next_agent_ID <<- next_agent_ID + length(researcher_IDs)
  return(current_agents)
}
