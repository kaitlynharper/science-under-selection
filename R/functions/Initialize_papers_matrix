#### Function: initialize_papers_matrix ####

initialize_papers_matrix <- function(
  n_agents,
  n_timesteps,
  mean_papers_per_agent_per_timestep
) {
  #Initialize matrix of papers
  # Calculate total number of papers
  total_papers <- n_agents *
    n_timesteps *
    (mean_papers_per_agent_per_timestep + 1)
  papers <- matrix(0, nrow = total_papers, ncol = 8)
  # Set column names
  colnames(papers) <- c(
    "paper_ID",
    "timestep",
    "author_ID",
    "hypothesis_ID",
    "estimated_m",
    "estimated_se",
    "verisimilitude_gain",
    "information_gain"
  )
  # Track current paper (ID and index should be the same for papers throughout)
  next_paper_ID <<- 1
  return(papers)
}
