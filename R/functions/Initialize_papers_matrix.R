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
  papers <- matrix(0, nrow = total_papers, ncol = 11)
  # Set column names
  colnames(papers) <- c(
    "study_id",
    "researcher_id",
    "effect_id",
    "timestep_completed",
    "sample_size",
    "estimated_mean",
    "estimated_standard_error",
    "p_value",
    "novelty_contribution",
    "truth_contribution",
    "publication_status"
  )
  # Track current paper (ID and index should be the same for papers throughout)
  next_paper_ID <<- 1
  return(papers)
}
