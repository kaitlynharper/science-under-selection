#### Function: initialize_studies_matrix ####

initialize_studies_matrix <- function(
  n_agents,
  n_timesteps,
  mean_studies_per_agent_per_timestep
) {
  # Initialize matrix of studies
  # Calculate total number of studies
  total_studies <- n_agents *
    n_timesteps *
    (mean_studies_per_agent_per_timestep + 1)
  studies <- matrix(0, nrow = total_studies, ncol = 12)
  # Set column names
  colnames(studies) <- c(
    "study_id",
    "researcher_id",
    "effect_id",
    "study_type",
    "timestep_completed",
    "sample_size",
    "estimated_mean",
    "estimated_se",
    "p_value",
    "novelty_contribution",
    "truth_contribution",
    "publication_status"
  )
  return(studies)
}
