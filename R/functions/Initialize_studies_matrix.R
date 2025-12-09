#### Function: initialize_studies_matrix ####

initialize_studies_matrix <- function(sim_env) {
  # Initialize matrix of studies
  # TODO update this calculation once we have better calibration of study counts
  total_studies <- sim_env$n_agents * sim_env$n_timesteps
  sim_env$studies <- matrix(NA, nrow = total_studies, ncol = 12)
  colnames(sim_env$studies) <- c(
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
}
