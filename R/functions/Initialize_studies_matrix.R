##########################################################################
# Initialize studies matrix
##########################################################################

#### Function: initialize_studies_matrix ####

initialize_studies_matrix <- function(sim_env) {
  # Initialize empty studies matrix (grows via rbind as studies are run)
  sim_env$studies <- matrix(NA, nrow = 0, ncol = 14)
  colnames(sim_env$studies) <- c(
    "study_id",
    "researcher_id",
    "effect_id",
    "study_type",
    "timestep_completed",
    "timesteps_duration",
    "sample_size",
    "estimated_mean",
    "estimated_se",
    "p_value",
    "p_value_original",
    "novelty_contribution",
    "truth_contribution",
    "publication_status"
  )
}
