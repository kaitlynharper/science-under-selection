#### Function: run_studies ####

run_studies <- function(sim_env) {
  # Identify agents who are ready to start a new study and are active
  browser() #debug point

  # Filter for active agents who are ready to start a new paper
  ready_indices <- which(
    !is.na(sim_env$agents[, "researcher_id"]) &
      is.na(sim_env$agents[, "timestep_inactive"]) &
      sim_env$agents[, "timestep_next_paper"] == sim_env$timestep
  )

  ready_agents <- sim_env$agents[
    ready_indices,
    c("researcher_id", "replication_probability", "target_power"),
    drop = FALSE
  ]
  n_studies <- nrow(ready_agents)
  # return NULL if no agents are ready (TODO revisit this once output is finalised)
  if (n_studies == 0) {
    return(NULL)
  }

  # Generate new study IDs (max + 1)
  existing_study_ids <- sim_env$studies[, "study_id"]
  if (all(is.na(existing_study_ids))) {
    next_study_id <- 1
  } else {
    next_study_id <- max(existing_study_ids, na.rm = TRUE) + 1
  }
  
  # Add columns to make a new studies matrix
  new_studies <- cbind(
    ready_agents,
    study_id = next_study_id:(next_study_id + n_studies - 1),
    effect_id = rep(NA, n_studies),
    study_type = rep(NA, n_studies),
    timestep_completed = rep(NA, n_studies),
    sample_size = rep(NA, n_studies),
    estimated_mean = rep(NA, n_studies),
    estimated_se = rep(NA, n_studies),
    p_value = rep(NA, n_studies),
    novelty_contribution = rep(NA, n_studies),
    truth_contribution = rep(NA, n_studies),
    publication_status = rep(1, n_studies)
  )
  
  # Store new_studies in environment for helper functions
  sim_env$new_studies <- new_studies

  # Determine study types and assign effects
  assign_effects(sim_env)
  
  # Calculate reference effects and sample sizes
  determine_sample_sizes(sim_env)
  
  # Calculate study durations and update agent next paper times
  determine_study_durations(sim_env)
  
  # Generate study results (observed effect sizes and p-values)
  generate_study_results(sim_env)
  
  # Calculate Bayesian posteriors and contribution metrics
  prepare_bayesian_data(sim_env)
  calculate_novelty_contribution(sim_env)
  calculate_truth_contribution(sim_env)

  # Fill in new studies into studies matrix
  # Find next available index in studies matrix (first row with NA study_id)
  existing_study_ids <- sim_env$studies[, "study_id"]
  if (all(is.na(existing_study_ids))) {
    start_index <- 1
  } else {
    start_index <- which(is.na(existing_study_ids))[1]
  }
  end_index <- start_index + n_studies - 1
  
  # Fill in study columns (excluding agent-specific columns)
  study_columns <- c("researcher_id", "study_id", "effect_id", "study_type", "timestep_completed",
                     "sample_size", "estimated_mean", "estimated_se", "p_value",
                     "novelty_contribution", "truth_contribution", "publication_status")
  
  sim_env$studies[start_index:end_index, study_columns] <- sim_env$new_studies[, study_columns]
  
}
