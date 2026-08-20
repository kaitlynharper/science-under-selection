##########################################################################
# Run studies
##########################################################################

#### Function: run_studies ####

# verbose: logical; when called from run_simulation, TRUE only when run_simulation(verbose=2)
run_studies <- function(sim_env, verbose = FALSE) {
  # Identify agents who are ready to start a new study and are active
  ready_indices <- which(
    !is.na(sim_env$agents[, "researcher_id"]) &
      is.na(sim_env$agents[, "timestep_inactive"]) &
      sim_env$agents[, "timestep_next_paper"] == sim_env$timestep
  )

  if (verbose) {
    print(paste0(
      ready_indices |> length(),
      " agents are ready to run studies."
    ))
  }

  # Grab the ready agents
  ready_agents <- sim_env$agents[
    ready_indices,
    c("researcher_id", "replication_probability", "target_power"),
    drop = FALSE
  ]
  n_studies <- nrow(ready_agents)
  # If no agents are ready, return NULL
  # return NULL if no agents are ready
  if (n_studies == 0) {
    return(NULL)
  }

  # Generate new study IDs
  existing_study_ids <- sim_env$studies[, "study_id"]
  if (length(existing_study_ids) == 0) {
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
    timesteps_duration = rep(NA, n_studies),
    sample_size = rep(NA, n_studies),
    estimated_mean = rep(NA, n_studies),
    estimated_se = rep(NA, n_studies),
    p_value = rep(NA, n_studies),
    p_value_original = rep(NA, n_studies),
    novelty_contribution = rep(NA, n_studies),
    truth_contribution = rep(NA, n_studies),
    publication_status = rep(NA, n_studies)
  )

  # Store new_studies in environment for efficient passing to helper functions
  sim_env$new_studies <- new_studies

  # Determine study types and assign effects
  assign_effects(sim_env, verbose = verbose)

  # Cache info for later steps (published originals, current beliefs, truth)
  prepare_information(sim_env)

  # Calculate sample sizes (esp for replications based on original effect size)
  determine_sample_sizes(sim_env)

  # Calculate study durations and note when agents can run next studies
  determine_study_durations(sim_env)

  # Generate study results (observed effect sizes and p-values)
  generate_study_results(sim_env)

  # Calculate Bayesian posteriors and contribution metrics
  prepare_bayesian_data(sim_env)
  calculate_novelty_contribution(sim_env)
  calculate_truth_contribution(sim_env)

  # Determine which studies get published based on significance and novelty
  apply_publication_bias(sim_env)

  # Update effects matrix with new posterior beliefs
  update_effects_beliefs(sim_env)

  # Append new studies to studies matrix
  study_columns <- c(
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
  sim_env$studies <- rbind(
    sim_env$studies,
    sim_env$new_studies[, study_columns]
  )
}
