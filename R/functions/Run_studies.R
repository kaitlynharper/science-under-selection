#### Function: run_studies ####

run_studies <- function(agents, effects, studies, timestep, next_study_id) {
  # Identify agents who are ready to start a new study and are active
  # grab their researcher_id, replication_probability, and target_power
  browser() #debug
  ready_agents <- agents[
    agents[, "timestep_next_paper"] == timestep,
    c("researcher_id", "replication_probability", "target_power"),
    drop = FALSE
  ]
  n_studies <- nrow(ready_agents)
  # return NULL if no agents are ready
  if (n_studies == 0) {
    return(NULL)
  }

  # Add columns to make a new studies matrix
  new_studies <- cbind(
    ready_agents,
    study_id = next_study_id:(next_study_id + n_studies - 1),
    effect_id = rep(NA, n_studies),
    study_type = rep(NA, n_studies),
    sample_size = rep(NA, n_studies),
    estimated_mean = rep(NA, n_studies),
    estimated_se = rep(NA, n_studies),
    p_value = rep(NA, n_studies)
  )
  # update next_study_id
  next_study_id <<- next_study_id + n_studies

  # Determine study types
  # Determine original or replication based on each agent's replication_probability
  is_replication <- runif(n_studies) < new_studies[, "replication_probability"]
  new_studies[, "study_type"] <- ifelse(is_replication, "1", "0")
  # Identify available effect_IDs for original studies
  available_original_effects <- effects[
    !effects[, "effect_id"] %in%
      studies[studies[, "publication_status"] == 1, "effect_id"],
    "effect_id"
  ]
  # Identify available effect_IDs for replication studies
  available_replication_effects <- unique(studies[
    studies[, "publication_status"] == 1,
    "effect_id"
  ])
  # If there are enough available effect_IDs for replication, go ahead, otherwise reassign agents to original studies
  if (sum(is_replication) > length(available_replication_effects)) {
    # turn the excess replications with the lowest replication_probability into original studies
    excess_replications <- sum(is_replication) -
      length(available_replication_effects)
    convert_indices <- which(is_replication)[order(new_studies[
      is_replication,
      "replication_probability"
    ])[1:excess_replications]]
    is_replication[convert_indices] <- FALSE #update logical indexing
    new_studies[convert_indices, "study_type"] <- "original" #update study type
  }

  # Assign effect_IDs to agents for original studies from available effect_IDs (without replacement)
  # Assign effect_IDs to agents for replication studies from available effect_IDs (without replacement)

  # Run original studies

  # Set reference effect size (first 20 studies(?) = medium cohens d, then mean of all published effect sizes)
  # Determine sample size using target_power and all published effect sizes (what to do for first timestep studies? medium cohens d)
  # Determine number of timesteps the study will take (and record the timestep when it will be done in paper and agent)
  # Use sample size and true effect size to determine observed effect size and p-value
  # Calculate novelty contribution and truth contribution and update effects matrix

  # Run replication studies

  # Set reference effect size (mean of original study for that effect_ID)
  # Determine sample size using target_power and previously published effect sizes
  # Determine number of timesteps the study will take (and record the timestep when it will be done in paper and agent)
  # Use sample size and true effect size to determine observed effect size and p-value (use one-tailed tests...)
  # Calculate novelty contribution and truth contribution and update effects matrix
}
