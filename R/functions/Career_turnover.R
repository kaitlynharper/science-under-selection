##########################################################################
# Career turnover function
##########################################################################

career_turnover <- function(sim_env, verbose = FALSE) {
  # create a local copy of the non-empty studies to speed up selection
  studies <- sim_env$studies[!is.na(sim_env$studies[, "study_id"]), ]

  #### Find active agents ####
  active_indices <- which(
    !is.na(sim_env$agents[, "researcher_id"]) &
      is.na(sim_env$agents[, "timestep_inactive"])
  )
  n_active <- length(active_indices)

  #### Calculate career_contribution for each active agent ####
  # Determine contribution column based on selection condition
  contribution_column <- if (sim_env$current_selection_condition == 0) {
    "truth_contribution"
  } else {
    "novelty_contribution"
  }

  # Filter studies to current career phase and published
  phase_start <- sim_env$timestep - sim_env$n_timesteps_per_career_step + 1

  # Under truth selection, consider all studies
  if (sim_env$current_selection_condition == 0) {
    in_phase <- which(
      !is.na(studies[, "timestep_completed"]) &
        studies[, "timestep_completed"] >= phase_start &
        studies[, "timestep_completed"] <= sim_env$timestep
    )
  }

  # Under novelty selection, only consider published studies
  if (sim_env$current_selection_condition == 1) {
    in_phase <- which(
      studies[, "publication_status"] == 1 &
        !is.na(studies[, "timestep_completed"]) &
        studies[, "timestep_completed"] >= phase_start &
        studies[, "timestep_completed"] <= sim_env$timestep
    )
  }

  # Sum contributions by researcher_id using rowsum
  contribution_sums <- rowsum(
    studies[in_phase, contribution_column, drop = FALSE],
    studies[in_phase, "researcher_id"]
  )

  # Match contributions to active agents (agents with no studies get 0)
  active_researcher_ids <- sim_env$agents[active_indices, "researcher_id"]
  career_contribution <- contribution_sums[
    match(active_researcher_ids, rownames(contribution_sums)),
    1
  ]
  career_contribution[is.na(career_contribution)] <- 0

  #### Identify bottom % of agents based on career_contribution ####
  # Rank agents by career_contribution (with random ties)
  ranks <- rank(career_contribution, ties.method = "random")
  # Identify the cutoff rank according to career_turnover_selection_rate
  n_retire <- floor(n_active * sim_env$career_turnover_selection_rate)
  cutoff_rank <- n_retire
  # Grab the indices of those agents
  which_to_retire <- which(ranks <= cutoff_rank)
  retire_indices <- active_indices[which_to_retire]
  # Mark the low-performing agents as inactive
  sim_env$agents[retire_indices, "timestep_inactive"] <- sim_env$timestep

  #### Replace retired agents with new agents ####
  surviving_indices <- active_indices[-which_to_retire]

  if (verbose) {
    print(paste0(
      "------- Career Turnover: Selecting on ",
      contribution_column,
      " -------"
    ))
    print(paste0("How many agents retiring: ", length(which_to_retire)))
  }

  if (length(which_to_retire > 0)) {
    #only add agents if needed
    # sample from survivors and add innovation noise
    new_rep_probs <- sample(
      sim_env$agents[surviving_indices, "replication_probability"],
      n_retire,
      replace = TRUE
    ) +
      rnorm(n_retire, 0, sim_env$innovation_sd)
    new_rep_probs <- pmax(0, pmin(1, new_rep_probs)) # ensure probabilities are between 0 and 1

    # apply mutation: flip replication_probability (0->1 or 1->0) with mutation_rate probability
    if (sim_env$mutation_rate > 0) {
      will_mutate <- runif(n_retire) < sim_env$mutation_rate
      new_rep_probs[will_mutate] <- 1 - new_rep_probs[will_mutate]
    }

    new_powers <- sample(
      sim_env$agents[surviving_indices, "target_power"],
      n_retire,
      replace = TRUE
    ) +
      rnorm(n_retire, 0, sim_env$innovation_sd)
    new_powers <- pmax(0.01, pmin(0.99, new_powers)) # ensure powers are between 0.01 and 0.99

    add_agents(
      sim_env = sim_env,
      n_agents = n_retire,
      timestep_active = sim_env$timestep,
      replication_probabilities = new_rep_probs,
      target_powers = new_powers,
      timestep_next_papers = rep(sim_env$timestep + 1, n_retire) # new agents are ready to do studies next timestep
    )
  }
}
