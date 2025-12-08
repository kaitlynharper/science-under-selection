##########################################################################
# Career turnover function
##########################################################################

career_turnover <- function(sim_env) {
  #### Find active agents ####
  active_indices <- which(
    !is.na(sim_env$agents[, "researcher_id"]) &
      is.na(sim_env$agents[, "timestep_inactive"])
  )
  n_active <- length(active_indices)

  #### Calculate career_contribution for each active agent ####
  # Determine contribution column based on selection condition
  contribution_column <- if (sim_env$selection_condition == 0) {
    "truth_contribution"
  } else {
    "novelty_contribution"
  }

  # Filter studies to current career phase and published
  phase_start <- sim_env$timestep - sim_env$n_timesteps_per_career_step + 1
  in_phase <- which(
    sim_env$studies[, "publication_status"] == 1 &
      !is.na(sim_env$studies[, "timestep_completed"]) &
      sim_env$studies[, "timestep_completed"] >= phase_start &
      sim_env$studies[, "timestep_completed"] <= sim_env$timestep
  )

  # Sum contributions by researcher_id using rowsum
  contribution_sums <- rowsum(
    sim_env$studies[in_phase, contribution_column, drop = FALSE],
    sim_env$studies[in_phase, "researcher_id"]
  )

  # Match contributions to active agents (agents with no studies get 0)
  active_researcher_ids <- sim_env$agents[active_indices, "researcher_id"]
  career_contribution <- contribution_sums[
    match(active_researcher_ids, rownames(contribution_sums)),
    1
  ]
  career_contribution[is.na(career_contribution)] <- 0

  #### Identify agents with lowest percentile of career_contribution ####
  threshold <- quantile(
    career_contribution,
    probs = sim_env$career_turnover_selection_rate,
    na.rm = TRUE
  )
  which_to_retire <- which(career_contribution < threshold)
  retire_indices <- active_indices[which_to_retire]
  n_retire <- length(retire_indices)

  print(paste0(
    "Retiring ",
    n_retire,
    " agents out of ",
    n_active,
    " active agents."
  ))
  # Mark low-performing agents as inactive
  sim_env$agents[retire_indices, "timestep_inactive"] <- sim_env$timestep

  #### Replace retired agents with new agents ####
  surviving_indices <- active_indices[-which_to_retire]

  if (length(which_to_retire > 0)) {
    # sample from survivors and add innovation noise
    new_rep_probs <- sample(
      sim_env$agents[surviving_indices, "replication_probability"],
      n_retire,
      replace = TRUE
    ) +
      rnorm(n_retire, 0, sim_env$innovation_sd)
    new_rep_probs <- pmax(0, pmin(1, new_rep_probs)) # ensure probabilities are between 0 and 1

    new_powers <- sample(
      sim_env$agents[surviving_indices, "target_power"],
      n_retire,
      replace = TRUE
    ) +
      rnorm(n_retire, 0, sim_env$innovation_sd)
    new_powers <- pmax(0, pmin(1, new_powers)) # ensure powers are between 0 and 1

    add_agents(
      sim_env = sim_env,
      n_agents = n_retire,
      timestep_active = sim_env$timestep,
      replication_probabilities = new_rep_probs,
      target_powers = new_powers,
      timestep_next_papers = rep(sim_env$timestep, n_retire)
    )
  }
}
