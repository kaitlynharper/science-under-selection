##########################################################################
# Helper functions for running studies
##########################################################################

# Implemented:
# assign_effects: assign effect_ids to studies based on type
# prepare_information: pull published-original and effect-belief info for later steps
# determine_sample_sizes: assign sample sizes (constant or power-based for replications)
# determine_study_durations: calculate study completion times
# generate_study_results: simulate observed effect sizes and p-values
# kl_norm: KL divergence between two normal distributions
# prepare_bayesian_data: calculate new posteriors from prepared priors + study results
# calculate_novelty_contribution: novelty contribution for each study
# calculate_truth_contribution: truth contribution for each study
# update_effects_beliefs: update effects matrix with new posterior beliefs

#### assign_effects ####
# verbose: logical; TRUE only when run_simulation(verbose=2), enables "available effects" and "replicators switched" messages
assign_effects <- function(sim_env, verbose = FALSE) {
  n_studies <- nrow(sim_env$new_studies)

  # determine study types based on each agent's replication_probability
  sim_env$is_replication <- runif(n_studies) <
    sim_env$new_studies[, "replication_probability"]
  sim_env$new_studies[, "study_type"] <- ifelse(sim_env$is_replication, 1, 0)

  # completed published studies (public knowledge)
  published_completed <- sim_env$studies[, "publication_status"] == 1 &
    !is.na(sim_env$studies[, "timestep_completed"]) &
    sim_env$studies[, "timestep_completed"] <= sim_env$timestep

  # in-progress published studies (will become public knowledge)
  published_in_progress <- sim_env$studies[, "publication_status"] == 1 &
    sim_env$studies[, "timestep_completed"] > sim_env$timestep

  published_completed_effect_ids <- unique(sim_env$studies[
    published_completed & !is.na(sim_env$studies[, "effect_id"]),
    "effect_id"
  ])

  published_in_progress_effect_ids <- unique(sim_env$studies[
    published_in_progress & !is.na(sim_env$studies[, "effect_id"]),
    "effect_id"
  ])

  # identify available effect_ids for original studies

  # TODO DOCUMENT: Eligible effects for original studies are those that
  # have not yet been published in completed studies and are not currently being
  # investigated in in-progress studies. The latter is a bit unrealistic
  # (as agents would need to know what effects all others are currently working on), but helps
  # avoiding conflicts where multiple original studies try to use the same effect_id.

  taken_effect_ids <- c(
    published_completed_effect_ids,
    published_in_progress_effect_ids
  )
  max_effect_id <- max(sim_env$effects[, "effect_id"], na.rm = TRUE)
  available_original_effects <- setdiff(seq_len(max_effect_id), taken_effect_ids)

  if (verbose) {
    print(paste0(
      "Available original effects: ",
      length(available_original_effects)
    ))
  }

  # TODO DOCUMENT: Eligible effects for replication studies are those that
  # have already been completed and published at least once. Then, when
  # assigning the replication effect IDs, we also consider in-progress replications
  # so that these effects are lower on the priority list for replication (currently priority only
  # considers how many times an effect has been published, no other factors)

  # if not enough available effects for replication, convert excess to originals
  # (all completed published studies can be replicated, even if they a replication is already completed or in progress)
  if (sum(sim_env$is_replication) > length(published_completed_effect_ids)) {
    excess_replications <- sum(sim_env$is_replication) -
      sum(!is.na(published_completed_effect_ids))
    convert_indices <- which(sim_env$is_replication)[order(sim_env$new_studies[
      sim_env$is_replication,
      "replication_probability"
    ])[1:excess_replications]]
    sim_env$is_replication[convert_indices] <- FALSE
    sim_env$new_studies[convert_indices, "study_type"] <- 0
    if (verbose) {
      print(paste0("Replicators switched to original studies: ", excess_replications))
    }
  }

  # TODO: This is a hotfix for a test (remove?)
  # In the burn-in period, all studies should be originals (=0)
  if (sim_env$timestep <= sim_env$burn_in_period) {
    sim_env$new_studies[, "study_type"] <- 0
  }

  # if not enough original effects, append a new block (double effects) and recompute pool
  n_original <- sum(!sim_env$is_replication)
  if (n_original > length(available_original_effects)) {
    # Create new effects matrix block
    new_ids <- (max_effect_id + 1):(max_effect_id + sim_env$n_effects)
    n_new <- length(new_ids)
    new_rows <- cbind(
      effect_id = new_ids,
      timestep = 0,
      true_effect_size = ifelse(
        runif(n_new) < sim_env$base_null_probability,
        0,
        rnorm(n_new, sim_env$effect_size_mean, sim_env$effect_size_variance)
      ),
      true_effect_variance = 0.01,
      prior_effect_size = NA,
      prior_effect_variance = NA,
      posterior_effect_size = sim_env$uninformed_prior_mean,
      posterior_effect_variance = sim_env$uninformed_prior_variance,
      study_id = NA
    )
    # Append new effects to effects matrix
    sim_env$effects <- rbind(sim_env$effects, new_rows)
    # Recompute available original effects
    max_effect_id <- max_effect_id + n_new
    available_original_effects <- setdiff(seq_len(max_effect_id), taken_effect_ids)
  }

  # assign effect_ids to original studies (without replacement)
  sim_env$new_studies[!sim_env$is_replication, "effect_id"] <- sample(
    available_original_effects,
    size = sum(!sim_env$is_replication),
    replace = FALSE
  )

  # assign effect_ids to replication studies
  # count publications including in-progress to deprioritize effects already being studied
  # (only effects with completed studies are actually replicable)
  replicable_studies <- (published_completed | published_in_progress) &
    !is.na(sim_env$studies[, "effect_id"]) &
    sim_env$studies[, "effect_id"] %in% published_completed_effect_ids

  publication_counts <- table(sim_env$studies[replicable_studies, "effect_id"])
  # add small random jitter to order effects randomly within each count level
  jittered_counts <- as.numeric(publication_counts) +
    runif(length(publication_counts)) * 0.01
  # order effect ids by jittered counts (ascending = fewer publications first)
  ordered_effects <- as.numeric(names(publication_counts)[order(
    jittered_counts
  )])
  # assign from ordered list (without replacement)
  sim_env$new_studies[sim_env$is_replication, "effect_id"] <- ordered_effects[
    1:sum(sim_env$is_replication)
  ]
}

#### prepare_information ####
# After effect assignment, cache lookups used by later steps
prepare_information <- function(sim_env) {
  # published completed originals (only needed for replications)
  if (any(sim_env$is_replication)) {
    pub_orig <- sim_env$studies[, "study_type"] == 0 &
      sim_env$studies[, "publication_status"] == 1 &
      !is.na(sim_env$studies[, "timestep_completed"]) &
      sim_env$studies[, "timestep_completed"] <= sim_env$timestep &
      !is.na(sim_env$studies[, "estimated_mean"])

    pub_orig_studies <- sim_env$studies[pub_orig, , drop = FALSE]
    orig_match <- match(
      sim_env$new_studies[, "effect_id"],
      pub_orig_studies[, "effect_id"]
    )
    sim_env$orig_estimated_mean <- pub_orig_studies[orig_match, "estimated_mean"]
    sim_env$orig_p_value <- pub_orig_studies[orig_match, "p_value"]
  }

  # latest belief row for each assigned effect
  is_latest <- !duplicated(sim_env$effects[, "effect_id"], fromLast = TRUE)
  effect_match <- match(
    sim_env$new_studies[, "effect_id"],
    sim_env$effects[is_latest, "effect_id"]
  )
  effect_rows <- which(is_latest)[effect_match]

  sim_env$true_means <- sim_env$effects[effect_rows, "true_effect_size"]
  sim_env$true_vars <- sim_env$effects[effect_rows, "true_effect_variance"]
  sim_env$prior_means <- sim_env$effects[effect_rows, "posterior_effect_size"]
  sim_env$prior_vars <- sim_env$effects[effect_rows, "posterior_effect_variance"]
}

#### determine_sample_sizes ####
determine_sample_sizes <- function(sim_env) {
  # all studies start with the base sample size
  sim_env$new_studies[, "sample_size"] <- sim_env$hold_samples_constant_at

  # if dynamic replication sample sizes disabled or no replications, we're done
  if (
    sim_env$replications_dynamic_sample_sizes == 0 ||
      sum(sim_env$is_replication) == 0
  ) {
    return()
  }

  # calculate power-based sample sizes for replications only
  orig_means <- sim_env$orig_estimated_mean[sim_env$is_replication]
  orig_pvals <- sim_env$orig_p_value[sim_env$is_replication]

  # reference effect: original effect if significant, otherwise 0.3
  reference_effects <- ifelse(orig_pvals < 0.05, abs(orig_means), 0.3)

  # calculate sample sizes for 80% power (one-sided test for replications)
  rep_sample_sizes <- vapply(
    reference_effects,
    function(delta) {
      power_result <- power.t.test(
        power = 0.8,
        delta = abs(delta),
        sd = 1,
        sig.level = 0.05,
        type = "two.sample",
        alternative = "one.sided"
      )
      max(ceiling(power_result$n), 1)
    },
    numeric(1)
  )

  sim_env$new_studies[sim_env$is_replication, "sample_size"] <- rep_sample_sizes
}

#### determine_study_durations ####
determine_study_durations <- function(sim_env) {
  # calculate duration: intercept (originals only) + coefficient * sample_size
  durations <- ceiling(
    ifelse(sim_env$is_replication, 0, sim_env$duration_original_intercept) +
      sim_env$duration_per_observation * sim_env$new_studies[, "sample_size"]
  )

  # calculate when studies will be complete
  sim_env$new_studies[, "timestep_completed"] <- sim_env$timestep + durations
  sim_env$new_studies[, "timesteps_duration"] <- durations

  # update agents matrix: when each researcher will be ready for next paper
  agent_indices <- match(
    sim_env$new_studies[, "researcher_id"],
    sim_env$agents[, "researcher_id"]
  )
  sim_env$agents[agent_indices, "timestep_next_paper"] <- sim_env$new_studies[,
    "timestep_completed"
  ]
}

#### generate_study_results ####
generate_study_results <- function(sim_env) {
  n_studies <- nrow(sim_env$new_studies)

  # get sample sizes
  sample_sizes <- sim_env$new_studies[, "sample_size"]

  # simulate observed t-statistics using noncentral t-distribution
  # (when ncp = 0, this is a central t-distribution)
  ncp <- sqrt(sample_sizes / 2) * sim_env$true_means
  df <- 2 * (sample_sizes - 1)
  t_obs <- stats::rt(n = n_studies, df = df, ncp = ncp)

  # convert t-statistics to cohen's d
  d_obs <- t_obs * sqrt(2 / sample_sizes)

  # calculate standard error of cohen's d using Hedges–Olkin SE(d) formula
  # for equal groups where sample_sizes is n per group
  # TODO more research on how this SE fits with our Bayesian approach
  se_obs <- sqrt(2 / sample_sizes + d_obs^2 / (4 * sample_sizes))

  # calculate p-values
  p_obs <- numeric(n_studies)

  # replications: one-sided test in direction of original study
  if (sum(sim_env$is_replication) > 0) {
    orig_direction <- sim_env$orig_estimated_mean[sim_env$is_replication]
    sim_env$new_studies[sim_env$is_replication, "p_value_original"] <-
      sim_env$orig_p_value[sim_env$is_replication]

    # test in same direction as original
    p_obs[sim_env$is_replication] <- ifelse(
      orig_direction > 0,
      stats::pt(
        t_obs[sim_env$is_replication],
        df[sim_env$is_replication],
        lower.tail = FALSE
      ),
      stats::pt(
        t_obs[sim_env$is_replication],
        df[sim_env$is_replication],
        lower.tail = TRUE
      )
    )
  }

  # originals: two-sided test
  p_obs[!sim_env$is_replication] <- 2 *
    stats::pt(
      abs(t_obs[!sim_env$is_replication]),
      df[!sim_env$is_replication],
      lower.tail = FALSE
    )

  # store results
  sim_env$new_studies[, "estimated_mean"] <- d_obs
  sim_env$new_studies[, "estimated_se"] <- se_obs
  sim_env$new_studies[, "p_value"] <- p_obs
}

#### kl_norm ####
kl_norm <- function(mu0, sd0, mu1, sd1) {
  # KL divergence from N(mu0, sd0) to N(mu1, sd1)
  log(sd1 / sd0) + (sd0^2 + (mu0 - mu1)^2) / (2 * sd1^2) - 0.5
}

#### prepare_bayesian_data ####
prepare_bayesian_data <- function(sim_env) {
  # likelihood from study results; priors/truth already set in prepare_information
  likelihood_means <- sim_env$new_studies[, "estimated_mean"]
  likelihood_vars <- sim_env$new_studies[, "estimated_se"]^2

  # bayesian update (normal-normal conjugacy)
  posterior_vars <- 1 / (1 / sim_env$prior_vars + 1 / likelihood_vars)
  posterior_means <- (sim_env$prior_means /
    sim_env$prior_vars +
    likelihood_means / likelihood_vars) *
    posterior_vars

  # save new posteriors in environment
  sim_env$new_posterior_means <- posterior_means
  sim_env$new_posterior_vars <- posterior_vars
}

#### calculate_novelty_contribution ####
calculate_novelty_contribution <- function(sim_env) {
  # information gain = KL(new posterior || old posterior)
  novelty <- kl_norm(
    sim_env$new_posterior_means,
    sqrt(sim_env$new_posterior_vars),
    sim_env$prior_means,
    sqrt(sim_env$prior_vars)
  )

  sim_env$new_studies[, "novelty_contribution"] <- novelty
}

#### calculate_truth_contribution ####
# TEMP: testing savage-dickey method. When truth_contribution_method == "savage_dickey",
# uses log posterior density at true effect - log prior density at true effect (point).
calculate_truth_contribution <- function(sim_env) {
  if (sim_env$truth_contribution_method == "savage_dickey") {
    # log Bayes factor (point at true effect): log p(theta_true|data) - log p(theta_true)
    log_prior_at_true <- stats::dnorm(
      sim_env$true_means,
      sim_env$prior_means,
      sqrt(sim_env$prior_vars),
      log = TRUE
    )
    log_posterior_at_true <- stats::dnorm(
      sim_env$true_means,
      sim_env$new_posterior_means,
      sqrt(sim_env$new_posterior_vars),
      log = TRUE
    )
    truth <- log_posterior_at_true - log_prior_at_true
  } else {
    # verisimilitude change = KL(true || prior) - KL(true || posterior)
    # positive when study moved beliefs closer to truth
    kl_prior_to_true <- kl_norm(
      sim_env$true_means,
      sqrt(sim_env$true_vars),
      sim_env$prior_means,
      sqrt(sim_env$prior_vars)
    )
    kl_posterior_to_true <- kl_norm(
      sim_env$true_means,
      sqrt(sim_env$true_vars),
      sim_env$new_posterior_means,
      sqrt(sim_env$new_posterior_vars)
    )
    truth <- kl_prior_to_true - kl_posterior_to_true
  }
  sim_env$new_studies[, "truth_contribution"] <- truth
}

#### update_effects_beliefs ####
update_effects_beliefs <- function(sim_env) {
  # filter to only published studies
  is_published <- sim_env$new_studies[, "publication_status"] == 1
  n_published <- sum(is_published)

  if (n_published == 0) {
    return() #finish if no published studies
  }

  # create new rows for each published study and append
  new_effect_rows <- cbind(
    effect_id = sim_env$new_studies[is_published, "effect_id"],
    timestep = sim_env$new_studies[is_published, "timestep_completed"],
    true_effect_size = sim_env$true_means[is_published],
    true_effect_variance = sim_env$true_vars[is_published],
    prior_effect_size = sim_env$prior_means[is_published],
    prior_effect_variance = sim_env$prior_vars[is_published],
    posterior_effect_size = sim_env$new_posterior_means[is_published],
    posterior_effect_variance = sim_env$new_posterior_vars[is_published],
    study_id = sim_env$new_studies[is_published, "study_id"]
  )
  sim_env$effects <- rbind(sim_env$effects, new_effect_rows)
}
