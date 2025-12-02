##########################################################################
# Helper functions for running studies
##########################################################################

# Implemented:
# assign_effects: assign effect_ids to studies based on type
# determine_sample_sizes: calculate sample sizes using power.t.test
# determine_study_durations: calculate study completion times
# generate_study_results: simulate observed effect sizes and p-values
# kl_norm: KL divergence between two normal distributions
# prepare_bayesian_data: extract current beliefs and calculate new posteriors
# calculate_novelty_contribution: novelty contribution for each study
# calculate_truth_contribution: truth contribution for each study
# update_effects_beliefs: update effects matrix with new posterior beliefs

#### assign_effects ####
assign_effects <- function(sim_env) {
  n_studies <- nrow(sim_env$new_studies)
  
  # determine study types based on each agent's replication_probability
  sim_env$is_replication <- runif(n_studies) < sim_env$new_studies[, "replication_probability"]
  sim_env$new_studies[, "study_type"] <- ifelse(sim_env$is_replication, 1, 0)
  
  # identify available effect_ids for original studies
  available_original_effects <- sim_env$effects[
    !sim_env$effects[, "effect_id"] %in%
      sim_env$studies[
        sim_env$studies[, "publication_status"] == 1,
        "effect_id"
      ],
    "effect_id"
  ]
  
  # identify available effect_ids for replication studies
  available_replication_effects <- unique(sim_env$studies[
    sim_env$studies[, "publication_status"] == 1 & !is.na(sim_env$studies[, "effect_id"]),  
    "effect_id"
  ])
  
  # if not enough available effects for replication, convert excess to originals
  if (sum(sim_env$is_replication) > length(available_replication_effects)) {
    excess_replications <- sum(sim_env$is_replication) -
      sum(!is.na(available_replication_effects))
    convert_indices <- which(sim_env$is_replication)[order(sim_env$new_studies[
      sim_env$is_replication,
      "replication_probability"
    ])[1:excess_replications]]
    sim_env$is_replication[convert_indices] <- FALSE
    sim_env$new_studies[convert_indices, "study_type"] <- 0
  }
  
  # stop if not enough original effects left
  if(sum(!sim_env$is_replication) > length(available_original_effects)){
    stop("Insufficient original effects at timestep ", sim_env$timestep)
  }
  
  # assign effect_ids to original studies (without replacement)
  sim_env$new_studies[!sim_env$is_replication, "effect_id"] <- sample(
    available_original_effects,
    size = sum(!sim_env$is_replication)
  )
  
  # assign effect_ids to replication studies
  # count total publications per effect
  publication_counts <- table(sim_env$studies[
    sim_env$studies[, "publication_status"] == 1 & !is.na(sim_env$studies[, "effect_id"]),
    "effect_id"
  ])
  # add small random jitter to order effects randomly within each count level
  jittered_counts <- as.numeric(publication_counts) + runif(length(publication_counts)) * 0.01
  # order effect ids by jittered counts (ascending = fewer publications first)
  ordered_effects <- as.numeric(names(publication_counts)[order(jittered_counts)])
  # assign from ordered list (without replacement)
  sim_env$new_studies[sim_env$is_replication, "effect_id"] <- ordered_effects[1:sum(sim_env$is_replication)]
}

#### determine_sample_sizes ####
determine_sample_sizes <- function(sim_env) {
  browser()
  n_studies <- nrow(sim_env$new_studies)
  
  # calculate reference effects
  reference_effects <- numeric(n_studies)
  
  # originals: burn-in medium or mean published
  if (sim_env$timestep < sim_env$n_timesteps_per_career_step) {
    reference_effects[!sim_env$is_replication] <- 0.5
  } else {
    published_effects <- sim_env$studies[
      sim_env$studies[, "publication_status"] == 1 & 
        !is.na(sim_env$studies[, "estimated_mean"]),
      "estimated_mean"
    ]
    reference_effects[!sim_env$is_replication] <- mean(published_effects)
  }
  
  # replications: use original effect if significant, otherwise medium
  if (sum(sim_env$is_replication) > 0) {
    # filter to published original studies
    pub_orig <- sim_env$studies[, "study_type"] == 0 & 
                sim_env$studies[, "publication_status"] == 1 &
                !is.na(sim_env$studies[, "estimated_mean"])
    
    # match replication effect_ids to published originals
    rep_effect_ids <- sim_env$new_studies[sim_env$is_replication, "effect_id"]
    orig_indices <- match(rep_effect_ids, sim_env$studies[pub_orig, "effect_id"])
    
    # extract means and p-values for matched originals
    orig_means <- sim_env$studies[pub_orig, "estimated_mean"][orig_indices]
    orig_pvals <- sim_env$studies[pub_orig, "p_value"][orig_indices]
    
    # use original effect if p < 0.05, otherwise 0.5
    reference_effects[sim_env$is_replication] <- ifelse(
      orig_pvals < 0.05,
      abs(orig_means),
      0.5
    )
  }
  
  # calculate sample sizes with appropriate test type
  sample_sizes <- mapply(
    function(power, delta, alt) {
      power_result <- power.t.test(
        power = power,
        delta = abs(delta),
        sd = 1,
        sig.level = 0.05,
        type = "two.sample",
        alternative = alt
      )
      max(ceiling(power_result$n), 1)
    },
    power = sim_env$new_studies[, "target_power"],
    delta = reference_effects,
    alt = ifelse(sim_env$is_replication, "one.sided", "two.sided")
  )
  sim_env$new_studies[, "sample_size"] <- sample_sizes
}

#### determine_study_durations ####
determine_study_durations <- function(sim_env) {
  browser()
  # calculate duration: intercept (originals only) + coefficient * sample_size
  durations <- ceiling(
    ifelse(sim_env$is_replication, 0, sim_env$duration_original_intercept) + 
    sim_env$duration_per_observation * sim_env$new_studies[, "sample_size"]
  )
  
  # calculate when studies will be complete
  sim_env$new_studies[, "timestep_completed"] <- sim_env$timestep + durations
  
  # update agents matrix: when each researcher will be ready for next paper
  agent_indices <- match(
    sim_env$new_studies[, "researcher_id"], 
    sim_env$agents[, "researcher_id"]
  )
  sim_env$agents[agent_indices, "timestep_next_paper"] <- sim_env$new_studies[, "timestep_completed"]
}

#### generate_study_results ####
generate_study_results <- function(sim_env) {
  n_studies <- nrow(sim_env$new_studies)
  
  # get true effect sizes for each study's effect
  effect_indices <- match(
    sim_env$new_studies[, "effect_id"],
    sim_env$effects[, "effect_id"]
  )
  true_effects <- sim_env$effects[effect_indices, "true_effect_size"]
  
  # get sample sizes
  sample_sizes <- sim_env$new_studies[, "sample_size"]
  
  # simulate observed t-statistics using noncentral t-distribution
  # (when ncp = 0, this is a central t-distribution)
  ncp <- sqrt(sample_sizes / 2) * true_effects
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
    # filter to published original studies
    pub_orig <- sim_env$studies[, "study_type"] == 0 & 
                sim_env$studies[, "publication_status"] == 1 &
                !is.na(sim_env$studies[, "estimated_mean"])
    
    # match replication effect_ids to published originals
    rep_effect_ids <- sim_env$new_studies[sim_env$is_replication, "effect_id"]
    orig_indices <- match(rep_effect_ids, sim_env$studies[pub_orig, "effect_id"])
    
    # extract direction from matched originals
    orig_direction <- sim_env$studies[pub_orig, "estimated_mean"][orig_indices]
    
    # test in same direction as original
    p_obs[sim_env$is_replication] <- ifelse(
      orig_direction > 0,
      stats::pt(t_obs[sim_env$is_replication], df[sim_env$is_replication], lower.tail = FALSE),
      stats::pt(t_obs[sim_env$is_replication], df[sim_env$is_replication], lower.tail = TRUE)
    )
  }
  
  # originals: two-sided test
  p_obs[!sim_env$is_replication] <- 2 * stats::pt(
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
  # grab most recent entry for each effect_id
  # (effects matrix accumulates multiple rows per effect as beliefs update)
  
  # identify latest rows for each effect_id
  is_latest <- !duplicated(sim_env$effects[, "effect_id"], fromLast = TRUE)
  
  # match studies to their latest effect rows
  effect_match <- match(
    sim_env$new_studies[, "effect_id"],
    sim_env$effects[is_latest, "effect_id"]
  )
  effect_rows <- which(is_latest)[effect_match]
  
  # extract data needed for bayesian update and kl calculations
  sim_env$prior_means <- sim_env$effects[effect_rows, "posterior_effect_size"]
  sim_env$prior_vars <- sim_env$effects[effect_rows, "posterior_effect_variance"]
  sim_env$true_means <- sim_env$effects[effect_rows, "true_effect_size"]
  sim_env$true_vars <- sim_env$effects[effect_rows, "true_effect_variance"]
  
  # likelihood from study results
  likelihood_means <- sim_env$new_studies[, "estimated_mean"]
  likelihood_vars <- sim_env$new_studies[, "estimated_se"]^2
  
  # bayesian update (normal-normal conjugacy)
  posterior_vars <- 1 / (1 / sim_env$prior_vars + 1 / likelihood_vars)
  posterior_means <- (sim_env$prior_means / sim_env$prior_vars + 
                      likelihood_means / likelihood_vars) * posterior_vars
  
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
calculate_truth_contribution <- function(sim_env) {
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
  
  # find next available index in effects matrix
  # (first row where effect_id is NA, indicating an unfilled row)
  available_rows <- which(is.na(sim_env$effects[, "effect_id"]))
  
  if (length(available_rows) < n_published) {
    stop("Insufficient rows in effects matrix at timestep ", sim_env$timestep)
  }
  
  start_index <- available_rows[1]
  end_index <- start_index + n_published - 1
  
  # create new rows for each published study
  new_effect_rows <- cbind(
    effect_id = sim_env$new_studies[is_published, "effect_id"],
    timestep = rep(sim_env$timestep, n_published),
    true_effect_size = sim_env$true_means[is_published],
    true_effect_variance = sim_env$true_vars[is_published],
    prior_effect_size = sim_env$prior_means[is_published],
    prior_effect_variance = sim_env$prior_vars[is_published],
    posterior_effect_size = sim_env$new_posterior_means[is_published],
    posterior_effect_variance = sim_env$new_posterior_vars[is_published],
    study_id = sim_env$new_studies[is_published, "study_id"]
  )
  
  # fill in rows directly
  sim_env$effects[start_index:end_index, ] <- new_effect_rows
}
