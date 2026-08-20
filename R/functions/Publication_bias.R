##########################################################################
# Publication bias helper functions
##########################################################################

# Functions:
# logistic_significant: publication probability for significant results (p < .05)
# logistic_nonsignificant: publication probability for non-significant results
# apply_publication_bias: determine which studies get published based on significance and novelty

#### Function: logistic_significant ####
# Publication probability for significant results (p < .05)
# Uses lower asymptote to set minimum probability and logistic curve for novelty bonus
logistic_significant <- function(
  novelty,
  sig_lower_asymptote,
  sig_logistic_midpoint,
  sig_logistic_steepness
) {
  sig_lower_asymptote + 
    ((1 - sig_lower_asymptote) / (1 + exp(-sig_logistic_steepness * (novelty - sig_logistic_midpoint))))
}

#### Function: logistic_nonsignificant ####
# Publication probability for non-significant results
# Uses logistic curve from 0 to 1 based on novelty
logistic_nonsignificant <- function(
  novelty,
  nonsig_logistic_midpoint,
  nonsig_logistic_steepness
) {
  1 / (1 + exp(-nonsig_logistic_steepness * (novelty - nonsig_logistic_midpoint)))
}

#### Function: apply_publication_bias ####
# Determine which studies get published based on significance and novelty
# Curve parameters come from params (sweeps override nonsig_logistic_midpoint via run_sweep merge)
# During burn-in, publication bias is always on; after burn-in, use configured publication_bias
apply_publication_bias <- function(sim_env) {
  
  n_studies <- nrow(sim_env$new_studies)

  # Determine if publication bias is currently effective based on burn-in and publication_bias parameter
  effective_pb <- if (sim_env$timestep <= sim_env$burn_in_period) {
    1
  } else {
    sim_env$publication_bias
  }
  
  if (effective_pb == 0) { # No publication bias
    # All papers are published
    sim_env$new_studies[, "publication_status"] <- rep(1, n_studies)
    return()
  }

  # determine if each study is significant
  is_significant <- sim_env$new_studies[, "p_value"] < 0.05
  
  # calculate publication probabilities based on significance and novelty
  publication_prob <- numeric(n_studies)
  
  # significant results: use logistic_significant
  publication_prob[is_significant] <- logistic_significant(
    novelty = sim_env$new_studies[is_significant, "novelty_contribution"],
    sig_lower_asymptote = sim_env$sig_lower_asymptote,
    sig_logistic_midpoint = sim_env$sig_logistic_midpoint,
    sig_logistic_steepness = sim_env$sig_logistic_steepness
  )
  
  # non-significant results: use logistic_nonsignificant
  publication_prob[!is_significant] <- logistic_nonsignificant(
    novelty = sim_env$new_studies[!is_significant, "novelty_contribution"],
    nonsig_logistic_midpoint = sim_env$nonsig_logistic_midpoint,
    nonsig_logistic_steepness = sim_env$nonsig_logistic_steepness
  )

  # determine publication status (1 = published, 0 = not published)
  sim_env$new_studies[, "publication_status"] <- as.integer(
    runif(n_studies) < publication_prob
  )
  
  # 1: all replications published; 0: replications follow regular publication bias
  if (sim_env$all_replications_published == 1) {
    # all replications are published
    is_replication <- sim_env$new_studies[, "study_type"] == 1
    sim_env$new_studies[is_replication, "publication_status"] <- 1
  }
}
