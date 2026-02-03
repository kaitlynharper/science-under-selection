##########################################################################
# Publication bias helper functions
##########################################################################

#### logistic_significant ####
# Publication probability for significant results (p < .05)
# Uses y-intercept to set minimum probability and logistic curve for novelty bonus
logistic_significant <- function(
  novelty,
  sig_y_intercept,
  sig_logistic_midpoint,
  sig_logistic_steepness
) {
  sig_y_intercept + 
    ((1 - sig_y_intercept) / (1 + exp(-sig_logistic_steepness * (novelty - sig_logistic_midpoint))))
}

#### logistic_nonsignificant ####
# Publication probability for non-significant results
# Pure logistic curve from 0 to 1 based on novelty
logistic_nonsignificant <- function(
  novelty,
  nonsig_logistic_midpoint,
  nonsig_logistic_steepness
) {
  1 / (1 + exp(-nonsig_logistic_steepness * (novelty - nonsig_logistic_midpoint)))
}

#### apply_publication_bias ####
# Determine which studies get published based on significance and novelty
apply_publication_bias <- function(sim_env) {
  
  n_studies <- nrow(sim_env$new_studies)
  
  #### Define publication bias parameters based on publication_bias level ####
  
  if(sim_env$publication_bias == 0){ #No publication bias
    # All papers are published
    sim_env$new_studies[, "publication_status"] <- rep(1, n_studies)
    return()
  }
  
  if(sim_env$publication_bias == 1){ # Weak publication bias
    sim_env$sig_y_intercept = 0.5 # minimum publication probability for p < .05 results
    sim_env$sig_logistic_midpoint = 0.5 # novelty midpoint for significant results
    sim_env$sig_logistic_steepness = 3 # steepness of logistic curve for significant results
    sim_env$nonsig_logistic_midpoint = 1.5 # novelty midpoint for non-significant results
    sim_env$nonsig_logistic_steepness = 3 # steepness of logistic curve for non-significant results
  }
  
  if(sim_env$publication_bias == 2){ # Strong publication bias
    sim_env$sig_y_intercept = 0.8 # minimum publication probability for p < .05 results
    sim_env$sig_logistic_midpoint = 0.2 # novelty midpoint for significant results
    sim_env$sig_logistic_steepness = 3 # steepness of logistic curve for significant results
    sim_env$nonsig_logistic_midpoint = 3 # novelty midpoint for non-significant results
    sim_env$nonsig_logistic_steepness = 3 # steepness of logistic curve for non-significant results
  }

  if (exists("run_sweep")) {
    sim_env$nonsig_logistic_midpoint = sim_env$set_nonsig_logistic_midpoint
  }

  # determine if each study is significant
  is_significant <- sim_env$new_studies[, "p_value"] < 0.05
  
  # calculate publication probabilities based on significance and novelty
  publication_prob <- numeric(n_studies)
  
  # significant results: use logistic_significant
  publication_prob[is_significant] <- logistic_significant(
    novelty = sim_env$new_studies[is_significant, "novelty_contribution"],
    sig_y_intercept = sim_env$sig_y_intercept,
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
  
  # if all_replications_published is 1, set all replications to published
  if (sim_env$all_replications_published == 1) {
    is_replication <- sim_env$new_studies[, "study_type"] == 1
    sim_env$new_studies[is_replication, "publication_status"] <- 1
  }
}
  