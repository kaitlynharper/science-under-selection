#### Function: initialize_effects_matrix ####

initialize_effects_matrix <- function(sim_env) {
  # Initialize matrix of effects
  # TODO update this calculation once we have better calibration of belief updates
  n_effects <- sim_env$n_effects
  total_rows <- n_effects + (sim_env$n_agents * sim_env$n_timesteps)
  sim_env$effects <- matrix(NA, nrow = total_rows, ncol = 9)
  colnames(sim_env$effects) <- c(
    "effect_id",
    "timestep",
    "true_effect_size",
    "true_effect_variance",
    "prior_effect_size",
    "prior_effect_variance",
    "posterior_effect_size",
    "posterior_effect_variance",
    "study_id"
  )

# I think I also need replication counter on effects matrix 
# (so I can enter uninformed, original, and replication updates)
# And also a column for the study id when the update happens
# what if I make a column for each type of update? 
# original_study_ID, original_posterior_mean and variance, replication_study_ID, replication_posterior_mean and variance
# and get rid of the uninformed prior columns because they will be a constant

  # Generate ids
  effect_ids <- 1:n_effects
  # Set timestep
  timesteps <- rep(0, n_effects)
  # Generate true effect sizes
  true_effect_sizes <- ifelse(
    runif(n_effects) < sim_env$base_null_probability, # such that base_null_probability% are 0
    0,
    rnorm(n_effects, sim_env$effect_size_mean, sim_env$effect_size_variance) # and the rest are drawn from normal distribution
  )

  # Generate true effect variance (not needed because they will all be the same - very narrow?)
  true_effect_variances <- rep(0.01, n_effects)
  # Set uninformative prior effect size
  prior_effect_sizes <- rep(NA, n_effects)
  # Set uninformative prior variance
  prior_effect_variances <- rep(NA, n_effects)
  # Initialize posterior (starts same as uninformed prior)
  posterior_effect_sizes <- rep(sim_env$uninformed_prior_mean, n_effects)
  posterior_effect_variances <- rep(sim_env$uninformed_prior_variance, n_effects)
  # Initialize study_id (NA = no studies done yet)
  study_ids <- rep(NA, n_effects)

  # Fill first n_effects rows with initial effect values
  # (remaining rows left as NA for future belief updates)
  sim_env$effects[1:n_effects, ] <- cbind(
    effect_ids,
    timesteps,
    true_effect_sizes,
    true_effect_variances,
    prior_effect_sizes,
    prior_effect_variances,
    posterior_effect_sizes,
    posterior_effect_variances,
    study_ids
  )
}
