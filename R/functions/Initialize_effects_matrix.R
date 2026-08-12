#### Function: initialize_effects_matrix ####

initialize_effects_matrix <- function(sim_env) {
  # Initialize effects matrix with starting effects only
  # (grows via rbind as new effects are created or beliefs update)
  n_effects <- sim_env$n_effects

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

  sim_env$effects <- cbind(
    effect_id = effect_ids,
    timestep = timesteps,
    true_effect_size = true_effect_sizes,
    true_effect_variance = true_effect_variances,
    prior_effect_size = prior_effect_sizes,
    prior_effect_variance = prior_effect_variances,
    posterior_effect_size = posterior_effect_sizes,
    posterior_effect_variance = posterior_effect_variances,
    study_id = study_ids
  )
}
