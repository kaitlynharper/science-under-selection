#### Extract belief accuracy over time ####
extract_belief_accuracy <- function(sim_env) {
  effects <- sim_env$effects[
    !is.na(sim_env$effects[, "effect_id"]),
    ,
    drop = FALSE
  ]
  timesteps <- 0:sim_env$n_timesteps
  n_t <- length(timesteps)

  # Pre-allocate output
  out <- data.frame(
    timestep = timesteps,
    total_kl = numeric(n_t),
    mean_kl = numeric(n_t)
  )

  for (i in seq_along(timesteps)) {
    t <- timesteps[i]
    relevant <- effects[effects[, "timestep"] <= t, , drop = FALSE]

    if (nrow(relevant) == 0) {
      out$total_kl[i] <- NA
      out$mean_kl[i] <- NA
      next
    }

    # Get latest row per effect using base R
    latest_idx <- !duplicated(relevant[, "effect_id"], fromLast = TRUE)
    latest <- relevant[latest_idx, , drop = FALSE]

    kl <- kl_norm(
      latest[, "true_effect_size"],
      sqrt(latest[, "true_effect_variance"]),
      latest[, "posterior_effect_size"],
      sqrt(latest[, "posterior_effect_variance"])
    )

    out$total_kl[i] <- sum(kl)
    out$mean_kl[i] <- mean(kl)
  }
  out
}
