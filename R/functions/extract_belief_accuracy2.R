#### Extract belief accuracy over time ####
extract_belief_accuracy2 <- function(sim_env) {

  # select only effects that have been at least investigated once:
  # The cumulative knowledge is defined by the published literature
  investigated_effect_ids <- sim_env$effects[
    !is.na(sim_env$effects[, "effect_id"]) & !is.na(sim_env$effects[, "study_id"]),
    "effect_id"
  ] |> unique()

  # from the universe of all existing effects, select those that have been investigated:
  effects <- sim_env$effects[
    sim_env$effects[, "effect_id"] %in% investigated_effect_ids,
    ,
    drop = FALSE
  ]
  timesteps <- 0:sim_env$n_timesteps
  n_t <- length(timesteps)

  # Pre-allocate output
  out <- data.frame(
    timestep = timesteps,
    total_kl = numeric(n_t),
    mean_kl = numeric(n_t),
    n_effects_investigated = integer(n_t)
  )

  for (i in seq_along(timesteps)) {
    threshold <- timesteps[i]
    relevant <- effects[effects[, "timestep"] <= threshold, , drop = FALSE]

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
    out$n_effects_investigated[i] <- relevant[!is.na(relevant[, "study_id"]), "effect_id"] |> unique() |> length()
  }
  out
}
