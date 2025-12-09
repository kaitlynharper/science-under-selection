library(dplyr)
library(ggplot2)
library(patchwork)

# make local copies of results components for faster access
agents <- as.data.frame(results$agents) |> filter(!is.na(researcher_id))
studies <- as.data.frame(results$studies) |> filter(!is.na(study_id))

#### Extract belief accuracy over time ####
extract_belief_accuracy <- function(sim_env) {

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



# At these time steps the career agent selection took place:
career_steps <- seq(0, results$n_timesteps, by=results$n_timesteps_per_career_step)


# -------------------------------------------------------
# get agent traits

res <- data.frame()
for (i in career_steps) {
  
  # select agents that started before or at that timestep and are not "retired" yet:
  active_agents <- agents |>
    filter(!is.na(timestep_active), timestep_active <= i, timestep_inactive > i | is.na(timestep_inactive))

  stopifnot(nrow(active_agents) == results$n_agents)
    
  res <- rbind(res, data.frame(
    timestep = i,
    avg_replication_prob = mean(active_agents$replication_probability),
    avg_power = mean(active_agents$target_power)
  ))
  
}

# -------------------------------------------------------
# Quality of literature: Belief accuracy over time

belief <- extract_belief_accuracy(results)

res2 <- left_join(res, belief, by="timestep")

res_long <- pivot_longer(res2, cols = c("avg_replication_prob", "avg_power"),
                         names_to = "measure",
                         values_to = "value")

p1 <- ggplot(res_long, aes(x=timestep, y=value, color=measure)) +
  geom_point() +
  geom_line() +
  ylim(c(0, 1)) +
  theme(legend.position="bottom")

p2 <- ggplot(res2, aes(x=timestep, y=mean_kl)) +
  geom_point() +
  geom_line()

p3 <- ggplot(res2, aes(x=timestep, y=n_effects_investigated)) +
  geom_point() +
  geom_line() +
  labs(y="Cumulative number of effects published")

patchwork <- p1 + p2 + p3
patchwork + plot_annotation(
  title = "n = 200, no PB, selection on truth"
)