##############################################################################
# Analyze a single simulation
#
# Description: Time series of replicator share and total scientific progress,
# plus a novelty vs truth contribution scatter for original and replication
# studies. Expects a `results` object in the environment (e.g. from
# 01_run_single_simulation.R).
##############################################################################

library(dplyr)
library(ggplot2)
library(patchwork)

agents <- as.data.frame(results$agents) |> filter(!is.na(researcher_id))
studies <- as.data.frame(results$studies) |> filter(!is.na(study_id))
effects <- results$effects |> as.data.frame() |> filter(!is.na(effect_id))

timesteps <- 0:results$n_timesteps
n_t <- length(timesteps)

# -------------------------------------------------------
# Replicator share (%) at each timestep
# Same quantity as 04_run_sweep.R mean_replication_rate, among agents active
# at that timestep.

replicator_share <- data.frame(
  timestep = timesteps,
  replicator_share = numeric(n_t)
)

for (i in seq_along(timesteps)) {
  t <- timesteps[i]
  active <- !is.na(agents$timestep_active) &
    agents$timestep_active <= t &
    (is.na(agents$timestep_inactive) | agents$timestep_inactive > t)
  replicator_share$replicator_share[i] <- 100 *
    mean(agents$replication_probability[active])
}

# -------------------------------------------------------
# Total scientific progress at each timestep
# Same definition as 04_run_sweep.R: latest posterior among effects studied
# by that timestep, vs uninformed prior.

prior_mean <- results$uninformed_prior_mean
prior_sd <- sqrt(results$uninformed_prior_variance)
effects_mat <- results$effects

total_scientific_progress <- data.frame(
  timestep = timesteps,
  total_scientific_progress = numeric(n_t)
)

for (i in seq_along(timesteps)) {
  t <- timesteps[i]
  relevant <- effects_mat[
    !is.na(effects_mat[, "effect_id"]) & effects_mat[, "timestep"] <= t,
    ,
    drop = FALSE
  ]
  if (nrow(relevant) == 0L) {
    next
  }

  is_latest_update <- !duplicated(relevant[, "effect_id"], fromLast = TRUE)
  has_been_studied <- !is.na(relevant[, "study_id"])
  studied_effects <- relevant[
    is_latest_update & has_been_studied,
    ,
    drop = FALSE
  ]
  if (nrow(studied_effects) == 0L) {
    next
  }

  true_mean <- studied_effects[, "true_effect_size"]
  true_sd <- sqrt(studied_effects[, "true_effect_variance"])
  posterior_mean <- studied_effects[, "posterior_effect_size"]
  posterior_sd <- sqrt(studied_effects[, "posterior_effect_variance"])

  if (results$truth_contribution_method == "savage_dickey") {
    log_prior_at_true <- stats::dnorm(
      true_mean,
      prior_mean,
      prior_sd,
      log = TRUE
    )
    log_posterior_at_true <- stats::dnorm(
      true_mean,
      posterior_mean,
      posterior_sd,
      log = TRUE
    )
    total_scientific_progress$total_scientific_progress[i] <- sum(
      log_posterior_at_true - log_prior_at_true
    )
  } else {
    baseline_kl <- kl_norm(true_mean, true_sd, prior_mean, prior_sd)
    current_kl <- kl_norm(true_mean, true_sd, posterior_mean, posterior_sd)
    total_scientific_progress$total_scientific_progress[i] <- sum(
      baseline_kl - current_kl
    )
  }
}

# -------------------------------------------------------
# Time markers (burn-in band and optional selection switch)

burn_in_end <- results$burn_in_period
time_markers <- list()
if (!is.na(burn_in_end) && burn_in_end > 0) {
  time_markers <- c(
    time_markers,
    list(
      annotate(
        "rect",
        xmin = 0,
        xmax = burn_in_end,
        ymin = -Inf,
        ymax = Inf,
        alpha = 0.1,
        fill = "blue"
      ),
      annotate(
        "text",
        x = 5,
        y = Inf,
        label = "Burn-in period",
        hjust = 1.1,
        vjust = 1.2,
        angle = 90,
        fontface = "italic",
        size = 3.5
      )
    )
  )
}
if (!is.na(results$switch_conditions_at)) {
  time_markers <- c(
    time_markers,
    list(
      geom_vline(
        xintercept = results$switch_conditions_at,
        linetype = "dashed",
        color = "grey60"
      )
    )
  )
}

fig_replicator_share <- ggplot(
  replicator_share,
  aes(x = timestep, y = replicator_share)
) +
  geom_line(linewidth = 1.2) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(x = "Timestep", y = "Replicator share (%)") +
  theme_classic() +
  time_markers

fig_scientific_progress <- ggplot(
  total_scientific_progress,
  aes(x = timestep, y = total_scientific_progress)
) +
  geom_line(linewidth = 1.2) +
  labs(x = "Timestep", y = "Total scientific progress") +
  theme_classic() +
  time_markers

fig_timeseries <- (fig_replicator_share / fig_scientific_progress)
print(fig_timeseries)

# -------------------------------------------------------
# Novelty vs truth contribution (originals vs replications)

# as effects are stored multiple times, we reduce it to a single row per effect
# (we just want the true effect size, not the evolution)
effects_reduced <- effects |> select(effect_id, true_effect_size) |> distinct()
S2 <- left_join(studies, effects_reduced, by = "effect_id")

S2$study_type_label <- factor(
  S2$study_type,
  levels = c(0, 1),
  labels = c("original", "replication")
)
S2$true_delta <- factor(
  S2$true_effect_size == 0,
  levels = c(TRUE, FALSE),
  labels = c("true effect = 0", "true effect > 0")
)
S2$publication_status_label <- factor(
  S2$publication_status,
  levels = c(0, 1),
  labels = c("unpublished", "published")
)

highest_original_novelty <- max(
  S2$novelty_contribution[S2$study_type == 0],
  na.rm = TRUE
)
S2 <- S2 |>
  mutate(higher_repl_novelty = novelty_contribution > highest_original_novelty)

print(
  ggplot(
    S2,
    aes(
      x = novelty_contribution,
      y = truth_contribution,
      color = study_type_label
    )
  ) +
    geom_point(aes(shape = higher_repl_novelty), alpha = 0.3) +
    geom_smooth(method = "lm", se = FALSE, color = "grey60") +
    facet_grid(true_delta ~ study_type_label + publication_status_label) +
    labs(
      x = "Novelty contribution",
      y = "Truth contribution",
      color = "Study Type",
      shape = "Novelty Higher for Replication"
    ) +
    theme(
      legend.position = "bottom",
      legend.box = "vertical",
      legend.title = element_text(size = rel(0.7)),
      legend.text = element_text(size = rel(0.7)),
      legend.margin = margin(0),
      legend.spacing.y = unit(1, "pt")
    )
)
