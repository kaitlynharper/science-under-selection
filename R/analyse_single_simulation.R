library(dplyr)
library(ggplot2)
library(scales)
library(patchwork)

# # Read results if not using already run simulation
# results <- readRDS(file="R/sim_results/felix_medium_PB_plot.RDS")

# make local copies of results components for faster access
agents <- as.data.frame(results$agents) |> filter(!is.na(researcher_id))
studies <- as.data.frame(results$studies) |> filter(!is.na(study_id))
effects <- results$effects |> as.data.frame() |> filter(!is.na(effect_id))

# At these time steps the career agent selection took place:
career_steps <- seq(
  0,
  results$n_timesteps,
  by = results$n_timesteps_per_career_step
)


# -------------------------------------------------------
# get agent traits

agent_traits <- data.frame()
for (i in career_steps) {
  # select agents that started before or at that timestep and are not "retired" yet:
  active_agents <- agents |>
    filter(
      !is.na(timestep_active),
      timestep_active <= i,
      timestep_inactive > i | is.na(timestep_inactive)
    )

  stopifnot(nrow(active_agents) == results$n_agents)

  studies_in_last_period <- studies

  agent_traits <- rbind(
    agent_traits,
    data.frame(
      timestep = i,
      avg_replication_prob = mean(active_agents$replication_probability),
      avg_power = mean(active_agents$target_power)
    )
  )
}


# -------------------------------------------------------
# get summaries of published studies

pub_studies <- studies |>
  filter(publication_status == 1) |>
  arrange(timestep_completed) |>
  mutate(
    truth_contribution_cumsum = cumsum(truth_contribution),
    career_phase = (floor(
      timestep_completed / results$n_timesteps_per_career_step
    ) +
      1) *
      results$n_timesteps_per_career_step
  )

pub_studies_binned <- pub_studies |>
  group_by(career_phase) |>
  summarise(
    truth_contribution_at_timestep = sum(truth_contribution)
  ) |>
  ungroup() |>
  mutate(timestep_completed = career_phase)


ggplot(
  pub_studies,
  aes(x = timestep_completed, y = truth_contribution_cumsum)
) +
  geom_line() +
  theme_minimal()

ggplot(
  pub_studies_binned,
  aes(x = career_phase, y = truth_contribution_at_timestep)
) +
  geom_line() +
  theme_minimal() +
  labs(
    y = "Truth contribution",
    x = "time",
    title = "Truth contribution of all publications in that career phase",
    subtitle = "0 = stagnation; positive values = truth gain; negative values = truth loss"
  )


# -------------------------------------------------------
# Total scientific progress over time
# Same definition as run_sweep.R, applied at each timestep:
# latest posterior among effects studied by that timestep, vs uninformed prior.

timesteps <- 0:results$n_timesteps
n_t <- length(timesteps)
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
  studied_effects <- relevant[is_latest_update & has_been_studied, , drop = FALSE]
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

res2 <- left_join(agent_traits, total_scientific_progress, by = "timestep")

res_long <- pivot_longer(
  res2,
  cols = c("avg_replication_prob", "avg_power"),
  names_to = "measure",
  values_to = "value"
)

p1 <- ggplot(
  res_long |> filter(measure == "avg_replication_prob"),
  aes(x = timestep, y = value, color = measure)
) +
  geom_line() +
  theme_minimal() +
  theme(legend.position = c(0.7, 0.15)) +
  guides(color = guide_legend(nrow = 1, byrow = TRUE, title = "")) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
  labs(y = "", x = "time", title = "Evolution of replicators")

p2 <- ggplot(
  pub_studies,
  aes(x = timestep_completed, y = truth_contribution_cumsum)
) +
  geom_line() +
  theme_minimal() +
  labs(
    y = "Cum. truth contrib.\nof published studies",
    title = "Knowledge gain, decrease,\nor stagnation?",
    x = "time",
  )


p3 <- ggplot(
  total_scientific_progress,
  aes(x = timestep, y = total_scientific_progress)
) +
  geom_line() +
  theme_minimal() +
  labs(
    y = "Total scientific progress",
    x = "time",
    title = "Cumulative scientific progress",
    subtitle = "Same measure as run_sweep.R, evaluated at each timestep"
  )


# add horizontal line at selection switch time if applicable
if (!is.na(results$switch_conditions_at)) {
  p1 <- p1 +
    geom_vline(
      xintercept = results$switch_conditions_at,
      linetype = "dashed",
      color = "grey60"
    )
  p2 <- p2 +
    geom_vline(
      xintercept = results$switch_conditions_at,
      linetype = "dashed",
      color = "grey60"
    )
  p3 <- p3 +
    geom_vline(
      xintercept = results$switch_conditions_at,
      linetype = "dashed",
      color = "grey60"
    )
}


# add burn-in annotation
#TODO: Implement dynamic burn-in graphing

burn_in <- list(
  annotate(
    "rect",
    xmin = 0,
    xmax = 300,
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

p1 <- p1 + burn_in
p2 <- p2 + burn_in
p3 <- p3 + burn_in

TITLE <- paste0(
  "n = ",
  results$hold_samples_constant_at,
  ", nonsig midpoint = ",
  results$nonsig_logistic_midpoint,
  ", selection on ",
  ifelse(
    is.na(results$switch_conditions_at),
    ifelse(results$initial_selection_condition == 0, "truth", "novelty"),
    ifelse(
      results$initial_selection_condition == 0,
      paste0("truth -> novelty at t=", results$switch_conditions_at),
      paste0("novelty -> truth at t=", results$switch_conditions_at)
    )
  ),
  ", a = ",
  results$n_agents,
  " agents"
)

#patchwork <- (p1 + p2) / (p3 + p4)
#patchwork <- (p1 / p2) | p3

patchwork <- p1 + p2
patchwork + plot_annotation(title = TITLE)


ggsave("R/plots/felix_medium_PB_plot_a1000.png", width = 10, height = 6)


# Analyze studies
#-------------------------------------------------------

study_type_count_per_effect <- studies |>
  group_by(effect_id) |>
  summarise(
    n_original_studies = sum(study_type == 0),
    n_replication_studies = sum(study_type == 1)
  )

table(study_type_count_per_effect$n_original_studies)
table(study_type_count_per_effect$n_replication_studies)

# Note: multiple original studies for one effect happen when the first original
# study was not published, and another agent then chose it again.

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
  labels = c("delta=0", "delta>0")
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
  facet_grid(true_delta ~ study_type_label ~ publication_status_label) +
  labs(title = TITLE, color = "Study Type")

print(paste0(
  "% of replication studies with higher novelty score than the highest original study: ",
  (sum(S2$higher_repl_novelty & S2$study_type == 1, na.rm = TRUE) /
    sum(S2$study_type == 1, na.rm = TRUE) *
    100) |>
    round(2),
  "%"
))


## Truth contribution of different study types
## ----------------------------------------------------------------------
## TODO: Dinstinguish replication of sig. originals vs. replications of non-sig. originals

S2 |>
  group_by(
    type = study_type_label,
    true_delta,
    pub = publication_status_label
  ) |>
  summarise(
    mean_estimate = mean(estimated_mean, na.rm = TRUE),
    mean_abs_estimate = mean(abs(estimated_mean), na.rm = TRUE), # take the absolute estimate so that positive and negative effects don't cancel out
    mean_truth_contrib = mean(truth_contribution, na.rm = TRUE),
    mean_novel_contrib = mean(novelty_contribution, na.rm = TRUE),
    k_studies = n()
  ) |>
  arrange(-mean_truth_contrib) |>
  print(n = Inf)


# # show histogram of line 7:
# S2 |> filter(study_type == 1, true_effect_size==0, publication_status==1) |>
#   pull("estimated_mean") |> hist(main="Published original studies of a true null", xlab="")

## FPR, PPV etc. of published literature

# sanity check: Of all conducted H0-studies, 5% should have a p-value < .05
nrow(S2 |> filter(true_effect_size == 0, p_value < 0.05)) /
  nrow(S2 |> filter(true_effect_size == 0))

# FPR (nominally should be 5%)
FPR <- nrow(
  S2 |> filter(true_effect_size == 0, p_value < 0.05, publication_status == 1)
) /
  nrow(S2 |> filter(true_effect_size == 0, publication_status == 1))
print(paste0("FPR: ", (FPR * 100) |> round(2), "%"))

# How many published studies are significant?
sig_published <- nrow(S2 |> filter(p_value < 0.05, publication_status == 1)) /
  nrow(S2 |> filter(publication_status == 1))
print(paste0(
  "% of published studies that are significant: ",
  (sig_published * 100) |> round(2),
  "%"
))


time_window <- 25

S2_binned <- S2 |>
  mutate(
    time_bin = cut(
      timestep_completed,
      breaks = seq(0, max(timestep_completed), by = time_window),
      include.lowest = TRUE,
      right = FALSE,
      ordered_result = TRUE
    ) |>
      as.numeric()
  ) |>
  group_by(time_bin) |>
  summarise(
    perc_original = 1 - mean(study_type),
    sum_KL = sum(truth_contribution)
  )

ggplot(S2_binned, aes(x = time_bin, y = perc_original)) +
  geom_point() +
  geom_line() +
  coord_cartesian(ylim = c(0, 1))

ggplot(S2_binned, aes(x = time_bin, y = sum_KL)) +
  geom_point() +
  geom_line() +
  labs(
    title = "Truth contribution in that bin",
    subtitle = "The lower, the higher the truth gain in that time window",
    y = "KL sum in bin"
  )
