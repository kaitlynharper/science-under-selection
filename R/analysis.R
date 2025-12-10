##############################################################################
# Analysis and Visualization
##############################################################################

library(ggplot2)
library(here)
library(rlang)

#### KL divergence between two normals (vectorized) ####
kl_norm <- function(mu0, sd0, mu1, sd1) {
  log(sd1 / sd0) + (sd0^2 + (mu0 - mu1)^2) / (2 * sd1^2) - 0.5
}

#### Extract agent traits over time ####
extract_agent_traits <- function(sim_env) {
  agents <- sim_env$agents[
    !is.na(sim_env$agents[, "researcher_id"]),
    ,
    drop = FALSE
  ]
  timesteps <- 0:sim_env$n_timesteps
  n_t <- length(timesteps)

  # Pre-allocate output
  out <- data.frame(
    timestep = timesteps,
    replication_probability_mean = numeric(n_t),
    replication_probability_sd = numeric(n_t),
    target_power_mean = numeric(n_t),
    target_power_sd = numeric(n_t)
  )

  for (i in seq_along(timesteps)) {
    t <- timesteps[i]
    active <- agents[, "timestep_active"] <= t #&
    # (is.na(agents[, "timestep_inactive"]) | agents[, "timestep_inactive"] > t)
    # comments mean I'm only showing new agents this timestep (not total)

    out$replication_probability_mean[i] <- mean(agents[
      active,
      "replication_probability"
    ])
    out$replication_probability_sd[i] <- sd(agents[
      active,
      "replication_probability"
    ])
    out$target_power_mean[i] <- mean(agents[active, "target_power"])
    out$target_power_sd[i] <- sd(agents[active, "target_power"])
  }
  out
}


#### Plot agent traits ####
plot_agent_traits <- function(trait_data, output_dir = NULL) {
  p <- ggplot(trait_data) +
    geom_ribbon(
      aes(
        x = .data$timestep,
        ymin = .data$replication_probability_mean -
          .data$replication_probability_sd,
        ymax = .data$replication_probability_mean +
          .data$replication_probability_sd
      ),
      alpha = 0.3,
      fill = "#4a90a4"
    ) +
    geom_line(
      aes(x = .data$timestep, y = .data$replication_probability_mean),
      color = "#2c5f6e",
      linewidth = 1
    ) +
    geom_ribbon(
      aes(
        x = .data$timestep,
        ymin = .data$target_power_mean - .data$target_power_sd,
        ymax = .data$target_power_mean + .data$target_power_sd
      ),
      alpha = 0.3,
      fill = "#a4784a"
    ) +
    geom_line(
      aes(x = .data$timestep, y = .data$target_power_mean),
      color = "#6e4a2c",
      linewidth = 1,
      linetype = "dashed"
    ) +
    labs(
      x = "Timestep",
      y = "Trait Value",
      title = "Agent Traits Over Time (mean ± SD)"
    ) +
    theme_minimal() +
    coord_cartesian(ylim = c(0, 1))

  if (!is.null(output_dir)) {
    ggsave(
      file.path(output_dir, "agent_traits.png"),
      p,
      width = 8,
      height = 5,
      dpi = 300
    )
  }
  p
}

#### Plot belief accuracy ####
plot_belief_accuracy <- function(belief_data, output_dir = NULL) {
  p <- ggplot(belief_data, aes(x = .data$timestep, y = .data$total_kl)) +
    geom_line(color = "#8b4a62", linewidth = 1) +
    labs(
      x = "Timestep",
      y = "Total KL Divergence",
      title = "Community Belief Accuracy (lower = more accurate)"
    ) +
    theme_minimal()

  if (!is.null(output_dir)) {
    ggsave(
      file.path(output_dir, "belief_accuracy.png"),
      p,
      width = 8,
      height = 5,
      dpi = 300
    )
  }
  p
}

#### Run analysis ####
analyze_simulation <- function(sim_env, output_dir = NULL) {
  trait_data <- extract_agent_traits(sim_env)
  belief_data <- extract_belief_accuracy(sim_env)

  trait_plot <- plot_agent_traits(trait_data, output_dir)
  belief_plot <- plot_belief_accuracy(belief_data, output_dir)

  list(
    trait_data = trait_data,
    belief_data = belief_data,
    trait_plot = trait_plot,
    belief_plot = belief_plot
  )
}

#### Run ####
analysis <- analyze_simulation(results, output_dir = here("output"))
