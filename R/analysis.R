##############################################################################
# Analysis and Visualization
##############################################################################

library(ggplot2)
library(here)
library(rlang)

#### KL divergence between two normals (vectorized) ####
kl_divergence_normal <- function(true_mean, true_sd, belief_mean, belief_sd) {
  log(belief_sd / true_sd) + (true_sd^2 + (true_mean - belief_mean)^2) / (2 * belief_sd^2) - 0.5
}

#### Extract agent traits over time ####
# agent_type: "active" = currently active agents, "new" = agents that joined this timestep
extract_agent_traits <- function(sim_env) {
  all_agents <- sim_env$agents[!is.na(sim_env$agents[, "researcher_id"]), , drop = FALSE]
  timesteps <- 0:sim_env$n_timesteps
  n_timesteps <- length(timesteps)
  
  # Pre-allocate output
  agent_traits <- data.frame(
    timestep = timesteps,
    replication_probability_mean = numeric(n_timesteps),
    replication_probability_sd = numeric(n_timesteps),
    target_power_mean = numeric(n_timesteps),
    target_power_sd = numeric(n_timesteps)
  )
  
  for (i in seq_along(timesteps)) {
    current_timestep <- timesteps[i]

      # Active agents: joined on or before current timestep AND (not yet inactive OR inactive after current)
      has_joined <- !is.na(all_agents[, "timestep_active"]) & 
                    all_agents[, "timestep_active"] <= current_timestep
      still_active <- is.na(all_agents[, "timestep_inactive"]) | 
                      all_agents[, "timestep_inactive"] > current_timestep
      is_selected <- has_joined & still_active
    
    if (sum(is_selected) == 0) {
      agent_traits$replication_probability_mean[i] <- NA
      agent_traits$replication_probability_sd[i] <- NA
      agent_traits$target_power_mean[i] <- NA
      agent_traits$target_power_sd[i] <- NA
    } else {
      agent_traits$replication_probability_mean[i] <- mean(all_agents[is_selected, "replication_probability"])
      agent_traits$replication_probability_sd[i] <- sd(all_agents[is_selected, "replication_probability"])
      agent_traits$target_power_mean[i] <- mean(all_agents[is_selected, "target_power"])
      agent_traits$target_power_sd[i] <- sd(all_agents[is_selected, "target_power"])
    }
  }
  agent_traits
}

#### Extract belief accuracy over time ####
extract_belief_accuracy <- function(sim_env) {
  all_effects <- sim_env$effects[!is.na(sim_env$effects[, "effect_id"]), , drop = FALSE]
  timesteps <- 0:sim_env$n_timesteps
  n_timesteps <- length(timesteps)
  
  # Pre-allocate output
  belief_accuracy <- data.frame(
    timestep = timesteps,
    total_kl = numeric(n_timesteps),
    mean_kl = numeric(n_timesteps)
  )
  
  for (i in seq_along(timesteps)) {
    current_timestep <- timesteps[i]
    effects_so_far <- all_effects[all_effects[, "timestep"] <= current_timestep, , drop = FALSE]
    
    if (nrow(effects_so_far) == 0) {
      belief_accuracy$total_kl[i] <- NA
      belief_accuracy$mean_kl[i] <- NA
      next
    }
    
    # Get latest row per effect using base R
    is_latest_effect <- !duplicated(effects_so_far[, "effect_id"], fromLast = TRUE)
    latest_effects <- effects_so_far[is_latest_effect, , drop = FALSE]
    
    kl_values <- kl_divergence_normal(
      true_mean = latest_effects[, "true_effect_size"],
      true_sd = sqrt(latest_effects[, "true_effect_variance"]),
      belief_mean = latest_effects[, "posterior_effect_size"],
      belief_sd = sqrt(latest_effects[, "posterior_effect_variance"])
    )
    
    belief_accuracy$total_kl[i] <- sum(kl_values)
    belief_accuracy$mean_kl[i] <- mean(kl_values)
  }
  belief_accuracy
}

#### Plot agent traits ####
plot_agent_traits <- function(agent_traits, output_dir = NULL) {
  plot <- ggplot(agent_traits, aes(x = .data$timestep)) +
    geom_line(aes(y = .data$replication_probability_mean, color = "Replication Probability"), 
              linewidth = 1) +
    geom_line(aes(y = .data$target_power_mean, color = "Target Power"), 
              linewidth = 1) +
    scale_color_manual(values = c("Replication Probability" = "#2c5f6e", 
                                   "Target Power" = "#6e4a2c")) +
    labs(x = "Timestep", y = "Mean Trait Value", 
         title = "Agent Traits Over Time",
         color = NULL) +
    theme_minimal() +
    coord_cartesian(ylim = c(0, 1)) +
    theme(legend.position = "bottom")
  
  if (!is.null(output_dir)) {
    ggsave(file.path(output_dir, "agent_traits.png"), plot, width = 8, height = 5, dpi = 300)
  }
  plot
}

#### Plot belief accuracy ####
plot_belief_accuracy <- function(belief_accuracy, output_dir = NULL) {
  plot <- ggplot(belief_accuracy, aes(x = .data$timestep, y = .data$total_kl)) +
    geom_line(color = "#8b4a62", linewidth = 1) +
    labs(x = "Timestep", y = "Total KL Divergence",
         title = "Community Belief Accuracy (lower = more accurate)") +
    theme_minimal()
  
  if (!is.null(output_dir)) {
    ggsave(file.path(output_dir, "belief_accuracy.png"), plot, width = 8, height = 5, dpi = 300)
  }
  plot
}

#### Run analysis ####
analyze_simulation <- function(sim_env, output_dir = NULL) {
  agent_traits <- extract_agent_traits(sim_env)
  belief_accuracy <- extract_belief_accuracy(sim_env)
  
  agent_traits_plot <- plot_agent_traits(agent_traits, output_dir)
  belief_accuracy_plot <- plot_belief_accuracy(belief_accuracy, output_dir)
  
  list(
    agent_traits = agent_traits,
    belief_accuracy = belief_accuracy,
    agent_traits_plot = agent_traits_plot,
    belief_accuracy_plot = belief_accuracy_plot
  )
}

#### Run ####
analysis <- analyze_simulation(results, output_dir = here("output"))
