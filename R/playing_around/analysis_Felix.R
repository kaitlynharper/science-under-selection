library(dplyr)
library(ggplot2)
library(patchwork)

# make local copies of results components for faster access
agents <- as.data.frame(results$agents) |> filter(!is.na(researcher_id))
studies <- as.data.frame(results$studies) |> filter(!is.na(study_id))


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

belief <- extract_belief_accuracy2(results)

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

# add horizontal line at selection switch time if applicable
if (!is.na(results$switch_conditions_at)) {
  p1 <- p1 + geom_vline(xintercept = results$switch_conditions_at, linetype="dashed", color="grey60")
  p2 <- p2 + geom_vline(xintercept = results$switch_conditions_at, linetype="dashed", color="grey60")
  p3 <- p3 + geom_vline(xintercept = results$switch_conditions_at, linetype="dashed", color="grey60")
}

TITLE <- paste0("n = ", results$hold_samples_constant_at, ", ",
                ifelse(results$publication_bias == 1, "with", "without"), " PB", 
                ", selection on ", 
                  ifelse(is.na(results$switch_conditions_at),
                    ifelse(results$initial_selection_condition == 0, "truth", "novelty"), 
                    ifelse(results$initial_selection_condition == 0, 
                      paste0("truth -> novelty at t=", results$switch_conditions_at),
                      paste0("novelty -> truth at t=", results$switch_conditions_at)
                    )
                  ),
                ", a = ", results$n_agents, " agents")

patchwork <- p1 + p2 + p3
patchwork + plot_annotation(title = TITLE)