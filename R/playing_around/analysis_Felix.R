library(dplyr)
library(ggplot2)
library(patchwork)

# make local copies of results components for faster access
agents <- as.data.frame(results$agents) |> filter(!is.na(researcher_id))
studies <- as.data.frame(results$studies) |> filter(!is.na(study_id))
effects <- results$effects |> as.data.frame() |> filter(!is.na(effect_id))

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

  studies_in_last_period <- studies
    
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



# Analyze studies
#-------------------------------------------------------

study_type_count_per_effect <- studies |> group_by(effect_id) |> 
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
S2 <- left_join(studies, effects_reduced, by="effect_id")

S2$study_type_label <- factor(S2$study_type, levels=c(0, 1), labels=c("original", "replication"))
S2$true_delta <- factor(S2$true_effect_size == 0, levels=c(TRUE, FALSE), labels=c("delta=0", "delta>0"))
S2$publication_status_label <- factor(S2$publication_status, levels=c(0, 1), labels=c("unpublished", "published"))

highest_original_novelty <- max(S2$novelty_contribution[S2$study_type == 0], na.rm=TRUE)
S2 <- S2 |>
  mutate(higher_repl_novelty = novelty_contribution > highest_original_novelty)


ggplot(S2, aes(
  x=novelty_contribution, y=truth_contribution, 
  color=study_type_label  
)) +
  geom_point(aes(shape = higher_repl_novelty), alpha=0.3) +
  geom_smooth(method="lm", se=FALSE, color="grey60") +
  facet_grid(true_delta~study_type_label~publication_status_label) +
  labs(title = TITLE, color="Study Type")

print(paste0("% of replication studies with higher novelty score than the highest original study: ", (sum(S2$higher_repl_novelty & S2$study_type == 1, na.rm=TRUE) / sum(S2$study_type == 1, na.rm=TRUE) * 100) |> round(2), "%"))


## Truth contribution of different study types
## ----------------------------------------------------------------------
## TODO: Dinstinguish replication of sig. originals vs. replications of non-sig. originals

S2 |> 
  group_by(type=study_type_label, true_delta, pub=publication_status_label) |>
  summarise(
    mean_estimate = mean(estimated_mean, na.rm=TRUE),
    mean_abs_estimate = mean(abs(estimated_mean), na.rm=TRUE),   # take the absolute estimate so that positive and negative effects don't cancel out
    mean_truth_contrib = mean(truth_contribution, na.rm=TRUE),
    mean_novel_contrib = mean(novelty_contribution, na.rm=TRUE),    
    k_studies = n()
  ) |>
  arrange(-mean_truth_contrib)



# # show histogram of line 7: 
# S2 |> filter(study_type == 1, true_effect_size==0, publication_status==1) |>
#   pull("estimated_mean") |> hist(main="Published original studies of a true null", xlab="")
