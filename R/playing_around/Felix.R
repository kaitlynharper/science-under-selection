library(dplyr)
library(ggplot2)

studies <- as.data.frame(results$studies)

truth_trajectory <- studies |> filter(researcher_id == 6) |> pull("truth_contribution")

# cumsum(truth_trajectory) |> plot()


agents <- as.data.frame(results$agents)

# At these time steps the career agent selection took place:
career_steps <- seq(0, results$n_timesteps, by=results$n_timesteps_per_career_step)

# look at all agents that are active in a time step
res <- data.frame()
for (i in career_steps) {
  
  # select agents that started before or at that timestep and are not "retired" yet:
  active_agents <- agents |>
    filter(!is.na(timestep_active), timestep_active <= i, timestep_inactive > i | is.na(timestep_inactive))

  stopifnot(nrow(active_agents) == results$n_agents)
    
  res <- rbind(res, data.frame(
    timestep = i,
    avg_replication_prob = mean(active_agents$replication_probability)
  ))
  
}

ggplot(res, aes(x=timestep, y=avg_replication_prob)) +
  geom_point() +
  geom_line() +
  ylim(c(0, 1))