##############################################################################
# Model
#
# Description: This script establishes the run_simulation function
##############################################################################

# Simulation function
run_simulation <- function(
  n_agents,
  n_timesteps,
  n_timesteps_per_career_step,
  n_effects,
  base_null_probability,
  effect_size_mean,
  effect_size_variance,
  uninformed_prior_mean,
  uninformed_prior_variance,
  mean_studies_per_agent_per_timestep
) {
  #### Initialize model ####

  # Initialize effects matrix
  effects <- initialize_effects_matrix(
    n_effects,
    base_null_probability,
    effect_size_mean,
    effect_size_variance,
    uninformed_prior_mean,
    uninformed_prior_variance
  )

  # Initialize empty studies matrix
  studies <- initialize_studies_matrix(
    n_agents,
    n_timesteps,
    mean_studies_per_agent_per_timestep
  )

  # Initialize empty agents matrix
  agents <- initialize_agents_matrix(
    n_agents,
    n_timesteps,
    n_timesteps_per_career_step
  )

  # Generate initial population of agents
  current_agents <- generate_initial_agents(
    n_agents
  )

  #### Timestep loop ####
  for (timestep in 0:n_timesteps) {
    # Update timestep for current_agents
    current_agents[, "timestep"] <- timestep

    # Save current agent info for easy access later
    current_n_agents <- nrow(current_agents)
    current_researcher_ids <- current_agents[, "researcher_id"]

    # Decide how many studies for each agent using Poisson distribution
    studies_per_agent <- rpois(
      current_n_agents,
      mean_studies_per_agent_per_timestep
    )
    # Calculate total number of studies
    total_studies_this_timestep <- sum(studies_per_agent)

    # Generate list of author ids
    researcher_ids <- rep(current_researcher_ids, studies_per_agent)

    # Generate timestep list
    timesteps_completed <- rep(timestep, total_studies_this_timestep)

    # Generate study ids
    study_ids <- next_study_id:(next_study_id + total_studies_this_timestep - 1)

    # Sample effect ids
    effect_ids <- sample(
      1:100000,
      total_studies_this_timestep,
      replace = FALSE
    )

    # Generate sample size values
    sample_sizes <- rpois(
      total_studies_this_timestep,
      100
    )

    # Generate effect size values
    estimated_means <- rnorm(
      total_studies_this_timestep,
      0.5,
      0.2
    )

    # Generate SE values
    estimated_standard_errors <- abs(rnorm(
      total_studies_this_timestep,
      0.1,
      0.05
    ))

    # Generate p-values
    p_values <- runif(
      total_studies_this_timestep,
      0,
      1
    )

    # Generate novelty contribution
    novelty_contributions <- rnorm(
      total_studies_this_timestep,
      0.2,
      0.1
    )

    # Generate truth contribution
    truth_contributions <- rnorm(
      total_studies_this_timestep,
      0.3,
      0.1
    )

    # Set publication status (0 = not published, 1 = published)
    publication_statuses <- rep(0, total_studies_this_timestep)

    # Add to matrix
    studies[
      next_study_id:(next_study_id + total_studies_this_timestep - 1),
    ] <-
      cbind(
        study_ids,
        researcher_ids,
        effect_ids,
        timesteps_completed,
        sample_sizes,
        estimated_means,
        estimated_standard_errors,
        p_values,
        novelty_contributions,
        truth_contributions,
        publication_statuses
      )
    # Update next study id tracker
    next_study_id <- next_study_id + total_studies_this_timestep

    # Career turnover phase
    if (timestep %% n_timesteps_per_career_step == 0) {
      # Save current agents to the agents matrix
      agents[
        next_agent_index:(next_agent_index + current_n_agents - 1),
      ] <- current_agents
      # Update agents matrix index tracker
      next_agent_index <- next_agent_index + current_n_agents
      # Retire set % of agents based on novelty or truth contributions
      # Generate new agents to fill lowest level (sample existing trait values + noise)
      # ^ Remember to update next_agent_id when doing this
    }
  }

  # Return list of results (eventually will also output effects matrix)
  return(list(
    studies = studies,
    agents = agents
  ))
}

# ### Option to run model within script for easier debugging ####
# library(here)
# # Source function files
# function_files = list.files(here("R", "functions"), full.names = TRUE)
# sapply(function_files, source, .GlobalEnv)
# results <- run_simulation(
#   n_agents = 5,
#   n_timesteps = 20,
#   n_timesteps_per_career_step = 10,
#   mean_studies_per_agent_per_timestep = 2
# )
# View(results$agents)
# View(results$studies)
