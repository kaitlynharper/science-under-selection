##############################################################################
# Model
#
# Description: This script establishes the run_simulation function
##############################################################################

# Simulation function
run_simulation <- function(params) {
  # Extract parameters dynamically
  for (param_name in names(params)) {
    assign(param_name, params[[param_name]])
  }
  
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
    
    # Run actual studies
    output <- run_studies(current_agents, effects, studies, timestep, next_study_id)

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
