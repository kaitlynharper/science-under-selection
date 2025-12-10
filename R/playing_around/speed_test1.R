
# Load required packages
library(here) # relative paths
library(pryr) # For memory usage tracking
library(profvis) # For performance profiling
library(tidyr)
library(dplyr)

# Source model files (incl all function files)
function_files <- list.files(here("R", "functions"), full.names = TRUE)
sapply(function_files, source, .GlobalEnv)
source(here("R", "model.R"))

# Define simulation parameters
params <- list(
  n_agents = 1000, # number of agents
  n_timesteps = 200, # number of timesteps
  n_timesteps_per_career_step = 5, # number of timesteps per career phase
  n_effects = 20000, # number of effects
  base_null_probability = .5, # base probability of a null effect
  effect_size_mean = .8, # mean effect size
  effect_size_variance = 0.1, # variance of effect sizes
  uninformed_prior_mean = 0, # mean of uninformed prior
  uninformed_prior_variance = 1, # variance of uninformed prior
  duration_per_observation = 0.1, # TODO calibration required # timesteps per observations
  duration_original_intercept = 1, # TODO calibration required # base timesteps for original studies
  # Publication bias parameters # TODO calibration required
  sig_y_intercept = 0.2, # minimum publication probability for p < .05 results
  sig_logistic_midpoint = .5, # novelty midpoint for significant results
  sig_logistic_steepness = 3, # steepness of logistic curve for significant results
  nonsig_logistic_midpoint = .5, # novelty midpoint for non-significant results
  nonsig_logistic_steepness = 3, # steepness of logistic curve for non-significant results
  # Career turnover parameters
  initial_selection_condition = 0, # 0 = selection based on truth, 1 = selection based on novelty
  career_turnover_selection_rate = 0.5, # proportion of agents to retire each career step
  innovation_sd = 0.05, # standard deviation of innovation noise added to new agents
  hold_samples_constant_at = 100, # if NA, sample sizes are calculated; if a value, all studies use that sample size
  publication_bias = 0 #0 = no publication bias, 1 = publication bias
)


# Create simulation environment
  sim_env <- new.env()
  
  # TODO consider keeping the params bundled for easy export
  # Store parameters in environment
  for (param_name in names(params)) {
    sim_env[[param_name]] <- params[[param_name]]
  }
  
  #### Initialize model ####

  # Initialize effects matrix
  initialize_effects_matrix(sim_env)

  # Initialize empty studies matrix
  initialize_studies_matrix(sim_env)

  # Initialize empty agents matrix
  initialize_agents_matrix(sim_env)

  # Initialize timestep tracker
  sim_env$timestep <- 0

  # Generate initial population of agents
  add_agents(
    sim_env = sim_env,
    n_agents = sim_env$n_agents,
    timestep_active = 0,
    #replication_probabilities = runif(sim_env$n_agents, 0, 1),
    replication_probabilities = sample(c(0,1), sim_env$n_agents, replace = T),
    target_powers = runif(sim_env$n_agents, 0, 1),
    timestep_next_papers = rep(0, sim_env$n_agents)
  )

  # #### Timestep loop ####
  # for (timestep in 0:sim_env$n_timesteps) {
    
    # Update timestep tracker in the environment
    # (R doesn't like to use an environment variable as a loop index variable)
    sim_env$timestep <- timestep
    
    print(paste0("Timestep ", timestep))
    
    # Run actual studies
    run_studies(sim_env)

    system.time(
      for (i in 1:10) career_turnover(sim_env)
    )
    
    system.time(
      for (i in 1:10) career_turnover2(sim_env)
    )

  # }
