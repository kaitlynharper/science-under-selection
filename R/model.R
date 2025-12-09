##############################################################################
# Model
#
# Description: This script establishes the run_simulation function
##############################################################################

# Simulation function
#' @param verbose Integer controlling verbosity level (0 = silent, 1 = only timestep, 2 = all intermediate functions).
run_simulation <- function(params, verbose=1) {
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
    replication_probabilities = rbinom(sim_env$n_agents, 1, 0.5),
    target_powers = runif(sim_env$n_agents, 0, 0.99),
    timestep_next_papers = rep(0, sim_env$n_agents)
  )

  #### Timestep loop ####
  for (timestep in 0:sim_env$n_timesteps) {
    # Update timestep tracker in the environment
    # (R doesn't like to use an environment variable as a loop index variable)
    sim_env$timestep <- timestep

    if (verbose > 0) print(paste0("Timestep ", timestep))

    # Run actual studies
    run_studies(sim_env, verbose=verbose > 1)

    # Career turnover phase (skip first career step)
    if (
      sim_env$timestep %% sim_env$n_timesteps_per_career_step == 0 &&
        sim_env$timestep > 0
    ) {
      if (!is.na(sim_env$switch_conditions_at)) {
        if (sim_env$switch_conditions_at == sim_env$timestep) {
          sim_env$selection_condition <- 1 - sim_env$selection_condition
        }
      }
      career_turnover(sim_env, verbose = verbose > 1)
    }
  }

  # Return the env
  return(sim_env)
}
