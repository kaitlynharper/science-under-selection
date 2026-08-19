##############################################################################
# Run simulation
#
# Description: This file sources the necessary files and provides code to run
# academiABM2 simulations
##############################################################################

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

# Set seed for reproducibility
# set.seed(123)

# Define simulation parameters
params <- list(
  # Parameters for agents and study design
  n_agents = 1000, # number of agents
  n_timesteps = 500, # number of timesteps
  n_timesteps_per_career_step = 35, # number of timesteps per career phase
  duration_per_observation = 0.1, # TODO calibration required # timesteps per observations
  duration_original_intercept = 1, # TODO calibration required # base timesteps for original studies

  # Parameters for true effects
  n_effects = 500000, # number of effects
  base_null_probability = .9, # base probability of a null effect
  effect_size_mean = .3, # mean effect size
  effect_size_variance = 0.1, # variance of effect sizes

  # Parameters for collective belief updating
  uninformed_prior_mean = 0, # mean of uninformed prior
  uninformed_prior_variance = 1, # variance of uninformed prior

  # Career turnover parameters
  initial_selection_condition = 0, # 0 = selection based on truth, 1 = selection based on novelty
  switch_conditions_at = NA, # if NA, no switch; if a value, condition switches at that timestep
  career_turnover_selection_rate = 0.5, # proportion of agents to retire each career step
  innovation_sd = 0, # standard deviation of innovation noise added to new agents
  mutation_rate = 0.1, # probability that a new agent's replication_probability flips (0->1 or 1->0)
  initial_replication_rate = 0.5, # initial proportion of agents who are replicators (0 or 1)
  hold_samples_constant_at = 50, # base sample size for all studies (originals always use this)
  replications_dynamic_sample_sizes = 1, # 0 = replications use hold_samples_constant_at, 1 = replications use 80% power of original effect, or 0.3 (if original non-sig)
  publication_bias = 1, # 0 = no publication bias, 1 = publication bias on
  sig_lower_asymptote = 0, # minimum publication probability for p < .05 results
  sig_logistic_midpoint = -0.5, # novelty midpoint for significant results
  sig_logistic_steepness = 3, # steepness of logistic curve for significant results
  nonsig_logistic_midpoint = 3, # novelty midpoint for non-significant results (swept)
  nonsig_logistic_steepness = 3, # steepness of logistic curve for non-significant results
  all_replications_published = 0, # 0 = normal publication bias, 1 = all replications published regardless of bias

  # Additional parameters
  burn_in_period = 100, # During burn-in, all agents run original studies only and no mutation (original-> replication or v.v.) happens
  # TEMP: testing savage-dickey method
  truth_contribution_method = "savage_dickey" # "kl" or "savage_dickey" (study-level and, in sweep, effect-level total progress)
)

# Run simulation (edit params above for a single run; use run_sweep_parallel.R for parameter sweeps)
# profvis_profile <- profvis({
results <- run_simulation(params)
# })
