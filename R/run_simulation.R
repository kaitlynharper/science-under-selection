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
  n_timesteps = 300, # number of timesteps
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
  publication_bias = 2, # 0 = no publication bias, 1 = weak publication bias, 2 = strong publication bias
  set_nonsig_logistic_midpoint = NA, # for setting specific pub bias function when sweeping
  all_replications_published = 0 # 0 = normal publication bias, 1 = all replications published regardless of bias
)

if (exists("run_conditions")) {
  params$n_timesteps <- n_timesteps
  params$initial_selection_condition <- initial_selection_condition
  params$switch_conditions_at <- switch_conditions_at
  params$publication_bias <- publication_bias
  params$hold_samples_constant_at <- hold_samples_constant_at
  params$all_replications_published <- all_replications_published
}

if (exists("run_sweep")) {
  params$hold_samples_constant_at <- hold_samples_constant_at
  params$set_nonsig_logistic_midpoint <- nonsig_logistic_midpoint
  params$base_null_probability <- base_null_probability
}

# Run simulation and track memory and runtime
# profvis_profile <- profvis({
results <- run_simulation(params)
# })

# Memory usage of results
# cat("=== MEMORY USAGE ===\n")
# cat("Studies matrix:", object_size(results$studies), "bytes\n")
# cat("Agents matrix:", object_size(results$agents), "bytes\n")
# cat("Effects matrix:", object_size(results$effects), "bytes\n")
# Display model output
# View(results$agents)
# View(results$studies)
# View(results$effects)
# Open profvis profile
# print(profvis_profile)

##########
# Investigaring novelty contribution by p-value significance and study type
##########

# Clean studies and add significance flag
df <- results$studies |>
  as.data.frame() |>
  filter(!is.na(study_id)) |>
  mutate(sig = p_value < 0.05)

# Get original study significance by effect_id
orig_sig <- df |>
  filter(study_type == 0) |>
  select(effect_id, orig_sig = sig)

# Join and ensure originals stay NA for orig_sig
df <- df |>
  left_join(orig_sig, by = "effect_id") |>
  mutate(orig_sig = if_else(study_type == 0, NA, orig_sig))

# Full crossed summary
df |>
  group_by(study_type, sig, orig_sig) |>
  summarise(
    mean_novelty = mean(novelty_contribution, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )
