##############################################################################
# Set sweep parameters
#
# Description: Configures which parameters to sweep and their ranges (control
# panel + master spec). Source this before 04_run_sweep.R, or let 04_run_sweep.R
# source it automatically.
##############################################################################
library(here)
library(dplyr)
library(lhs)

# Source all functions once before parallel execution
function_files <- list.files(here("R", "functions"), full.names = TRUE)
sapply(function_files, source, .GlobalEnv)
source(here("R", "00_model.R"))

##############################################################################
#### CONTROL PANEL ####
##############################################################################

# Run settings
n_sims <- 10 # total number of simulations (param combinations from LHS)
n_cores <- parallel::detectCores() - 1
max_sweep_topups <- 3 # 1 = single pass; 2+ = re-run missing seeds from prior passes

# Which parameters to sweep (names must be in sweepable_params below)
sweep_param_names <- c(
  "hold_samples_constant_at",
  "nonsig_logistic_midpoint",
  "base_null_probability"
)

##############################################################################
#### BASE PARAMETERS ####
##############################################################################
base_params <- list(
  # Agents and study design
  n_agents = 1000, # number of agents
  n_timesteps = 350, # number of timesteps
  n_timesteps_per_career_step = 35, # number of timesteps per career phase
  duration_per_observation = 0.1, # timesteps per observation
  duration_original_intercept = 1, # base timesteps for original studies
  # True effects
  n_effects = 500000, # number of effects
  base_null_probability = 0.9, # base probability of a null effect
  effect_size_mean = 0.3, # mean effect size
  effect_size_variance = 0.1, # standard deviation of the distribution of effect sizes
  # Collective belief updating
  uninformed_prior_mean = 0, # mean of uninformed prior
  uninformed_prior_variance = 1, # variance of uninformed prior
  # Career turnover
  initial_selection_condition = 0, # 0 = selection based on truth, 1 = selection based on novelty
  switch_conditions_at = NA, # if NA, no switch; if a value, condition switches at that timestep
  career_turnover_selection_rate = 0.5, # proportion of agents to retire each career step 0 = NO EVOLUTION, 0.5 = standard evolution
  innovation_sd = 0, # standard deviation of innovation noise added to new agents
  mutation_rate = 0.1, # probability that a new agent's replication_probability flips (0->1 or 1->0)
  initial_replication_rate = 0, # initial proportion of agents who are replicators (0 or 1)
  # Study design and publication
  hold_samples_constant_at = 50, # base sample size for all studies (originals always use this)
  replications_dynamic_sample_sizes = 1, # 0 = replications use hold_samples_constant_at, 1 = replications use 80% power of original effect, or 0.3 (if original non-sig)
  publication_bias = 1, # 0 = no publication bias, 1 = publication bias on
  sig_lower_asymptote = 0, # minimum publication probability for p < .05 results
  sig_logistic_midpoint = -0.5, # novelty midpoint for significant results
  sig_logistic_steepness = 3, # steepness of logistic curve for significant results
  nonsig_logistic_midpoint = 3, # novelty midpoint for non-significant results (swept)
  nonsig_logistic_steepness = 3, # steepness of logistic curve for non-significant results
  all_replications_published = 0, # 0 = normal publication bias, 1 = all replications published regardless of bias
  burn_in_period = 100, # During burn-in, all agents run original studies only and no mutation
  # TEMP: testing savage-dickey method
  truth_contribution_method = "kl" # "kl" or "savage_dickey" (study-level and effect-level total_scientific_progress)
)

##############################################################################
#### SWEEPABLE PARAMETERS ####
# Every parameter that can be swept: min, max, label, color; optional log_scale.
##############################################################################
sweepable_params <- list(
  hold_samples_constant_at = list(
    min = 10,
    max = 200,
    label = "Sample Size",
    color = "#E69F00",
    log_scale = TRUE # log-uniform for better coverage of small values
  ),
  nonsig_logistic_midpoint = list(
    min = -0.5,
    max = 3,
    label = "Publication Bias",
    color = "#56B4E9"
  ),
  base_null_probability = list(
    min = 0,
    max = 1,
    label = "Base Null Probability",
    color = "#009E73"
  ),
  initial_replication_rate = list(
    min = 0,
    max = 1,
    label = "Initial Replication Rate",
    color = "#D55E00"
  ),
  uninformed_prior_variance = list(
    min = 0.1,
    max = 2,
    label = "Prior Variance",
    color = "#CC79A7"
  ),
  effect_size_mean = list(
    min = 0.1,
    max = 0.5,
    label = "Effect Size Mean",
    color = "#0072B2"
  ),
  career_turnover_selection_rate = list(
    min = 0.3,
    max = 0.7,
    label = "Selection Rate",
    color = "#F0E442"
  ),
  mutation_rate = list(
    min = 1e-4,
    max = 0.1,
    label = "Mutation Rate",
    color = "#999999",
    log_scale = TRUE
  )
)

# Active sweep config (only the parameters being swept this run)
stopifnot(all(sweep_param_names %in% names(sweepable_params)))
param_config <- sweepable_params[sweep_param_names]

##############################################################################
#### LATIN HYPERCUBE SAMPLING FUNCTION ####
##############################################################################
draw_params <- function(n = 1) {
  k <- length(param_config)
  h <- randomLHS(n, k)
  df <- data.frame(row.names = seq_len(n))
  for (i in seq_len(k)) {
    nm <- names(param_config)[i]
    spec <- param_config[[i]]
    a <- spec$min
    b <- spec$max
    if (isTRUE(spec$log_scale)) {
      val <- exp(qunif(h[, i], log(a), log(b)))
      if (nm == "hold_samples_constant_at") {
        val <- round(val)
      }
      df[[nm]] <- val
    } else {
      df[[nm]] <- qunif(h[, i], a, b)
    }
  }
  df
}

# Draw parameters for each simulation
sweep_params <- draw_params(n_sims)
sweep_params$seed <- seq_len(n_sims)
sweep_params_full <- sweep_params
