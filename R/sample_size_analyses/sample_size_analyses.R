##############################################################################
# Sample size analyses
#
# Description: Exploration of ideal sample size for truthful information gain.
# Sourced by sample_size_analyses.qmd; assigns named ggplot objects.
#
# Output folder:
#   R/sample_size_analyses/output/
# Cached files (delete one to re-run that analysis):
#   bayes_df.rds, results_no_pb.rds, results_pb.rds
##############################################################################

library(here)
library(dplyr)
library(ggplot2)

# Model functions (includes kl_norm and the conjugate update used below)
function_files <- list.files(here("R", "functions"), full.names = TRUE)
sapply(function_files, source, .GlobalEnv)
source(here("R", "00_model.R"))

# Header knobs
n_reps <- 20
n_timesteps <- 1000
n_agents <- 1000
n_cores <- parallel::detectCores() - 1
max_sweep_topups <- 1

sample_sizes <- c(2:100, 125, 150, 200)
pb_levels <- seq(-0.5, 5, by = 0.5)

# Shared ABM settings: no evolution, no burn-in (burn-in would force PB on),
# duration = 1 per observation so study length is not rounded into a sawtooth
base_params <- list(
  n_agents = n_agents,
  n_timesteps = n_timesteps,
  n_timesteps_per_career_step = 35,
  duration_per_observation = 1,
  duration_original_intercept = 1,
  n_effects = 500000,
  base_null_probability = 0.9,
  effect_size_mean = 0.3,
  effect_size_variance = 0.1,
  uninformed_prior_mean = 0,
  uninformed_prior_variance = 1,
  initial_selection_condition = 0,
  switch_conditions_at = NA,
  career_turnover_selection_rate = 0,
  innovation_sd = 0,
  mutation_rate = 0,
  initial_replication_rate = 0,
  hold_samples_constant_at = 50,
  replications_dynamic_sample_sizes = 0,
  publication_bias = 0,
  sig_lower_asymptote = 0,
  sig_logistic_midpoint = -0.5,
  sig_logistic_steepness = 3,
  nonsig_logistic_midpoint = 3,
  nonsig_logistic_steepness = 3,
  all_replications_published = 0,
  burn_in_period = 0,
  truth_contribution_method = "savage_dickey"
)

# Cached results live here. Delete an rds file to re-run that analysis.
out_dir <- here("R", "sample_size_analyses", "output")
dir.create(out_dir, showWarnings = FALSE)

# Load a cached sweep, or run 04_run_sweep.R and save.
load_or_run_sweep <- function(path) {
  if (file.exists(path)) {
    message("Loading existing results: ", path)
    return(readRDS(path))
  }
  source(here("R", "04_run_sweep.R"), local = FALSE)
  saveRDS(sweep_results, path)
  file.remove(here(sweep_path)) # drop the timestamped copy in output/
  message("Saved results to ", path)
  sweep_results
}

##############################################################################
# Pure Bayesian model
##############################################################################

# Conjugate normal-normal update, same formulas as prepare_bayesian_data().
# Prior N(0, 1); true effect 0.3 (ABM effect_size_mean).
# Likelihood is Cohen's d with n per group, matching generate_study_results().
bayes_path <- file.path(out_dir, "bayes_df.rds")
if (file.exists(bayes_path)) {
  message("Loading existing results: ", bayes_path)
  bayes_df <- readRDS(bayes_path)
} else {
  set.seed(1)
  n_seq <- 1:200
  true_d <- 0.3
  n_draws <- 10000 # Monte Carlo draws per n (mean IG, then plot the differences)
  prior_mean <- 0
  prior_sd <- 1

  expected_ig <- sapply(n_seq, function(n) {
    # Monte Carlo expected KL(posterior || prior) at this n
    se_true <- sqrt(2 / n + true_d^2 / (4 * n))
    d_obs <- rnorm(n_draws, true_d, se_true)
    se_obs <- sqrt(2 / n + d_obs^2 / (4 * n))
    lik_var <- se_obs^2
    post_var <- 1 / (1 / prior_sd^2 + 1 / lik_var)
    post_mean <- (prior_mean / prior_sd^2 + d_obs / lik_var) * post_var
    mean(kl_norm(post_mean, sqrt(post_var), prior_mean, prior_sd))
  })

  # Marginal = extra IG from one more observation per group
  bayes_df <- data.frame(
    sample_size = n_seq,
    expected_ig = expected_ig,
    marginal_ig = c(NA, diff(expected_ig))
  )
  saveRDS(bayes_df, bayes_path)
  message("Saved results to ", bayes_path)
}

fig_bayesian_marginal <- ggplot(
  bayes_df[!is.na(bayes_df$marginal_ig), ],
  aes(x = sample_size, y = marginal_ig)
) +
  geom_line() +
  labs(x = "Sample size (n per group)", y = "Marginal information gain") +
  theme_classic()

##############################################################################
# Simplified agent-based model
##############################################################################

# Sweep sample size only; all studies published (publication_bias = 0)
sweep_param_names <- "hold_samples_constant_at"
param_config <- list(
  hold_samples_constant_at = list(
    min = min(sample_sizes),
    max = max(sample_sizes),
    label = "Sample size",
    color = "#E69F00"
  )
)
sweep_params <- data.frame(
  hold_samples_constant_at = rep(sample_sizes, each = n_reps)
)
sweep_params$seed <- seq_len(nrow(sweep_params))
sweep_params_full <- sweep_params
n_sims <- nrow(sweep_params)

base_params$publication_bias <- 0
results_no_pb <- load_or_run_sweep(file.path(out_dir, "results_no_pb.rds"))

no_pb_summary <- results_no_pb |>
  group_by(hold_samples_constant_at) |>
  summarise(
    absolute_ig = mean(total_information_gain, na.rm = TRUE),
    .groups = "drop"
  )

fig_abm_no_pb <- ggplot(
  no_pb_summary,
  aes(x = hold_samples_constant_at, y = absolute_ig)
) +
  geom_line() +
  geom_point() +
  labs(x = "Sample size", y = "Absolute information gain") +
  theme_classic()

##############################################################################
# Publication bias agent-based model
##############################################################################

# Same n grid, now with PB on, sweeping nonsig midpoint from -0.5 to 5
sweep_param_names <- c("hold_samples_constant_at", "nonsig_logistic_midpoint")
param_config <- list(
  hold_samples_constant_at = list(
    min = min(sample_sizes),
    max = max(sample_sizes),
    label = "Sample size",
    color = "#E69F00"
  ),
  nonsig_logistic_midpoint = list(
    min = min(pb_levels),
    max = max(pb_levels),
    label = "Publication bias",
    color = "#56B4E9"
  )
)
sweep_params <- expand.grid(
  hold_samples_constant_at = sample_sizes,
  nonsig_logistic_midpoint = pb_levels,
  rep = seq_len(n_reps)
)[, c("hold_samples_constant_at", "nonsig_logistic_midpoint")]
sweep_params$seed <- seq_len(nrow(sweep_params))
sweep_params_full <- sweep_params
n_sims <- nrow(sweep_params)

base_params$publication_bias <- 1
results_pb <- load_or_run_sweep(file.path(out_dir, "results_pb.rds"))

pb_summary <- results_pb |>
  group_by(hold_samples_constant_at, nonsig_logistic_midpoint) |>
  summarise(
    absolute_ig = mean(total_information_gain, na.rm = TRUE),
    truthful_ig = mean(total_scientific_progress, na.rm = TRUE),
    .groups = "drop"
  )

# Midpoint = 3 is the usual "strong PB" setting from the original model
fig_abm_pb_midpoint3 <- ggplot(
  filter(pb_summary, nonsig_logistic_midpoint == 3),
  aes(x = hold_samples_constant_at, y = absolute_ig)
) +
  geom_line() +
  geom_point() +
  labs(x = "Sample size", y = "Absolute information gain") +
  theme_classic()

fig_abm_pb_absolute <- ggplot(
  pb_summary,
  aes(
    x = hold_samples_constant_at,
    y = absolute_ig,
    colour = nonsig_logistic_midpoint,
    group = nonsig_logistic_midpoint
  )
) +
  geom_line() +
  scale_colour_viridis_c(name = "Publication bias") +
  labs(x = "Sample size", y = "Absolute information gain") +
  theme_classic()

fig_abm_pb_truthful <- ggplot(
  pb_summary,
  aes(
    x = hold_samples_constant_at,
    y = truthful_ig,
    colour = nonsig_logistic_midpoint,
    group = nonsig_logistic_midpoint
  )
) +
  geom_line() +
  scale_colour_viridis_c(name = "Publication bias") +
  labs(x = "Sample size", y = "Truthful information gain") +
  theme_classic()
