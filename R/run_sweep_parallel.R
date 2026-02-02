##############################################################################
# Run sweep (parallel version)
#
# Description: This file sets distributions for each parameter to sweep, runs the
# simulations across many combinations, then plots the resulting replication rate
# as partial dependency plots (isolating the effect of each parameter averaged
# across all others)
##############################################################################
library(here)
library(dplyr)
library(ggplot2)
library(patchwork)
library(lhs)
library(foreach)
library(doSNOW)

# Source all functions once before parallel execution
function_files <- list.files(here("R", "functions"), full.names = TRUE)
sapply(function_files, source, .GlobalEnv)
source(here("R", "model.R"))

#### SETTING PARAMETER DISTRIBUTIONS ####
# define parameter ranges (used for sampling and plotting)
param_config <- list(
  hold_samples_constant_at = list(
    min = 10,
    max = 200,
    label = "Sample Size",
    color = "#E69F00"
  ),
  nonsig_logistic_midpoint = list(
    min = -0.5,
    max = 5,
    label = "Publication Bias",
    color = "#56B4E9"
  ),
  base_null_probability = list(
    min = 0,
    max = 1,
    label = "Base Null Probability",
    color = "#009E73"
  )
)

# MAYBE CONSIDER ADDING LATER
# uninformed_prior_variance = list(min = 0.5, max = 2, label = "Prior Variance")
# effect_size_mean = list(min = 0.1, max = 0.5, label = "Effect Size Mean")
# career_turnover_selection_rate = list(min = 0.3, max = 0.7, label = "Selection Rate")
# mutation_rate = list(min = 1e-4, max = 0.1, label = "Mutation Rate", log = TRUE)

# Latin hypercube sampling for better parameter space coverage
draw_params <- function(n = 1) {
  h <- randomLHS(n, length(param_config))
  data.frame(
    # log-uniform for sample size (better coverage of small values)
    hold_samples_constant_at = round(exp(qunif(
      h[, 1],
      log(param_config$hold_samples_constant_at$min),
      log(param_config$hold_samples_constant_at$max)
    ))),
    nonsig_logistic_midpoint = qunif(
      h[, 2],
      param_config$nonsig_logistic_midpoint$min,
      param_config$nonsig_logistic_midpoint$max
    ),
    base_null_probability = qunif(
      h[, 3],
      param_config$base_null_probability$min,
      param_config$base_null_probability$max
    )
  )
}

#### SETUP PARALLEL BACKEND ####
n_cores <- parallel::detectCores() - 1
cl <- makeCluster(n_cores)
registerDoSNOW(cl)
cat("Running on", n_cores, "cores\n")

#### RUNNING THE SWEEP ####
run_sweep <- TRUE # flag for apply_publication_bias to use set_nonsig_logistic_midpoint
n_sims <- 2000 # total number of simulations (with param combinations randomly sampled)

# Progress bar
pb <- txtProgressBar(max = n_sims, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)

# Pre-allocate parameter dataframe
sweep_params <- draw_params(n_sims)
sweep_params$seed <- seq_len(n_sims)

# Base parameters
base_params <- list(
  n_agents = 1000,
  n_timesteps = 350,
  n_timesteps_per_career_step = 35,
  duration_per_observation = 0.1,
  duration_original_intercept = 1,
  n_effects = 500000,
  effect_size_mean = 0.3,
  effect_size_variance = 0.1,
  uninformed_prior_mean = 0,
  uninformed_prior_variance = 1,
  initial_selection_condition = 1,
  switch_conditions_at = NA,
  career_turnover_selection_rate = 0.5,
  innovation_sd = 0,
  mutation_rate = 0.1,
  replications_dynamic_sample_sizes = 1,
  publication_bias = 2
)

# Run simulations in parallel
sweep_results <- foreach(
  i = seq_len(n_sims),
  .combine = rbind,
  .packages = c("dplyr", "testthat"),
  .export = c(
    # data
    "base_params",
    "sweep_params",
    "run_sweep",
    # main simulation function
    "run_simulation",
    # initialization functions
    "initialize_effects_matrix",
    "initialize_agents_matrix",
    "initialize_studies_matrix",
    # agent functions
    "add_agents",
    "career_turnover",
    # study functions
    "run_studies",
    "assign_effects",
    "determine_sample_sizes",
    "determine_study_durations",
    "generate_study_results",
    # bayesian/contribution functions
    "kl_norm",
    "prepare_bayesian_data",
    "calculate_novelty_contribution",
    "calculate_truth_contribution",
    "update_effects_beliefs",
    # publication bias functions
    "apply_publication_bias",
    "logistic_significant",
    "logistic_nonsignificant"
  ),
  .options.snow = list(progress = progress)
) %dopar%
  {
    # Set seed for this simulation (reproducible)
    set.seed(sweep_params$seed[i])

    # Build params for this run
    params <- base_params
    params$hold_samples_constant_at <- sweep_params$hold_samples_constant_at[i]
    params$set_nonsig_logistic_midpoint <- sweep_params$nonsig_logistic_midpoint[
      i
    ]
    params$base_null_probability <- sweep_params$base_null_probability[i]

    # Run simulation (verbose=0 to suppress printing in parallel)
    sim_env <- run_simulation(params, verbose = 0)

    # Extract mean replication_probability of agents active in final 50 timesteps
    window_start <- sim_env$n_timesteps - 50
    active_in_window <- which(
      !is.na(sim_env$agents[, "researcher_id"]) &
        (is.na(sim_env$agents[, "timestep_inactive"]) |
          sim_env$agents[, "timestep_inactive"] > window_start)
    )
    # save agent replication probabilities (% replicator agents)
    mean_rep_rate <- mean(sim_env$agents[
      active_in_window,
      "replication_probability"
    ])
    # save % of original studies published (vs not)
    mean_original_published <- mean(
      sim_env$studies[
        sim_env$studies[, "study_type"] == 0,
        "publication_status"
      ],
      na.rm = TRUE
    )
    # save % of replication studies published (vs not)
    mean_replication_published <- mean(
      sim_env$studies[
        sim_env$studies[, "study_type"] == 1,
        "publication_status"
      ],
      na.rm = TRUE
    )
    # calculate total scientific progress (sum of KL reduction for studied effects)
    has_effect_id <- !is.na(sim_env$effects[, "effect_id"])
    is_latest_update <- !duplicated(
      sim_env$effects[, "effect_id"],
      fromLast = TRUE
    )
    has_been_studied <- !is.na(sim_env$effects[, "study_id"])
    studied_effects <- sim_env$effects[
      has_effect_id & is_latest_update & has_been_studied,
    ]

    # extract values for KL calculations
    true_mean <- studied_effects[, "true_effect_size"]
    true_sd <- sqrt(studied_effects[, "true_effect_variance"])
    posterior_mean <- studied_effects[, "posterior_effect_size"]
    posterior_sd <- sqrt(studied_effects[, "posterior_effect_variance"])
    prior_mean <- sim_env$uninformed_prior_mean
    prior_sd <- sqrt(sim_env$uninformed_prior_variance)

    # KL from uninformed prior to truth (baseline uncertainty)
    baseline_kl <- kl_norm(true_mean, true_sd, prior_mean, prior_sd)
    # KL from current posterior to truth (remaining uncertainty)
    current_kl <- kl_norm(true_mean, true_sd, posterior_mean, posterior_sd)
    # total progress = sum of reductions (higher = better)
    total_scientific_progress <- sum(baseline_kl - current_kl)

    # Return a one-row data frame (these get rbind'd together)
    data.frame(
      hold_samples_constant_at = sweep_params$hold_samples_constant_at[i],
      nonsig_logistic_midpoint = sweep_params$nonsig_logistic_midpoint[i],
      base_null_probability = sweep_params$base_null_probability[i],
      seed = sweep_params$seed[i],
      mean_replication_rate = mean_rep_rate,
      mean_original_published = mean_original_published,
      mean_replication_published = mean_replication_published,
      total_scientific_progress = total_scientific_progress
    )
  }

# Clean up
close(pb)
stopCluster(cl)
cat("\nCompleted", n_sims, "simulations\n")

#### PLOTTING ####
# set the outcome variable to plot (uncomment one pair)
outcome_var <- "mean_replication_rate"
outcome_label <- "% replicator agents"
# outcome_var <- "mean_original_published"
# outcome_label <- "% original studies published"
# outcome_var <- "mean_replication_published"
# outcome_label <- "% replication studies published"
# outcome_var <- "total_scientific_progress"
# outcome_label <- "Total scientific progress (KL)"

# extract plotting info from param_config
param_names <- names(param_config)
param_labels <- sapply(param_config, `[[`, "label")
param_colors <- sapply(param_config, `[[`, "color")
param_ranges <- lapply(param_config, function(x) c(x$min, x$max))

# Normalize parameters to 0-1 for combined partial dependency plot
sweep_norm <- sweep_results
for (i in seq_along(param_names)) {
  r <- param_ranges[[i]]
  sweep_norm[[paste0(param_names[i], "_norm")]] <- (sweep_results[[param_names[
    i
  ]]] -
    r[1]) /
    (r[2] - r[1])
}

# Build loess predictions for each parameter for the partial dependency plot
pdp_data <- data.frame()
for (i in seq_along(param_names)) {
  norm_col <- paste0(param_names[i], "_norm")
  fit <- loess(
    as.formula(paste(outcome_var, "~ get(norm_col)")),
    data = sweep_norm,
    span = 0.75
  )
  grid_x <- seq(0, 1, length.out = 100)
  pdp_data <- rbind(
    pdp_data,
    data.frame(
      param = param_labels[i],
      x_norm = grid_x,
      y = predict(fit, newdata = setNames(data.frame(grid_x), norm_col))
    )
  )
}

# Combined partial dependency plot
p_pdp <- ggplot(pdp_data, aes(x = x_norm, y = y, color = param)) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = setNames(param_colors, param_labels)) +
  labs(
    x = "Parameter Value (normalized)",
    y = outcome_label,
    color = NULL
  ) +
  theme_classic() +
  theme(legend.position = "bottom")

# Individual scatterplots
make_scatter <- function(i) {
  ggplot(
    sweep_results,
    aes(x = .data[[param_names[i]]], y = .data[[outcome_var]])
  ) +
    geom_point(color = param_colors[i], alpha = 0.6) +
    geom_smooth(
      method = "loess",
      se = FALSE,
      color = param_colors[i],
      span = 0.75
    ) +
    labs(x = param_labels[i], y = outcome_label) +
    theme_classic()
}

# Display
p_pdp
make_scatter(1) + make_scatter(2) + make_scatter(3)

#### TWO-WAY INTERACTION HEATMAPS ####

make_heatmap <- function(xvar, yvar, xlabel, ylabel, selection = T) {
  ggplot(
    sweep_results[selection, ],
    aes(x = .data[[xvar]], y = .data[[yvar]], z = .data[[outcome_var]])
  ) +
    stat_summary_2d(fun = mean, bins = 15) +
    scale_fill_viridis_c(name = outcome_label) +
    labs(x = xlabel, y = ylabel) +
    theme_classic()
}

p_int1 <- make_heatmap(
  "nonsig_logistic_midpoint",
  "base_null_probability",
  "Publication Bias",
  "Base Null Probability",
)
p_int2 <- make_heatmap(
  "nonsig_logistic_midpoint",
  "hold_samples_constant_at",
  "Publication Bias",
  "Sample Size"
)
p_int3 <- make_heatmap(
  "base_null_probability",
  "hold_samples_constant_at",
  "Base Null Probability",
  "Sample Size"
)

p_int1
p_int2
p_int3

# three-way interaction slices
lower_bound <- c(0, 0.2, 0.4, 0.6, 0.8)
for (j in lower_bound) {
  p_slice <- make_heatmap(
    "nonsig_logistic_midpoint",
    "hold_samples_constant_at",
    "Publication Bias",
    "Sample Size",
    selection = sweep_results$base_null_probability > j &
      sweep_results$base_null_probability < (j + 0.2)
  )
  p_slice <- p_slice + ggtitle(paste0("Base null between ", j, " and ", j + .2))
  print(p_slice)
}
