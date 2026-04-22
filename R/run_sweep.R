##############################################################################
# Run sweep (parallel version)
#
# Description: Sets which parameters to sweep and their ranges (via control
# panel + master spec), runs simulations across many combinations in parallel,
# and saves results with metadata to RDS for analysis in analyze_sweep.R.
##############################################################################
library(here)
library(dplyr)
library(lhs)
library(foreach)
library(doSNOW)

# Source all functions once before parallel execution
function_files <- list.files(here("R", "functions"), full.names = TRUE)
sapply(function_files, source, .GlobalEnv)
source(here("R", "model.R"))

##############################################################################
#### CONTROL PANEL ####
##############################################################################

# Run settings
n_sims <- 2000 # total number of simulations (param combinations from LHS)
n_cores <- parallel::detectCores() - 2

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
  effect_size_variance = 0.1, # variance of effect sizes
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
  publication_bias = 2, # 0 = no publication bias, 1 = weak publication bias, 2 = strong publication bias
  nonsig_logistic_midpoint = NA, # NA = use preset from publication_bias; number = use this value (e.g. in sweeps)
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

##############################################################################
#### RUN SWEEP ####
##############################################################################
# Set up parallel backend
cl <- makeCluster(n_cores)
registerDoSNOW(cl)

# Initialize progress bar
pb <- txtProgressBar(max = n_sims, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)

# Draw parameters for each simulation
sweep_params <- draw_params(n_sims)
sweep_params$seed <- seq_len(n_sims)

# Run simulations in parallel
sweep_results <- foreach(
  i = seq_len(n_sims),
  .combine = rbind,
  .packages = c("dplyr", "testthat"),
  .errorhandling = "remove",
  .options.snow = list(progress = progress)
) %dopar%
  {
    set.seed(sweep_params$seed[i])

    params <- base_params
    for (j in seq_along(sweep_param_names)) {
      nm <- sweep_param_names[j]
      params[[nm]] <- sweep_params[[nm]][i]
    }

    sim_env <- run_simulation(params, verbose = 0)

    window_start <- sim_env$n_timesteps - 50
    active_in_window <- which(
      !is.na(sim_env$agents[, "researcher_id"]) &
        (is.na(sim_env$agents[, "timestep_inactive"]) |
          sim_env$agents[, "timestep_inactive"] > window_start)
    )
    mean_rep_rate <- mean(sim_env$agents[
      active_in_window,
      "replication_probability"
    ])
    mean_original_published <- mean(
      sim_env$studies[
        sim_env$studies[, "study_type"] == 0,
        "publication_status"
      ],
      na.rm = TRUE
    )
    mean_replication_published <- mean(
      sim_env$studies[
        sim_env$studies[, "study_type"] == 1,
        "publication_status"
      ],
      na.rm = TRUE
    )

    # For replication-success metrics, use published originals as the reference
    published_originals <- sim_env$studies[
      sim_env$studies[, "study_type"] == 0 &
        sim_env$studies[, "publication_status"] == 1 &
        !is.na(sim_env$studies[, "estimated_mean"]) &
        !is.na(sim_env$studies[, "effect_id"]),
      ,
      drop = FALSE
    ]
    # Grab all replications
    replications <- sim_env$studies[
      sim_env$studies[, "study_type"] == 1 &
        !is.na(sim_env$studies[, "p_value"]) &
        !is.na(sim_env$studies[, "p_value_original"]) &
        !is.na(sim_env$studies[, "estimated_mean"]) &
        !is.na(sim_env$studies[, "effect_id"]),
      ,
      drop = FALSE
    ]

    # Match each replication to its original via effect_id
    original_means <- published_originals[, "estimated_mean"][
      match(replications[, "effect_id"], published_originals[, "effect_id"])
    ]
    has_original <- !is.na(original_means)

    # Replication "success" = same significance + same effect direction
    p_match <- (replications[has_original, "p_value"] < 0.05) ==
      (replications[has_original, "p_value_original"] < 0.05)
    direction_match <- sign(replications[has_original, "estimated_mean"]) ==
      sign(original_means[has_original])
    replication_success <- p_match & direction_match

    # Percent of all replications that match their original
    rep_success_prepub <- if (length(replication_success) == 0) {
      NA_real_
    } else {
      100 * mean(replication_success)
    }

    # Percent of published replications that match their original
    published_replications <- replications[
      has_original,
      "publication_status"
    ] ==
      1
    rep_success_postpub <- if (sum(published_replications, na.rm = TRUE) == 0) {
      NA_real_
    } else {
      100 * mean(replication_success[published_replications], na.rm = TRUE)
    }

    # Calculating scientific progress
    has_effect_id <- !is.na(sim_env$effects[, "effect_id"])
    is_latest_update <- !duplicated(
      sim_env$effects[, "effect_id"],
      fromLast = TRUE
    )
    has_been_studied <- !is.na(sim_env$effects[, "study_id"])
    studied_effects <- sim_env$effects[
      has_effect_id & is_latest_update & has_been_studied,
    ]
    true_mean <- studied_effects[, "true_effect_size"]
    true_sd <- sqrt(studied_effects[, "true_effect_variance"])
    posterior_mean <- studied_effects[, "posterior_effect_size"]
    posterior_sd <- sqrt(studied_effects[, "posterior_effect_variance"])
    prior_mean <- sim_env$uninformed_prior_mean
    prior_sd <- sqrt(sim_env$uninformed_prior_variance)
    # TEMP: testing savage-dickey method. Effect-level total progress: KL vs Savage-Dickey.
    if (params$truth_contribution_method == "savage_dickey") {
      log_prior_at_true <- stats::dnorm(
        true_mean,
        prior_mean,
        prior_sd,
        log = TRUE
      )
      log_posterior_at_true <- stats::dnorm(
        true_mean,
        posterior_mean,
        posterior_sd,
        log = TRUE
      )
      total_scientific_progress <- sum(
        log_posterior_at_true - log_prior_at_true
      )
    } else {
      baseline_kl <- kl_norm(true_mean, true_sd, prior_mean, prior_sd)
      current_kl <- kl_norm(true_mean, true_sd, posterior_mean, posterior_sd)
      total_scientific_progress <- sum(baseline_kl - current_kl)
    }

    # Calculating total resources (timesteps) that count towards published knowledge
    total_timesteps <- sum(
      sim_env$studies[, "timesteps_duration"],
      na.rm = TRUE
    )
    published_timesteps <- sum(
      sim_env$studies[
        sim_env$studies[, "publication_status"] == 1,
        "timesteps_duration"
      ],
      na.rm = TRUE
    )
    perc_resources_published <- 100 * published_timesteps / total_timesteps

    # Store all in results df
    result_df <- as.data.frame(lapply(sweep_param_names, function(nm) {
      sweep_params[[nm]][i]
    }))
    names(result_df) <- sweep_param_names
    result_df$seed <- sweep_params$seed[i]
    result_df$mean_replication_rate <- mean_rep_rate
    result_df$mean_original_published <- mean_original_published
    result_df$mean_replication_published <- mean_replication_published
    result_df$rep_success_prepub <- rep_success_prepub
    result_df$rep_success_postpub <- rep_success_postpub
    result_df$total_scientific_progress <- total_scientific_progress
    result_df$perc_resources_published <- perc_resources_published
    result_df
  }

close(pb)
stopCluster(cl)

##############################################################################
#### SAVE OUTPUT ####
##############################################################################
# Save output as list with parameter metadata and results, named with date
sweep_output <- list(
  meta = list(
    param_config = param_config,
    sweep_param_names = sweep_param_names,
    base_params = base_params,
    n_sims = n_sims,
    timestamp = Sys.time()
  ),
  results = sweep_results
)
timestamp <- format(Sys.time(), "%Y-%m-%d-%H%M")
sweep_path <- paste0("output/sweep_results_", timestamp, ".rds")
saveRDS(sweep_output, here(sweep_path))
# Save path so analyze_sweep.R can load "latest" run
writeLines(sweep_path, here("output/last_sweep_path.txt"))
