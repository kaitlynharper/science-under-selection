##############################################################################
# Run sweep (parallel version)
#
# Description: Runs simulations across many parameter combinations in parallel
# and saves results with metadata to RDS for analysis in analyse_sweep.R.
# Source set_sweep_parameters.R first, or run it before this script.
##############################################################################
library(here)
library(foreach)
library(doSNOW)

if (!exists("base_params")) {
  source(here("R", "set_sweep_parameters.R"))
}

##############################################################################
#### RUN SWEEP ####
##############################################################################
# Set up parallel backend
cl <- makeCluster(n_cores)
registerDoSNOW(cl)

# Initialise sweep_results
sweep_results <- NULL

# Check for any missing simulation seeds to rerun
for (sweep_topup in seq_len(max_sweep_topups)) {
  if (sweep_topup > 1L) {
    missing_seeds <- setdiff(sweep_params_full$seed, sweep_results$seed)
    if (length(missing_seeds) == 0L) {
      break # skip if no seeds are missing
    }
    sweep_params <- sweep_params_full[
      sweep_params_full$seed %in% missing_seeds,
      ,
      drop = FALSE
    ]
    # Message about how many seeds are missing
    message(paste0(
      "Rerunning ",
      length(missing_seeds),
      " missing simulation seed/s."
    ))
  } else {
    sweep_params <- sweep_params_full
  }

  pb <- txtProgressBar(max = nrow(sweep_params), style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)

  results <- foreach(
    i = seq_len(nrow(sweep_params)),
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
      rep_success_postpub <- if (
        sum(published_replications, na.rm = TRUE) == 0
      ) {
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
      if (length(sweep_param_names) == 0L) {
        result_df <- data.frame(seed = sweep_params$seed[i])
      } else {
        result_df <- as.data.frame(lapply(sweep_param_names, function(nm) {
          sweep_params[[nm]][i]
        }))
        names(result_df) <- sweep_param_names
        result_df$seed <- sweep_params$seed[i]
      }
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
  sweep_results <- dplyr::bind_rows(sweep_results, results)
}

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
# Save path so analyse_sweep.R can load "latest" run
writeLines(sweep_path, here("output/last_sweep_path.txt"))
