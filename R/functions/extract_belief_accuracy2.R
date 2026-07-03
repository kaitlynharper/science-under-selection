#' Extract Belief Accuracy Over Time
#'
#' Calculates the Kullback-Leibler (KL) divergence between posterior beliefs
#' and true effect sizes for all investigated effects at each timestep.
#' This function tracks how beliefs evolve over time and measures the accuracy
#' of the scientific community's collective knowledge.
#'
#' @param sim_env An environment containing simulation results
#'
#' @return A data frame with one row per timestep (0 to n_timesteps) containing:
#'   \describe{
#'     \item{timestep}{The timestep number}
#'     \item{total_kl}{Sum of KL divergences across all investigated effects at that timestep}
#'     \item{kl_per_effect}{Mean KL divergence per investigated effects at that timestep}
#'     \item{n_effects_investigated}{Number of unique effects that have been investigated
#'       (have at least one associated study) up to that timestep}
#'   }
#'
#' @details
#' The function:
#' \itemize{
#'   \item Identifies all effects that have been investigated at least once (have a study_id)
#'   \item For each timestep, finds the most recent posterior belief for each effect
#'   \item Calculates KL divergence from posterior to truth using \code{kl_norm()}
#'   \item Returns NA for timesteps where no effects have been investigated yet
#' }
#'
#' KL divergence measures how far posterior beliefs are from the true effect sizes,
#' with lower values indicating more accurate beliefs.
#'
#' @seealso \code{\link{kl_norm}} for the KL divergence calculation
extract_belief_accuracy2 <- function(sim_env) {

  # select only effects that have been at least investigated once:
  # The cumulative knowledge is defined by the published literature
  investigated_effect_ids <- sim_env$effects[
    !is.na(sim_env$effects[, "effect_id"]) & !is.na(sim_env$effects[, "study_id"]),
    "effect_id"
  ] |> unique()

  # from the universe of all existing effects, select those that have been investigated:
  effects <- sim_env$effects[
    sim_env$effects[, "effect_id"] %in% investigated_effect_ids,
    ,
    drop = FALSE
  ]
  timesteps <- 0:sim_env$n_timesteps
  n_t <- length(timesteps)

  # Pre-allocate output
  out <- data.frame(
    timestep = timesteps,
    total_kl = numeric(n_t),
    kl_per_effect = numeric(n_t),
    n_effects_investigated = integer(n_t),
    n_studies_published = integer(n_t)
  )

  for (i in seq_along(timesteps)) {
    threshold <- timesteps[i]
    
    # Version 1: Only look at effects that have been investigated up to each timestep
    relevant <- effects[effects[, "timestep"] <= threshold, , drop = FALSE]

    # Version 2: Look at all effects; those that have not been investigated yet are reverted to the default prior of non-knowledge
    # relevant <- effects
    # relevant[relevant[, "timestep"] > threshold, "posterior_effect_size"] <- 0
    # relevant[relevant[, "timestep"] > threshold, "posterior_effect_variance"] <- 1

    if (nrow(relevant) == 0) {
      out$total_kl[i] <- NA
      out$kl_per_effect[i] <- NA
      next
    }

    # Get latest row per effect using base R
    latest_idx <- !duplicated(relevant[, "effect_id"], fromLast = TRUE)
    latest <- relevant[latest_idx, , drop = FALSE]

    kl <- kl_norm(
      latest[, "true_effect_size"],
      sqrt(latest[, "true_effect_variance"]),
      latest[, "posterior_effect_size"],
      sqrt(latest[, "posterior_effect_variance"])
    )

    out$total_kl[i] <- sum(kl)
    out$kl_per_effect[i] <- mean(kl)
    out$n_effects_investigated[i] <- relevant[!is.na(relevant[, "study_id"]), "effect_id"] |> unique() |> length()
    out$n_studies_published[i] <- relevant[!is.na(relevant[, "study_id"]), "study_id"] |> unique() |> length()
  }
  out
}

#' Extract Total Scientific Progress Over Time
#'
#' Matches the end-of-simulation \code{total_scientific_progress} measure from
#' parameter sweeps: for each timestep, sums over studied effects the reduction
#' in KL divergence from the uninformed prior to the current posterior at truth.
#'
#' @param sim_env An environment containing simulation results
#'
#' @return A data frame with one row per timestep (0 to n_timesteps) containing
#'   \code{timestep} and \code{total_scientific_progress}.
#'
#' @details Beliefs update when a study is run, which may be before
#'   \code{timestep_completed}; study start time is
#'   \code{timestep_completed - timesteps_duration}.
#'
#' @seealso \code{\link{extract_belief_accuracy2}}
extract_total_scientific_progress <- function(sim_env) {
  studies <- as.data.frame(sim_env$studies)
  published_studies <- studies[
    !is.na(studies$study_id) & studies$publication_status == 1,
    c("study_id", "timestep_completed", "timesteps_duration"),
    drop = FALSE
  ]
  published_studies$timestep_started <-
    published_studies$timestep_completed -
    published_studies$timesteps_duration

  effects <- as.data.frame(sim_env$effects)
  effects <- effects[
    !is.na(effects$effect_id) & !is.na(effects$study_id),
    ,
    drop = FALSE
  ]
  effects <- merge(
    effects,
    published_studies[, c("study_id", "timestep_started")],
    by = "study_id",
    sort = FALSE
  )

  timesteps <- 0:sim_env$n_timesteps
  n_t <- length(timesteps)
  prior_mean <- sim_env$uninformed_prior_mean
  prior_sd <- sqrt(sim_env$uninformed_prior_variance)
  use_savage_dickey <- identical(sim_env$truth_contribution_method, "savage_dickey")

  out <- data.frame(
    timestep = timesteps,
    total_scientific_progress = numeric(n_t)
  )

  for (i in seq_along(timesteps)) {
    threshold <- timesteps[i]
    relevant <- effects[effects$timestep_started <= threshold, , drop = FALSE]

    if (nrow(relevant) == 0) {
      next
    }

    latest_idx <- !duplicated(relevant$effect_id, fromLast = TRUE)
    latest <- relevant[latest_idx, , drop = FALSE]

    true_mean <- latest$true_effect_size
    true_sd <- sqrt(latest$true_effect_variance)
    post_mean <- latest$posterior_effect_size
    post_sd <- sqrt(latest$posterior_effect_variance)

    if (use_savage_dickey) {
      log_prior_at_true <- stats::dnorm(
        true_mean, prior_mean, prior_sd, log = TRUE
      )
      log_posterior_at_true <- stats::dnorm(
        true_mean, post_mean, post_sd, log = TRUE
      )
      out$total_scientific_progress[i] <- sum(
        log_posterior_at_true - log_prior_at_true
      )
    } else {
      baseline_kl <- kl_norm(true_mean, true_sd, prior_mean, prior_sd)
      current_kl <- kl_norm(true_mean, true_sd, post_mean, post_sd)
      out$total_scientific_progress[i] <- sum(baseline_kl - current_kl)
    }
  }

  out
}
