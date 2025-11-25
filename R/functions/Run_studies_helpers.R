##########################################################################
# Helper functions for running studies
##########################################################################

# Determine sample size: power.t.test
# Determine number of timesteps: base rate for sample size + intercept for original study vs replication?
# Generate observed effect size and p-value
# Calculate novelty contribution and truth contribution
# Publication bias?
# Update effects matrix

#### assign_effects ####
assign_effects <- function(sim_env) {
  n_studies <- nrow(sim_env$new_studies)
  
  # determine study types based on each agent's replication_probability
  is_replication <- runif(n_studies) < sim_env$new_studies[, "replication_probability"]
  sim_env$new_studies[, "study_type"] <- ifelse(is_replication, 1, 0)
  
  # identify available effect_ids for original studies
  available_original_effects <- sim_env$effects[
    !sim_env$effects[, "effect_id"] %in%
      sim_env$studies[
        sim_env$studies[, "publication_status"] == 1,
        "effect_id"
      ],
    "effect_id"
  ]
  
  # identify available effect_ids for replication studies
  available_replication_effects <- unique(sim_env$studies[
    sim_env$studies[, "publication_status"] == 1 & !is.na(sim_env$studies[, "effect_id"]),  
    "effect_id"
  ])
  
  # if not enough available effects for replication, convert excess to originals
  if (sum(is_replication) > length(available_replication_effects)) {
    excess_replications <- sum(is_replication) -
      sum(!is.na(available_replication_effects))
    convert_indices <- which(is_replication)[order(sim_env$new_studies[
      is_replication,
      "replication_probability"
    ])[1:excess_replications]]
    is_replication[convert_indices] <- FALSE
    sim_env$new_studies[convert_indices, "study_type"] <- 0
  }
  
  # stop if not enough original effects left
  if(sum(!is_replication) > length(available_original_effects)){
    stop("Insufficient original effects at timestep ", sim_env$timestep)
  }
  
  # assign effect_ids to original studies (without replacement)
  sim_env$new_studies[!is_replication, "effect_id"] <- sample(
    available_original_effects,
    size = sum(!is_replication)
  )
  
  # assign effect_ids to replication studies
  # count total publications per effect
  publication_counts <- table(sim_env$studies[
    sim_env$studies[, "publication_status"] == 1 & !is.na(sim_env$studies[, "effect_id"]),
    "effect_id"
  ])
  # add small random jitter to order effects randomly within each count level
  jittered_counts <- as.numeric(publication_counts) + runif(length(publication_counts)) * 0.01
  # order effect ids by jittered counts (ascending = fewer publications first)
  ordered_effects <- as.numeric(names(publication_counts)[order(jittered_counts)])
  # assign from ordered list (without replacement)
  sim_env$new_studies[is_replication, "effect_id"] <- ordered_effects[1:sum(is_replication)]
}

#### determine_sample_sizes ####
determine_sample_sizes <- function(sim_env) {
  browser()
  n_studies <- nrow(sim_env$new_studies)
  
  # identify study types
  is_replication <- sim_env$new_studies[, "study_type"] == 1
  
  # calculate reference effects
  reference_effects <- numeric(n_studies)
  
  # originals: burn-in or mean published
  if (sim_env$timestep < sim_env$n_timesteps_per_career_step) {
    reference_effects[!is_replication] <- 0.5
  } else {
    published_effects <- sim_env$studies[
      sim_env$studies[, "publication_status"] == 1 & 
        !is.na(sim_env$studies[, "estimated_mean"]),
      "estimated_mean"
    ]
    reference_effects[!is_replication] <- mean(published_effects)
  }
  
  # replications: original study mean
  if (sum(is_replication) > 0) {
    # create lookup: effect_id -> estimated_mean (for published originals only)
    published_original_rows <- sim_env$studies[, "study_type"] == 0 & 
                              sim_env$studies[, "publication_status"] == 1 &
                              !is.na(sim_env$studies[, "estimated_mean"])
    
    original_means_by_effect <- sim_env$studies[published_original_rows, "estimated_mean"]
    names(original_means_by_effect) <- sim_env$studies[published_original_rows, "effect_id"]
    
    # look up means for each replication
    replication_effect_ids <- sim_env$new_studies[is_replication, "effect_id"]
    reference_effects[is_replication] <- original_means_by_effect[as.character(replication_effect_ids)]
  }
  
  # calculate sample sizes with appropriate test type
  sample_sizes <- mapply(
    function(power, delta, alt) {
      power_result <- power.t.test(
        power = power,
        delta = abs(delta),
        sd = 1,
        sig.level = 0.05,
        type = "two.sample",
        alternative = alt
      )
      max(ceiling(power_result$n), 1)
    },
    power = sim_env$new_studies[, "target_power"],
    delta = reference_effects,
    alt = ifelse(is_replication, "one.sided", "two.sided")
  )
  sim_env$new_studies[, "sample_size"] <- sample_sizes
}
