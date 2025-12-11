##############################################################################
# Exploring the replication database
# Description: Explore "The Replication Database" file
# available under CCBY4.0 https://doi.org/10.17605/OSF.IO/9CSP3
##############################################################################

# Packages
library(tidyr)
library(dplyr)
library(ggplot2)

# Read data
setwd("~/Documents/Github/science-under-selection/preregistration/")
replication_database <- read.csv("replication_database_jopd_red_data.csv")

# Select variables
replication_database <- replication_database %>%
  select(c(
    "X",
    "es_original",
    "es_replication",
    "n_original",
    "n_replication",
    "ci.lower_original",
    "ci.upper_original",
    "ci.lower_replication",
    "ci.upper_replication",
    "n_original",
    "n_replication",
    "significant_original",
    "significant_replication"
  ))

# Compute SE values
replication_database <- replication_database %>%
  mutate(
    se_original = (ci.upper_original - ci.lower_original) / (2 * 1.96),
    se_replication = (ci.upper_replication - ci.lower_replication) / (2 * 1.96)
  )

# Calculate KL measures
# KL divergence function
kl_norm <- function(mu0, sd0, mu1, sd1) {
  log(sd1 / sd0) + (sd0^2 + (mu0 - mu1)^2) / (2 * sd1^2) - 0.5
}
# Calculate posteriors functions
posteriors_original <- function(data, prior_mean, prior_sd) {
  prior_var <- prior_sd^2
  likelihood_mean <- data$es_original
  likelihood_sd <- data$se_original
  likelihood_var <- likelihood_sd^2

  posterior_var <- 1 / (1 / prior_var + 1 / likelihood_var)
  posterior_mean <- ((prior_mean / prior_var) +
    (likelihood_mean / likelihood_var)) *
    posterior_var
  posterior_sd <- sqrt(posterior_var)

  data$posterior_mean_original <- posterior_mean
  data$posterior_sd_original <- posterior_sd

  return(data)
}
posteriors_replication <- function(data) {
  prior_mean <- data$posterior_mean_original
  prior_sd <- data$posterior_sd_original
  prior_var <- prior_sd^2
  likelihood_mean <- data$es_replication
  likelihood_sd <- data$se_replication
  likelihood_var <- likelihood_sd^2

  posterior_var <- 1 / (1 / prior_var + 1 / likelihood_var)
  posterior_mean <- ((prior_mean / prior_var) +
    (likelihood_mean / likelihood_var)) *
    posterior_var
  posterior_sd <- sqrt(posterior_var)

  data$posterior_mean_replication <- posterior_mean
  data$posterior_sd_replication <- posterior_sd

  return(data)
}

# Calculate posteriors for original then replication studies
replication_database <- posteriors_original(
  replication_database,
  uninformed_prior_mean,
  uninformed_prior_sd
)
replication_database <- posteriors_replication(replication_database)

# Calculate novelty scores for all studies
replication_database <- replication_database %>%
  mutate(
    novelty_original = kl_norm(
      posterior_mean_original,
      posterior_sd_original,
      uninformed_prior_mean,
      uninformed_prior_sd
    )
  )
replication_database <- replication_database %>%
  mutate(
    novelty_replication = kl_norm(
      posterior_mean_replication,
      posterior_sd_replication,
      posterior_mean_original,
      posterior_sd_original
    )
  )

# Add original significance for original and replications
replication_database$significant_o_original <- replication_database$significant_original
replication_database$significant_o_replication <- replication_database$significant_original

# Calculate consistency between original and replication significance
replication_database <- replication_database %>%
  mutate(
    consistent = case_when(
      is.na(significant_original) | is.na(significant_replication) ~ NA, #if either is NA, it is NA
      significant_original == significant_replication ~ TRUE, #if they are the same, it is consistent (true)
      TRUE ~ FALSE #otherwise it is inconsistent (false)
    )
  )

# Determine true effect (either 0 or mean of original studies) according to percent_null_effect
replication_database <- replication_database %>%
  mutate(
    true_mean = ifelse(
      runif(n()) < percent_null_effects,
      0,
      es_original
    )
  )

# Calculate truth contribution of original study
replication_database <- replication_database %>%
  mutate(
    truth_original = (kl_norm(
      true_mean,
      true_sd,
      uninformed_prior_mean,
      uninformed_prior_sd
    )) -
      (kl_norm(
        true_mean,
        true_sd,
        posterior_mean_original,
        posterior_sd_original
      ))
  )

# Calculate truth contribution of replication
replication_database <- replication_database %>%
  mutate(
    truth_replication = (kl_norm(
      true_mean,
      true_sd,
      posterior_mean_original,
      posterior_sd_original
    )) -
      (kl_norm(
        true_mean,
        true_sd,
        posterior_mean_replication,
        posterior_sd_replication
      ))
  )

# Convert to long format
replication_database_long <- as.data.frame(
  replication_database %>%
    pivot_longer(
      cols = ends_with("_original") | ends_with("_replication"),
      names_to = c(".value", "type"),
      names_pattern = "(.*)_(original|replication)"
    )
)

# Define logistic functions for publication probability
# Logistic function for significant results (0.5 to 1)
logistic_significant <- function(
  novelty,
  midpoint = sig_logistic_midpoint,
  steepness = sig_logistic_steepness
) {
  sig_y_intercept +
    ((1 - sig_y_intercept) / (1 + exp(-steepness * (novelty - midpoint))))
}
# Logistic function for non-significant results (0 to 1)
logistic_nonsignificant <- function(
  novelty,
  midpoint = nonsig_logistic_midpoint,
  steepness = nonsig_logistic_steepness
) {
  1 / (1 + exp(-steepness * (novelty - midpoint)))
}

# Visualize the logistic functions
novelty_range <- seq(0, 50, by = 0.01)
viz_data <- data.frame(
  novelty = rep(novelty_range, 2),
  publication_prob = c(
    logistic_significant(novelty_range),
    logistic_nonsignificant(novelty_range)
  ),
  result_type = rep(
    c("Significant", "Non-significant"),
    each = length(novelty_range)
  )
)
pub_prob <- ggplot(
  viz_data,
  aes(x = novelty, y = publication_prob, linetype = result_type)
) +
  geom_line(color = "black", linewidth = 1.5) +
  theme_minimal() +
  labs(
    title = "Publication Probability by Novelty and Significance",
    x = "Novelty",
    y = "Publication Probability",
    linetype = "Result Type"
  ) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
  scale_linetype_manual(values = c("dotted", "solid")) +
  scale_x_continuous(limits = c(0, 3))


# Apply functions based on significance
replication_database_long <- replication_database_long %>%
  mutate(
    publication_prob = ifelse(
      significant == 1,
      logistic_significant(novelty),
      logistic_nonsignificant(novelty)
    )
  )

# Create combined grouping variable for plotting
if (show_significance_detail) {
  # Detailed breakdown by significance
  replication_database_long <- replication_database_long %>%
    mutate(
      group = case_when(
        type == "original" & significant == 1 ~ "Original (sig)",
        type == "original" & significant == 0 ~ "Original (non-sig)",
        type == "replication" & significant == 1 ~ "Replication (sig)",
        type == "replication" & significant == 0 ~ "Replication (non-sig)"
      ),
      group = factor(
        group,
        levels = c(
          "Original (sig)",
          "Original (non-sig)",
          "Replication (sig)",
          "Replication (non-sig)"
        )
      )
    )

  # Define colors: darker for significant, lighter for non-significant
  group_colors <- c(
    "Original (sig)" = "#8B0000", # dark red
    "Original (non-sig)" = "#FFB6C1", # light red
    "Replication (sig)" = "#00008B", # dark blue
    "Replication (non-sig)" = "#ADD8E6" # light blue
  )
} else if (show_consistency_detail) {
  # Detailed breakdown by consistency
  replication_database_long <- replication_database_long %>%
    mutate(
      group = case_when(
        type == "original" ~ "Original",
        type == "replication" & consistent == TRUE ~ "Replication (consistent)",
        type == "replication" & consistent == FALSE ~
          "Replication (inconsistent)"
      ),
      group = factor(
        group,
        levels = c(
          "Original",
          "Replication (consistent)",
          "Replication (inconsistent)"
        )
      )
    ) %>%
    filter(!is.na(group))

  # Define colors: red for original, dark blue for consistent, light blue for inconsistent
  group_colors <- c(
    "Original" = "#DC143C", # normal red
    "Replication (consistent)" = "#00008B", # dark blue
    "Replication (inconsistent)" = "#ADD8E6" # light blue
  )
} else {
  # Simple original vs replication
  replication_database_long <- replication_database_long %>%
    mutate(
      group = case_when(
        type == "original" ~ "Original",
        type == "replication" ~ "Replication"
      ),
      group = factor(
        group,
        levels = c("Original", "Replication")
      )
    )

  # Define simple colors: red and blue
  group_colors <- c(
    "Original" = "#DC143C", # red
    "Replication" = "#1E90FF" # blue
  )
}

# Plotting
variable_codes <- c("es", "se", "n", "novelty", "truth", "publication_prob")
variable_names <- c(
  "Effect size",
  "Standard error",
  "Sample size",
  "Novelty",
  "Truth",
  "Publication probability"
)
for (variable in 1:length(variable_codes)) {
  p <- ggplot(
    replication_database_long %>% filter(!is.na(significant)),
    aes(x = .data[[variable_codes[variable]]], fill = group)
  ) +
    geom_histogram(
      position = "identity",
      alpha = 0.5,
      bins = 40,
      color = "white"
    ) +
    scale_fill_manual(values = group_colors) +
    theme_minimal() +
    labs(
      title = paste("Distribution of", variable_names[variable], "by Type"),
      x = variable_names[variable],
      y = "Count",
      fill = "Group"
    )

  # Set explicit x-axis range for sample size
  if (variable_codes[variable] == "n") {
    p <- p + xlim(0, 2000) # cuts off a few outliers at ~3000, 4000, and up to 100 000's
  }
  # Set explicit x-axis range for novelty
  if (variable_codes[variable] == "novelty") {
    p <- p + xlim(0, novelty_y_lim)
  }
  # Set explicit x-axis range for truth
  if (variable_codes[variable] == "truth") {
    p <- p + xlim(-truth_y_range, truth_y_range)
  }

  print(p)
}

# Plot per Berinsky (2020)
# Summarize data
plot_data <- replication_database_long %>%
  group_by(type, significant, significant_o) %>%
  summarise(
    mean_pub_prob = mean(publication_prob, na.rm = TRUE),
    se_pub_prob = sd(publication_prob, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )
plot_data <- replication_database_long %>%
  filter(
    !is.na(significant),
    !is.na(significant_o),
    !is.na(publication_prob),
    !is.na(type)
  ) %>%
  group_by(type, significant, significant_o) %>%
  summarise(
    mean_pub_prob = mean(publication_prob, na.rm = TRUE),
    se_pub_prob = sd(publication_prob, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) %>%
  mutate(
    x_label = case_when(
      type == "original" ~ "Combined",
      type == "replication" & significant_o == FALSE ~
        "Originally Insignificant",
      type == "replication" & significant_o == TRUE ~ "Originally Significant"
    ),
    significant = ifelse(significant, "Significant", "Insignificant"),
    type = ifelse(type == "original", "Original Study", "Replication Study")
  )
# Plot publication %
plot <- ggplot(plot_data, aes(x = x_label, y = mean_pub_prob)) +
  geom_point(size = 3) +
  geom_errorbar(
    aes(ymin = mean_pub_prob - se_pub_prob, ymax = mean_pub_prob + se_pub_prob),
    width = 0.1
  ) +
  facet_grid(~ type + significant, scales = "free_x", space = "free_x") +
  labs(
    x = NULL,
    y = "% Would Take Action in Favor of Publication",
    title = NULL
  ) +
  theme_bw(base_size = 14) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.background = element_rect(fill = "grey95", color = "grey70"),
    strip.text = element_text(size = 14),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    axis.title.y = element_text(size = 14)
  )
print(plot)

#print publication probability plot
print(pub_prob)
