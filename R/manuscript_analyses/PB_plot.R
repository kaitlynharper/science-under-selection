##############################################################################
# Publication-bias logistic visualization (for the manuscript)
#
# Description: Draw the logistic publication-probability curves used in the
# model. No simulation output is needed.
#
# Figures:
#   plot_PB() function      — one configuration of all five PB parameters
#   method_pb_figure_plot   — methods figure: significant curve plus the
#                             family of non-significant curves across the
#                             swept midpoint range
#
# Defaults match base_params in the manuscript analysis scripts.
##############################################################################

# Load packages
library(here)
library(ggplot2)

# Source publication bias functions
source(here("R", "functions", "Publication_bias.R"))

##############################################################################
#### DEFAULTS ####
##############################################################################

# Curve parameters (match base_params defaults)
pb_defaults <- list(
  sig_lower_asymptote = 0,
  sig_logistic_midpoint = -0.5,
  sig_logistic_steepness = 3,
  nonsig_logistic_midpoint = 3,
  nonsig_logistic_steepness = 3
)

# Midpoints shown in the methods figure 
method_nonsig_midpoints <- seq(-0.5, 3, 0.5)

##############################################################################
#### HELPERS ####
##############################################################################

# Data for the significant curve and one or more non-significant curves
pb_curve_data <- function(
  sig_lower_asymptote,
  sig_logistic_midpoint,
  sig_logistic_steepness,
  nonsig_logistic_midpoint,
  nonsig_logistic_steepness,
  max_novelty = 5,
  novelty_step = 0.01
) {
  novelty <- seq(0, max_novelty, by = novelty_step)

  # Significant curve
  sig <- data.frame(
    novelty = novelty,
    publication_prob = logistic_significant(
      novelty,
      sig_lower_asymptote,
      sig_logistic_midpoint,
      sig_logistic_steepness
    ),
    result_type = "Significant",
    curve_id = "Significant"
  )

  # Non-significant curves
  nonsig <- expand.grid(
    novelty = novelty,
    midpoint = nonsig_logistic_midpoint,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  nonsig$publication_prob <- logistic_nonsignificant(
    nonsig$novelty,
    nonsig$midpoint,
    nonsig_logistic_steepness
  )
  nonsig$result_type <- "Non-significant"
  nonsig$curve_id <- as.character(nonsig$midpoint)
  nonsig$midpoint <- NULL

  # Combine significant and non-significant curves
  rbind(sig, nonsig)
}

# Helper function to plot the publication bias curves
pb_curve_ggplot <- function(
  data,
  max_novelty,
  title = NULL,
  subtitle = NULL,
  linewidth = 1
) {
  # Plot the publication bias curves
  ggplot(
    data,
    aes(x = novelty, y = publication_prob, linetype = result_type, group = curve_id)
  ) +
    geom_line(color = "black", linewidth = linewidth) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    scale_x_continuous(limits = c(0, max_novelty)) +
    scale_linetype_manual(
      values = c("Significant" = "solid", "Non-significant" = "dotted")
    ) +
    labs(
      title = title,
      subtitle = subtitle,
      x = "Novelty",
      y = "Publication Probability",
      linetype = "Result type"
    ) +
    theme_classic() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
}

##############################################################################
#### SINGLE CONFIGURATION ####
##############################################################################

# One significant curve and one non-significant curve.
# Change any argument to try a different PB setting; defaults match base_params.
plot_PB <- function(
  sig_lower_asymptote = pb_defaults$sig_lower_asymptote,
  sig_logistic_midpoint = pb_defaults$sig_logistic_midpoint,
  sig_logistic_steepness = pb_defaults$sig_logistic_steepness,
  nonsig_logistic_midpoint = pb_defaults$nonsig_logistic_midpoint,
  nonsig_logistic_steepness = pb_defaults$nonsig_logistic_steepness,
  max_novelty = 5,
  subtitle = NULL
) {
  data <- pb_curve_data(
    sig_lower_asymptote,
    sig_logistic_midpoint,
    sig_logistic_steepness,
    nonsig_logistic_midpoint,
    nonsig_logistic_steepness,
    max_novelty = max_novelty
  )
  pb_curve_ggplot(
    data,
    max_novelty = max_novelty,
    title = "Publication Probability by Novelty and Significance",
    subtitle = subtitle,
    linewidth = 1.2
  )
}

##############################################################################
#### METHODS FIGURE ####
##############################################################################

# Significant curve plus a family of non-significant curves across midpoints
plot_PB_method <- function(
  sig_lower_asymptote = pb_defaults$sig_lower_asymptote,
  sig_logistic_midpoint = pb_defaults$sig_logistic_midpoint,
  sig_logistic_steepness = pb_defaults$sig_logistic_steepness,
  nonsig_logistic_midpoints = method_nonsig_midpoints,
  nonsig_logistic_steepness = pb_defaults$nonsig_logistic_steepness,
  max_novelty = 3
) {
  data <- pb_curve_data(
    sig_lower_asymptote,
    sig_logistic_midpoint,
    sig_logistic_steepness,
    nonsig_logistic_midpoints,
    nonsig_logistic_steepness,
    max_novelty = max_novelty
  )
  pb_curve_ggplot(data, max_novelty = max_novelty)
}

method_pb_figure_plot <- plot_PB_method()
method_pb_figure_plot
