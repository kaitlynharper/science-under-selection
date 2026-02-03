##########################################################################
# Visualize the logistic functions for publication bias
##########################################################################

# Source helper functions (logistic_significant, logistic_nonsignificant)
source(here::here("R", "functions", "Publication_bias.R"))
library(ggplot2)

#' @param sig_y_intercept minimum publication probability for p < .05 results
#' @param sig_logistic_midpoint novelty midpoint for significant results
#' @param sig_logistic_steepness steepness of logistic curve for significant results
#' @param nonsig_logistic_midpoint novelty midpoint for non-significant results
#' @param nonsig_logistic_steepness steepness of logistic curve for non-significant 

plot_PB <- function(sig_y_intercept, sig_logistic_midpoint, sig_logistic_steepness, nonsig_logistic_midpoint, nonsig_logistic_steepness, max_novelty = 5, subtitle=NA) {
  
  novelty_range <- seq(0, max_novelty, by = 0.01)

  viz_data <- data.frame(
    novelty = rep(novelty_range, 2),
    publication_prob = c(
      logistic_significant(novelty_range, sig_y_intercept, sig_logistic_midpoint, sig_logistic_steepness),
      logistic_nonsignificant(novelty_range, nonsig_logistic_midpoint, nonsig_logistic_steepness)
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
    geom_line(color = "black", linewidth = 1.2) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    scale_linetype_manual(values = c("dotted", "solid")) +
    scale_x_continuous(limits = c(0, max_novelty)) +
    labs(
      title = "Publication Probability by Novelty and Significance",
      subtitle = subtitle,
      x = "Novelty",
      y = "Publication Probability",
      linetype = NULL
    ) +
    theme_classic() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )

  pub_prob
}

# # Medium PB:
# plot_PB(
#   sig_y_intercept = .5, sig_logistic_midpoint = 0.5, 
#   sig_logistic_steepness = 3, nonsig_logistic_midpoint = 1.5, 
#   nonsig_logistic_steepness = 3, subtitle="Medium PB")

# # Strong PB:
# plot_PB(
#   sig_y_intercept = 0.8, sig_logistic_midpoint = 0.2,
#   sig_logistic_steepness = 3, nonsig_logistic_midpoint = 3,
#   nonsig_logistic_steepness = 3, subtitle="Strong PB")

# # Playing around:
# for(publication_bias_param in seq(-0.5, 5, 0.5)){
# plot <- plot_PB(
#   sig_y_intercept = 0.8, sig_logistic_midpoint = 0.2,
#   sig_logistic_steepness = 3, nonsig_logistic_midpoint = publication_bias_param,
#   nonsig_logistic_steepness = 3, subtitle=paste0("Publication bias parameter = ", publication_bias_param))
# print(plot)
# }