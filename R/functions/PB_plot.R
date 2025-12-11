#' Visualize the logistic functions for publication bias

#' @param sig_y_intercept minimum publication probability for p < .05 results
#' @param sig_logistic_midpoint novelty midpoint for significant results
#' @param sig_logistic_steepness steepness of logistic curve for significant results
#' @param nonsig_logistic_midpoint novelty midpoint for non-significant results
#' @param nonsig_logistic_steepness steepness of logistic curve for non-significant 

# Medium PB:
plot_PB(
  sig_y_intercept = .5, sig_logistic_midpoint = 0.5, 
  sig_logistic_steepness = 3, nonsig_logistic_midpoint = 1.5, 
  nonsig_logistic_steepness = 3, subtitle="Medium PB")

# Strong PB:
plot_PB(
  sig_y_intercept = 0.8, sig_logistic_midpoint = 0.2, 
  sig_logistic_steepness = 3, nonsig_logistic_midpoint = 2.5, 
  nonsig_logistic_steepness = 3, subtitle="Strong PB")



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
    geom_line(color = "black", linewidth = 1.5) +
    theme_minimal() +
    labs(
      title = "Publication Probability by Novelty and Significance",
      subtitle = subtitle,
      x = "Novelty",
      y = "Publication Probability",
      linetype = "Result Type"
    ) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
    scale_linetype_manual(values = c("dotted", "solid")) +
    scale_x_continuous(limits = c(0, max_novelty))

  pub_prob
}
