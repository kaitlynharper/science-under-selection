##############################################################################
# Run conditions
#
# Description: This file loops through set conditions and saves all results
##############################################################################

library(here)

# Set run conditions parameter (meaningless) so that run_simulations script works with these parameters
run_conditions <- T

# Set lists of condition parameters
initial_selection_condition_list <- c(0, 0, 0, 0, 1, 1, 1, 1)
publication_bias_list <- c(0, 0, 2, 2, 0, 0, 2, 2)
sample_sizes_list <- c(30, 200, 30, 200, 30, 200, 30, 200)
n_timesteps_list <- c(350, 350, 350, 350, 350, 350, 350, 350)
switch_conditions_at_list <- c(NA, NA, NA, NA, NA, NA, NA, NA)

# Loop through each condition
for (i in 1:length(initial_selection_condition_list)) {
  initial_selection_condition <- initial_selection_condition_list[i]
  switch_conditions_at <- switch_conditions_at_list[i]
  publication_bias <- publication_bias_list[i]
  n_timesteps <- n_timesteps_list[i]
  hold_samples_constant_at <- sample_sizes_list[i]
  source(here("R", "run_simulation.R"))
  source(here("R", "analysis.R"))
  # Save the object "patchwork" with the filename being a string of the four parameters above and their values
  ggsave(
    here(
      "output",
      paste0(
        "selection",
        initial_selection_condition,
        "_switch",
        switch_conditions_at,
        "_pub",
        publication_bias,
        "_t",
        n_timesteps,
        "_samplesize",
        hold_samples_constant_at,
        ".png"
      )
    ),
    patchwork,
    width = 30,
    height = 10
  )
}
