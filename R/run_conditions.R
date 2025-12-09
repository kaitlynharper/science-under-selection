##############################################################################
# Run conditions
#
# Description: This file loops through set conditions and saves all results
##############################################################################

library(here)

# Set lists of condition parameters
selection_condition_list <- c(0, 0, 1, 1, 1)
switch_conditions_at_list <- c(NA, NA, NA, NA, 200)
publication_bias_list <- c(0, 1, 0, 1, 1)
n_timesteps_list <- c(200, 200, 200, 200, 400)

# Loop through each condition
for (i in 1:length(selection_condition_list)) {
  selection_condition <- selection_condition_list[i]
  switch_conditions_at <- switch_conditions_at_list[i]
  publication_bias <- publication_bias_list[i]
  n_timesteps <- n_timesteps_list[i]
  source(here("R", "run_simulation.R"))
  source(here("R", "playing_around", "analysis_Felix.R"))
  # Save the object "patchwork" with the filename being a string of the four parameters above and their values
  ggsave(here("output", paste0("selection", selection_condition, "_switch", switch_conditions_at, "_pub", publication_bias, "_t", n_timesteps, ".png")), patchwork, width = 30, height = 10)
}
