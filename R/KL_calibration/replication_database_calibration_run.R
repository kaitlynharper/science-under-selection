##############################################################################
# Exploring the replication database
# Description: Explore "The Replication Database" file
# available under CCBY4.0 https://doi.org/10.17605/OSF.IO/9CSP3
##############################################################################

# Parameters
percent_null_effects <- 0.5
true_sd <- 0.01

uninformed_prior_mean <- 0
uninformed_prior_sd <- 0.8

sig_y_intercept <- 0.5
sig_logistic_midpoint <- 0.5
sig_logistic_steepness <- 3

nonsig_logistic_midpoint <- 1.5
nonsig_logistic_steepness <- 3

# Toggle for significance breakdown in histograms
# Only use one or the other
show_significance_detail <- F
show_consistency_detail <- T

novelty_y_lim <- 15
truth_y_range <- 20

# Run calibration script
source(
  "~/Documents/Github/science-under-selection/preregistration/replication_database_calibration.R"
)

# break down by confirmatory/disconfirmatory?
# graph truth contribution scores (assuming randomly the original or replication?)
# try more realistic effects
