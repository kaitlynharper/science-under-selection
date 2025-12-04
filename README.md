This repository will hold an agent-based model of academia developed to explore the balance between original and replication research with insights from evolutionary dynamics. It is funded by the Deutsche Forschungsgemeinschaft (DFG) as part of the priority programme META-REP, which investigates replicability and reform in scientific research. See the project page here: https://gepris.dfg.de/gepris/projekt/464411255

**Authors**

**Kaitlyn Harper** Postdoctoral researcher at the Department of Psychology, Ludwig-Maximilians-Universität (LMU) Munich. Uses agent-based models programmed in R to simulate academia and explore social and behavioural phenomena in metascience.

**Felix Schönbrodt** Professor at the Department of Psychology at LMU, Director of LMU Open Science Center, Principle Investigator of this project. Leads research on metascience, replicability, and statistics/methods. Personal website: https://www.nicebread.de/

## Getting started

[these are preliminary notes]

(1) Source all functions:

```r
function_files <- list.files(here("R", "functions"), full.names = TRUE)
sapply(function_files, source, .GlobalEnv)
source(here("R", "model.R"))
```

(2) Define the parameters of the simulations

See `R/run_simulation.R` - here's a full list of available parameters:

```r
params <- list(
  n_agents = 1000, # number of agents
  n_timesteps = 100, # number of timesteps
  n_timesteps_per_career_step = 10, # number of timesteps per career phase
  n_effects = 20000, # number of effects
  base_null_probability = .9, # base probability of a null effect
  effect_size_mean = .3, # mean effect size
  effect_size_variance = 0.1, # variance of effect sizes
  uninformed_prior_mean = 0, # mean of uninformed prior
  uninformed_prior_variance = 1, # variance of uninformed prior
  duration_per_observation = 0.1, # TODO calibration required # timesteps per observations
  duration_original_intercept = 1, # TODO calibration required # base timesteps for original studies
  # Publication bias parameters # TODO calibration required
  sig_y_intercept = 0.2, # minimum publication probability for p < .05 results
  sig_logistic_midpoint = .5, # novelty midpoint for significant results
  sig_logistic_steepness = 3, # steepness of logistic curve for significant results
  nonsig_logistic_midpoint = .5, # novelty midpoint for non-significant results
  nonsig_logistic_steepness = 3, # steepness of logistic curve for non-significant results
  # Career turnover parameters
  selection_condition = 0, # 0 = selection based on truth, 1 = selection based on novelty
  career_turnover_selection_rate = 0.5, # proportion of agents to retire each career step
  innovation_sd = 0.05, # standard deviation of innovation noise added to new agents
  hold_samples_constant_at = 30, # if NA, sample sizes are calculated; if a value, all studies use that sample size
  publication_bias = 1 #0 = no publication bias, 1 = publication bias
)
```

(3) Run the simulation

```r
results <- run_simulation(params)
```

(4) Analyze and visualize the results

