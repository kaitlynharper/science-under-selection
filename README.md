This repository will hold an agent-based model of academia developed to explore the balance between original and replication research with insights from evolutionary dynamics. It is funded by the Deutsche Forschungsgemeinschaft (DFG) as part of the priority programme META-REP, which investigates replicability and reform in scientific research. See the project page here: https://gepris.dfg.de/gepris/projekt/464411255

**Authors**

**Kaitlyn Harper** Postdoctoral researcher at the Department of Psychology, Ludwig-Maximilians-Universität (LMU) Munich. Uses agent-based models programmed in R to simulate academia and explore social and behavioural phenomena in metascience.

**Felix Schönbrodt** Professor at the Department of Psychology at LMU, Director of LMU Open Science Center, Principle Investigator of this project. Leads research on metascience, replicability, and statistics/methods. Personal website: https://www.nicebread.de/

## Repository Structure

```
├── R/
│   ├── model.R              # Main simulation function (run_simulation)
│   ├── run_simulation.R     # Run a single simulation with parameters
│   ├── run_conditions.R     # Batch runs across parameter combinations
│   ├── analysis.R           # Visualization and analysis functions
│   ├── functions/           # Core model components
│   │   ├── initialize_agents_matrix.R
│   │   ├── initialize_effects_matrix.R
│   │   ├── Initialize_studies_matrix.R
│   │   ├── Add_agents.R
│   │   ├── Run_studies.R
│   │   ├── Run_studies_helpers.R
│   │   ├── Publication_bias.R
│   │   ├── Career_turnover.R
│   │   └── extract_belief_accuracy.R
│   └── playing_around/      # Preliminary/experimental scripts
│       └── analysis_Felix.R # Most developed analysis script
├── docs/                    # Manuscript and documentation (including ODD preregistration document)
└── tests/                   # Unit tests (none yet) 
```

## Getting started

### Prerequisites

- R (≥ 4.0)
- Required packages: `here`, `dplyr`, `tidyr`, `pryr`, `profvis`, `testthat`
- For analysis: `ggplot2`, `patchwork`, `rlang`

### Running a simulation

The simplest way to run a simulation is to open and execute `R/run_simulation.R`, which sources all required functions, sets default parameters, and runs the model. The steps below break down what that script does:

(1) Load packages and source all functions:

```r
library(here)
library(pryr)
library(profvis)
library(tidyr)
library(dplyr)

function_files <- list.files(here("R", "functions"), full.names = TRUE)
sapply(function_files, source, .GlobalEnv)
source(here("R", "model.R"))
```

(2) Define the parameters of the simulations

See `R/run_simulation.R` for a full list of available parameters:

```r
params <- list(
  # Parameters for agents and study design
  n_agents = 500, # number of agents
  n_timesteps = 300, # number of timesteps
  n_timesteps_per_career_step = 35, # number of timesteps per career phase
  duration_per_observation = 0.1, # timesteps per observation
  duration_original_intercept = 1, # base timesteps for original studies

  # Parameters for true effects
  n_effects = 500000, # number of effects
  base_null_probability = .9, # base probability of a null effect
  effect_size_mean = .3, # mean effect size
  effect_size_variance = 0.1, # variance of effect sizes

  # Parameters for collective belief updating
  uninformed_prior_mean = 0, # mean of uninformed prior
  uninformed_prior_variance = 1, # variance of uninformed prior

  # Career turnover parameters
  initial_selection_condition = 1, # 0 = selection based on truth, 1 = selection based on novelty
  switch_conditions_at = NA, # if NA, no switch; if a value, condition switches at that timestep
  career_turnover_selection_rate = 0.5, # proportion of agents to retire each career step
  innovation_sd = 0, # standard deviation of innovation noise added to new agents
  hold_samples_constant_at = 30, # if NA, sample sizes are calculated; if a value, all studies use that sample size
  publication_bias = 1 # 0 = no publication bias, 1 = weak publication bias, 2 = strong publication bias
)
```

(3) Run the simulation

```r
results <- run_simulation(params)
```

(4) Analyze and visualize the results

Analysis scripts have not yet been finalized. For the most detailed visualisations, run `R/playing_around/analysis_Felix.R`.

