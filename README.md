This repository will hold an agent-based model of academia developed to explore the balance between original and replication research with insights from evolutionary dynamics. It is funded by the Deutsche Forschungsgemeinschaft (DFG) as part of the priority programme META-REP, which investigates replicability and reform in scientific research. See the project page here: https://gepris.dfg.de/gepris/projekt/464411255

**Authors**

**Kaitlyn Harper** Postdoctoral researcher at the Department of Psychology, Ludwig-Maximilians-Universität (LMU) Munich. Uses agent-based models programmed in R to simulate academia and explore social and behavioural phenomena in metascience.

**Felix Schönbrodt** Professor at the Department of Psychology at LMU, Director of LMU Open Science Center, Principle Investigator of this project. Leads research on metascience, replicability, and statistics/methods. Personal website: https://www.nicebread.de/

## Repository Structure

```
├── R/
│   ├── model.R                    # Main simulation function
│   ├── run_single_simulation.R    # Run a single simulation
│   ├── run_sweep.R                # Parameter sweeps in parallel
│   ├── analyse_single_simulation.R # Analysis and plots for a single simulation
│   ├── analyse_sweep.R            # Analysis and plots for parameter sweep results
│   ├── functions/                 # Core model functions
│   │   ├── initialize_agents_matrix.R
│   │   ├── initialize_effects_matrix.R
│   │   ├── Initialize_studies_matrix.R
│   │   ├── Add_agents.R
│   │   ├── Run_studies.R
│   │   ├── Run_studies_helpers.R
│   │   ├── Publication_bias.R
│   │   ├── Career_turnover.R
│   │   ├── extract_belief_accuracy.R
│   │   ├── extract_belief_accuracy2.R
│   │   └── PB_plot.R
│   └── playing_around/            # Preliminary/experimental scripts
├── output/                        # Sweep results (RDS) and last_sweep_path.txt
├── docs/                          # Manuscript and documentation (including ODD preregistration document)
└── tests/                         # Unit tests (none yet)
```

## Getting started

### Prerequisites

- R (≥ 4.0)
- Required packages: `here`, `dplyr`, `tidyr`, `pryr`, `profvis`, `testthat`
- For analysis: `ggplot2`, `patchwork`, `rlang`, `scales`
- For parameter sweeps: `lhs`, `foreach`, `doSNOW`

### Running a simulation

The simplest way to run a simulation is to open and execute `R/run_single_simulation.R`, which sources all required functions, sets default parameters, and runs the model. The steps below break down what that script does:

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

(2) Define the parameters of the simulation

See `R/run_single_simulation.R` for the full list of available parameters. Example:

```r
params <- list(
  # Agents and study design
  n_agents = 1000,
  n_timesteps = 500,
  n_timesteps_per_career_step = 35,
  duration_per_observation = 0.1,
  duration_original_intercept = 1,
  # True effects
  n_effects = 500000,
  base_null_probability = 0.9,
  effect_size_mean = 0.3,
  effect_size_variance = 0.1,
  # Collective belief updating
  uninformed_prior_mean = 0,
  uninformed_prior_variance = 1,
  # Career turnover
  initial_selection_condition = 0,   # 0 = truth-based, 1 = novelty-based selection
  switch_conditions_at = NA,
  career_turnover_selection_rate = 0.5,
  innovation_sd = 0,
  mutation_rate = 0.1,
  initial_replication_rate = 0.5,
  hold_samples_constant_at = 50,
  replications_dynamic_sample_sizes = 1,
  publication_bias = 2,              # 0/1/2 = none / weak / strong
  nonsig_logistic_midpoint = NA,
  all_replications_published = 0,
  burn_in_period = 100
)
```

(3) Run the simulation

```r
results <- run_simulation(params)
```

(4) Analyze and visualize the results

- For a single run: use `R/analyse_single_simulation.R` (expects a `results` object in the environment, e.g. from `run_single_simulation.R`).
- For parameter sweeps: run `R/run_sweep.R` to run simulations in parallel and save output to `output/sweep_results_<timestamp>.rds`, then run `R/analyse_sweep.R` to load the latest sweep (or a chosen RDS path) and produce partial dependency plots and other summaries.
- Additional visualisations: `R/playing_around/analysis_Felix.R`.

