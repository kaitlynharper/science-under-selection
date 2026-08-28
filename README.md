This repository holds an agent-based model of academia developed to explore the balance between original and replication research with insights from evolutionary dynamics. It is funded by the Deutsche Forschungsgemeinschaft (DFG) as part of the priority programme META-REP, which investigates replicability and reform in scientific research. See the project page here: https://gepris.dfg.de/gepris/projekt/464411255

**Authors**

**Kaitlyn Harper** Postdoctoral researcher at the Department of Psychology, Ludwig-Maximilians-Universität (LMU) Munich. Uses agent-based models programmed in R to simulate academia and explore social and behavioural phenomena in metascience. Personal website: www.kaitlynharper.com

**Felix Schönbrodt** Professor at the Department of Psychology at LMU, Director of LMU Open Science Center, Principle Investigator of this project. Leads research on metascience, replicability, and statistics/methods. Personal website: www.nicebread.de

## Repository Structure

```
├── R/
│   ├── 00_model.R                      # Main simulation function (run_simulation)
│   ├── 01_run_single_simulation.R      # Run a single simulation
│   ├── 02_analyse_single_simulation.R  # Analysis and plots for a single simulation
│   ├── 03_set_sweep_parameters.R       # Parameter ranges and LHS design for sweeps
│   ├── 04_run_sweep.R                  # Parameter sweeps in parallel
│   ├── 05_analyse_sweep.R              # Analysis and plots for parameter sweep results
│   ├── speed_test.R                    # Timing / performance check
│   ├── functions/                      # Core model functions
│   │   ├── Initialize_agents_matrix.R
│   │   ├── Initialize_effects_matrix.R
│   │   ├── Initialize_studies_matrix.R
│   │   ├── Add_agents.R
│   │   ├── Run_studies.R
│   │   ├── Run_studies_helpers.R
│   │   ├── Publication_bias.R
│   │   └── Career_turnover.R
│   └── manuscript_analyses/            # Scripts and output for manuscript analyses
│       ├── focal_parameter_sweep.R     # Focal LHS batches (sample size, PB, null rate)
│       ├── nonfocal_robustness_sweep.R # Robustness scenarios using focal LHS batch 1
│       ├── publication_bias_sweep.R    # Sweep of publication bias for non-significant results
│       ├── realistic_condition_montecarlo.R  # Stochastic replicates at one realistic setting
│       ├── PB_plot.R                   # Publication-bias logistic visualization
│       ├── make_figures.R              # Manuscript figures (requires all results present)
│       └── output/                     # Saved RDS and logs for the analyses above
│           ├── focal_parameter_sweep/
│           ├── nonfocal_robustness/
│           ├── publication_bias_sweep/
│           └── realistic_condition_montecarlo/
├── output/                             # Exploratory sweep results (RDS) and last_sweep_path.txt
├── docs/                               # Documents (including preregistration)
└── tests/                              # Unit tests (none currently)
```

## Getting started

### Prerequisites

R (≥ 4.0). Install packages with:

```r
install.packages(c(
  "here", "dplyr", "tidyr", "pryr", "profvis",  # running simulations
  "ggplot2", "patchwork", "scales",             # analysis
  "lhs", "foreach", "doSNOW"                    # parameter sweeps
))
```

### Running a simulation

1. Open and run `R/01_run_single_simulation.R`. It sources the model functions, sets default parameters, and calls `run_simulation()`. Edit the `params` list in that file to change a run.
2. With a `results` object in the environment, run `R/02_analyse_single_simulation.R` for time series and study-level plots.

### Running a parameter sweep

1. Set which parameters to sweep, their ranges, and the number of simulations in `R/03_set_sweep_parameters.R`.
2. Run `R/04_run_sweep.R` (it sources `03` automatically if needed). Results are saved to `output/sweep_results_<timestamp>.rds`, and the path is written to `output/last_sweep_path.txt`.
3. Run `R/05_analyse_sweep.R` to load the latest sweep (or set a path) and produce marginal results plots and other summaries.

## Replicating manuscript analyses

Scripts and saved output for the manuscript live in `R/manuscript_analyses/`. Each analysis writes to its own folder under `output/`. If a results file already exists, the script leaves it in place and does not re-run the simulations; delete the files in the relevant output folder if you want a fresh run. Incomplete focal batches and nonfocal scenarios are picked up on the next run.

### Analyses you can run independently

1. Run `focal_parameter_sweep.R` for the main LHS over sample size, publication bias (`nonsig_logistic_midpoint`), and base null probability. The first run writes `lhs_design.rds` which samples the parameter values; later runs fill missing batches and write `focal_sweep_combined.rds` once all batches are present.
2. Run `publication_bias_sweep.R` to sweep publication bias for calibration purposes (Supplementary Material).
3. Run `realistic_condition_montecarlo.R` for many stochastic replicates at one parameter setting (seed change only; no parameter sweep).
4. Run `PB_plot.R` to draw the logistic publication-bias curves for the manuscript (no simulation output needed).

### Analyses with a prerequisite

1. Run `nonfocal_robustness_sweep.R` after the focal sweep has written `lhs_design.rds`. It reuses focal batch 1 of that design and varies one non-focal setting per scenario (optimistic prior, tight prior, large effects, all replications published, strong selection, slow originals, long career window).

### Figures

1. Run `make_figures.R` once the other results are in place. It needs `focal_sweep_combined.rds`, the nonfocal scenario batches, and `realistic_condition_montecarlo.rds`. It uses the publication bias sweep if present, and prints some summaries.
