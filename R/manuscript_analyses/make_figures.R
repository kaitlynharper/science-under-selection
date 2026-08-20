##############################################################################
# Make figures (for the manuscript)
#
# Description: Plots for the manuscript and supplementary material.
# Highlighted-region and realistic-condition Monte Carlo summaries print after
# the figures. Plot objects are assigned in this script's environment and
# printed at the end.
#
# Input folders (run the corresponding simulations first):
#   R/manuscript_analyses/output/focal_parameter_sweep/
#   R/manuscript_analyses/output/nonfocal_robustness/
#   R/manuscript_analyses/output/publication_bias_sweep/
#   R/manuscript_analyses/output/realistic_condition_montecarlo/
#
# Workflow summary:
#   1. Load focal combined + nonfocal scenario batches; build marginal, scatter,
#      heatmap, spaghetti, and batch-stability plots
#   2. Load publication-bias sweep; build publication-composition figure
#   3. Print figures, then highlighted-region and Monte Carlo summaries
#   4. Assign manuscript inline values
#
# Figures produced:
# Main manuscript (focal sweep):
#   fig_marginal_focal_mean_replication_rate
#   fig_marginal_focal_total_scientific_progress
#   fig_scatter_focal_mean_replication_rate
#   fig_scatter_focal_total_scientific_progress
#   fig_heatmap_null_bins_mean_replication_rate
#   fig_heatmap_null_bins_total_scientific_progress
# Publication bias:
#   fig_publication_composition
# Stability results:
#   fig_focal_batch_stability_mean_replication_rate
#   fig_focal_batch_stability_total_scientific_progress
# Sensitivity scenarios (where *scenario_label* is the name of the scenario):
#   fig_spaghetti_marginal_mean_replication_rate
#   fig_spaghetti_marginal_total_scientific_progress
#   fig_marginal_*scenario_label*_mean_replication_rate
#   fig_marginal_*scenario_label*_total_scientific_progress
#   fig_scatter_*scenario_label*_mean_replication_rate
#   fig_scatter_*scenario_label*_total_scientific_progress
# Available scenario labels:
#   optimistic_prior
#   tight_prior
#   large_effects
#   all_reps_published
#   strong_selection
#   slow_originals
#   long_career_window
#
# Manuscript inline values:
#   manuscript_stochastic_variability (n_replicates, mean_pct, sd_pct, se_pct, two_se_pct)
#   manuscript_highlight_replication (mean_pct_033_066, mean_pct_066_1)
#   manuscript_highlight_published_are_replications (mean_pct_033_066, mean_pct_066_1)
#   manuscript_highlight_success_prepub (mean_pct_033_066, mean_pct_066_1)
#   manuscript_highlight_success_postpub (mean_pct_033_066, mean_pct_066_1)
#   manuscript_format_pct()
##############################################################################

# Load packages
library(here)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(scales)

##############################################################################
#### SETUP ####
##############################################################################

# Use the current script environment so figures are available after source()
plot_env <- environment()

# Define outcome variables and axis labels
outcomes <- list(
  list(var = "mean_replication_rate", label = "Replicator share (%)"),
  list(var = "total_scientific_progress", label = "Total scientific progress")
)

# Define realistic condition parameters for the highlighted regions
highlight_region <- list(
  null_bins = c("0.33-0.66", "0.66-1"),
  sample_size = c(20, 40),
  nonsig_logistic_midpoint = c(2, 3)
)

# Define colours for spaghetti overlays (one colour per scenario)
scenario_palette <- c(
  "Focal sweep" = "black",
  "Optimistic prior" = "#0072B2",
  "Tight prior" = "#D55E00",
  "Large effects" = "#009E73",
  "All replications published" = "#CC79A7",
  "Strong selection" = "#E69F00",
  "Slow originals" = "#56B4E9",
  "Long career window" = "#7B4F9E"
)
focal_scenario_label <- "Focal sweep"

##############################################################################
#### PATHS ####
##############################################################################

# Focal sweep (combined file + optional per-batch files for stability plots)
focal_dir <- here(
  "R",
  "manuscript_analyses",
  "output",
  "focal_parameter_sweep"
)
focal_path <- file.path(focal_dir, "focal_sweep_combined.rds")

# Nonfocal robustness scenarios (one batch file per scenario)
nonfocal_dir <- here(
  "R",
  "manuscript_analyses",
  "output",
  "nonfocal_robustness"
)

# Publication-bias sweep
publication_bias_path <- here(
  "R",
  "manuscript_analyses",
  "output",
  "publication_bias_sweep",
  "publication_bias_sweep.rds"
)

# Realistic-condition Monte Carlo
realistic_montecarlo_path <- here(
  "R",
  "manuscript_analyses",
  "output",
  "realistic_condition_montecarlo",
  "realistic_condition_montecarlo.rds"
)

# Require the combined focal file and at least one nonfocal scenario batch
if (!file.exists(focal_path)) {
  stop("Focal sweep not found: ", focal_path)
}

focal_batch_files <- sort(list.files(
  focal_dir,
  pattern = "^batch_\\d+\\.rds$",
  full.names = TRUE
))

nonfocal_batch_files <- sort(list.files(
  nonfocal_dir,
  pattern = "^batch_.+\\.rds$",
  full.names = TRUE
))
if (length(nonfocal_batch_files) == 0L) {
  stop("No nonfocal scenario batch files in ", nonfocal_dir)
}

# Dataset list: focal combined, then one entry per nonfocal scenario batch
datasets <- c(
  list(list(id = "focal", path = focal_path)),
  lapply(nonfocal_batch_files, function(path) {
    list(
      id = sub("^batch_(.+)\\.rds$", "\\1", basename(path)),
      path = path
    )
  })
)

##############################################################################
#### HELPERS ####
##############################################################################

# Map a 0-1 grid back onto a parameter's original range (log if needed)
denorm_param_grid <- function(x_norm, spec) {
  if (isTRUE(spec$log_scale)) {
    log_min <- log(spec$min)
    log_max <- log(spec$max)
    exp(log_min + x_norm * (log_max - log_min))
  } else {
    spec$min + x_norm * (spec$max - spec$min)
  }
}

# Partial-dependence (loess) curves, one per swept parameter
marginal_pdp_data <- function(
  sweep_results,
  param_config,
  outcome_var,
  dataset_label
) {
  param_names <- names(param_config)
  param_labels <- vapply(param_config, `[[`, character(1L), "label")
  param_ranges <- lapply(param_config, function(x) c(x$min, x$max))

  # Normalize each swept parameter to [0, 1] for marginal plots
  sweep_norm <- sweep_results
  for (i in seq_along(param_names)) {
    r <- param_ranges[[i]]
    sweep_norm[[paste0(param_names[i], "_norm")]] <-
      (sweep_results[[param_names[i]]] - r[1]) / (r[2] - r[1])
  }

  # Fit a loess on the normalized axis, then map the grid back to original units
  pdp_data <- data.frame()
  for (i in seq_along(param_names)) {
    norm_col <- paste0(param_names[i], "_norm")
    fit <- loess(
      as.formula(paste(outcome_var, "~ get(norm_col)")),
      data = sweep_norm,
      span = 0.75
    )
    grid_x <- seq(0, 1, length.out = 100)
    pdp_data <- rbind(
      pdp_data,
      data.frame(
        dataset = dataset_label,
        param = param_labels[i],
        x_norm = grid_x,
        x = denorm_param_grid(grid_x, param_config[[i]]),
        y = {
          pred <- predict(fit, newdata = setNames(data.frame(grid_x), norm_col))
          if (outcome_var == "mean_replication_rate") 100 * pred else pred
        }
      )
    )
  }
  pdp_data
}

# Function: make a combined marginal plot (all params, x-axis normalized)
make_marginal_plot <- function(pdp_data, param_config, outcome_label, title) {
  param_labels <- vapply(param_config, `[[`, character(1L), "label")
  param_colors <- vapply(param_config, `[[`, character(1L), "color")

  ggplot(pdp_data, aes(x = x_norm, y = y, color = param)) +
    geom_line(linewidth = 1.2) +
    scale_color_manual(values = setNames(param_colors, param_labels)) +
    labs(
      title = title,
      x = "Parameter Value (normalized)",
      y = outcome_label,
      color = NULL
    ) +
    theme_classic() +
    theme(legend.position = "bottom")
}

# Function: make a scatterplot, one panel per parameter
make_scatter_plot <- function(
  sweep_results,
  param_config,
  outcome_var,
  outcome_label,
  title
) {
  param_names <- names(param_config)
  param_labels <- vapply(param_config, `[[`, character(1L), "label")
  param_colors <- vapply(param_config, `[[`, character(1L), "color")
  n_params <- length(param_names)

  # Convert replicator share to percent for plotting
  plot_results <- sweep_results
  if (outcome_var == "mean_replication_rate") {
    plot_results[[outcome_var]] <- 100 * plot_results[[outcome_var]]
  }

  scatter_list <- lapply(seq_len(n_params), function(i) {
    ggplot(
      plot_results,
      aes(x = .data[[param_names[i]]], y = .data[[outcome_var]])
    ) +
      geom_point(color = param_colors[i], alpha = 0.6) +
      geom_smooth(
        method = "loess",
        se = FALSE,
        color = param_colors[i],
        span = 0.75
      ) +
      labs(x = param_labels[i], y = outcome_label) +
      theme_classic()
  })

  wrap_plots(scatter_list, ncol = min(3L, n_params)) +
    plot_annotation(title = title)
}

# Overlay all scenarios' marginals on the original (denormalized) x-axis
make_spaghetti_marginal_plot <- function(pdp_data, outcome_label) {
  scenario_levels <- names(scenario_palette)[
    names(scenario_palette) %in% unique(pdp_data$dataset)
  ]
  plot_data <- pdp_data |>
    mutate(dataset = factor(dataset, levels = scenario_levels))

  # Draw non-focal scenarios first so the focal line sits on top
  pdp_other <- plot_data |> filter(dataset != focal_scenario_label)
  pdp_focal <- plot_data |> filter(dataset == focal_scenario_label)

  # Plot the spaghetti marginal plot
  ggplot() +
    geom_line(
      data = pdp_other,
      aes(
        x = x,
        y = y,
        color = dataset,
        group = interaction(dataset, param)
      ),
      linewidth = 0.9
    ) +
    geom_line(
      data = pdp_focal,
      aes(
        x = x,
        y = y,
        color = dataset,
        group = interaction(dataset, param)
      ),
      linewidth = 1.2
    ) +
    scale_color_manual(values = scenario_palette, breaks = names(scenario_palette)) +
    facet_wrap(~param, scales = "free_x") +
    labs(
      title = "All scenarios",
      x = NULL,
      y = outcome_label,
      color = NULL
    ) +
    theme_classic() +
    theme(legend.position = "bottom")
}

# Per-batch marginals vs pooled-all-sims line (focal sweep only)
make_focal_batch_stability_plot <- function(
  batch_pdp,
  pooled_pdp,
  outcome_label,
  n_batches
) {
  p <- ggplot() +
    geom_line(
      data = batch_pdp,
      aes(
        x = x,
        y = y,
        color = dataset,
        group = interaction(dataset, param)
      ),
      linewidth = 0.7,
      alpha = 0.55
    ) +
    geom_line(
      data = pooled_pdp,
      aes(x = x, y = y, group = param),
      color = "black",
      linewidth = 1.3
    ) +
    facet_wrap(~param, scales = "free_x") +
    labs(
      title = sprintf("Focal sweep batch stability (%d batches)", n_batches),
      x = NULL,
      y = outcome_label,
      color = "Batch"
    ) +
    theme_classic() +
    theme(legend.position = "bottom")

  # Hide the batch legend when there are too many batches to read
  if (n_batches > 6L) {
    p <- p + guides(color = "none")
  }
  p
}

# Function: make a heatmap of sample size × publication bias, one panel per base-null third
make_null_bin_heatmaps <- function(
  sweep_results,
  param_config,
  outcome_var,
  outcome_label,
  title,
  highlight_region = NULL
) {
  x_var <- "nonsig_logistic_midpoint"
  y_var <- "hold_samples_constant_at"
  null_labels <- c("0-0.33", "0.33-0.66", "0.66-1")

  # Bin simulations into thirds of base null probability
  plot_data <- sweep_results |>
    mutate(
      null_bin = cut(
        base_null_probability,
        breaks = c(0, 1 / 3, 2 / 3, 1),
        labels = null_labels,
        include.lowest = TRUE
      )
    ) |>
    filter(!is.na(null_bin), !is.na(.data[[outcome_var]]))

  # Convert replicator share to percent for the fill scale
  if (outcome_var == "mean_replication_rate") {
    plot_data[[outcome_var]] <- 100 * plot_data[[outcome_var]]
  }

  # Red rectangle marks the highlighted region on the relevant panels
  highlight_df <- if (is.null(highlight_region)) {
    NULL
  } else {
    data.frame(
      null_bin = highlight_region$null_bins,
      xmin = min(highlight_region$nonsig_logistic_midpoint),
      xmax = max(highlight_region$nonsig_logistic_midpoint),
      ymin = min(highlight_region$sample_size),
      ymax = max(highlight_region$sample_size)
    )
  }

  fill_limits <- if (outcome_var == "mean_replication_rate") {
    c(0, 100)
  } else {
    range(plot_data[[outcome_var]], na.rm = TRUE)
  }

  # Define the breaks for the heatmap
  heatmap_breaks <- list(
    x = seq(param_config[[x_var]]$min, param_config[[x_var]]$max, length.out = 17),
    y = seq(param_config[[y_var]]$min, param_config[[y_var]]$max, length.out = 17)
  )

  # Make a panel for each base-null third
  panels <- lapply(null_labels, function(lbl) {
    panel_plot <- ggplot(
      filter(plot_data, null_bin == lbl),
      aes(
        x = .data[[x_var]],
        y = .data[[y_var]],
        z = .data[[outcome_var]]
      )
    ) +
      stat_summary_2d(
        fun = function(z) mean(z, na.rm = TRUE),
        breaks = heatmap_breaks
      ) +
      coord_cartesian(expand = FALSE) +
      scale_fill_viridis_c(
        option = "D",
        limits = fill_limits,
        oob = scales::squish
      ) +
      labs(
        title = paste("Base null:", lbl),
        x = param_config[[x_var]]$label,
        y = param_config[[y_var]]$label,
        fill = outcome_label
      ) +
      theme_classic()

    panel_highlight <- if (is.null(highlight_df)) {
      NULL
    } else {
      filter(highlight_df, null_bin == lbl)
    }

    # Add a red rectangle to the panel if the highlighted region is defined
    if (!is.null(panel_highlight) && nrow(panel_highlight) > 0L) {
      panel_plot <- panel_plot +
        geom_rect(
          data = panel_highlight,
          aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
          inherit.aes = FALSE,
          color = "red",
          fill = NA,
          linewidth = 0.8,
          linetype = "solid"
        )
    }

    panel_plot
  })

  wrap_plots(panels, ncol = 3, guides = "collect") +
    plot_annotation(title = title) &
    theme(legend.position = "bottom")
}

# Function: make a plot of published-literature composition vs publication-bias parameter
make_publication_composition_plot <- function(sweep_results) {
  composition_vars <- c(
    pct_published_originals_sig = "Published originals that are significant",
    pct_published_replications_sig = "Published replications that are significant",
    pct_published_are_replications = "Published studies that are replications"
  )

  # Check that the publication bias sweep results have the required columns
  missing_vars <- setdiff(names(composition_vars), names(sweep_results))
  if (length(missing_vars) > 0L) {
    stop(
      "Publication bias sweep is missing outcome columns: ",
      paste(missing_vars, collapse = ", "),
      "\nRe-run R/manuscript_analyses/publication_bias_sweep.R ",
      "(delete publication_bias_sweep.rds first if it exists but is empty)."
    )
  }
  if (nrow(sweep_results) == 0L) {
    stop(
      "Publication bias sweep has no rows. ",
      "Re-run publication_bias_sweep.R after fixing base_params."
    )
  }

  # Pivot the publication bias sweep results to long format
  plot_data <- sweep_results |>
    pivot_longer(
      cols = all_of(names(composition_vars)),
      names_to = "metric",
      values_to = "pct"
    ) |>
    mutate(metric = composition_vars[metric])

  # Plot the publication composition plot
  ggplot(
    plot_data,
    aes(x = nonsig_logistic_midpoint, y = pct, color = metric)
  ) +
    geom_hline(yintercept = c(1, 96), linetype = "dotted", color = "black") +
    geom_point(alpha = 0.25, size = 1.2) +
    geom_smooth(method = "loess", se = FALSE, linewidth = 1.1, span = 0.75) +
    scale_color_manual(
      values = c(
        "#0072B2",
        "#D55E00",
        "#009E73"
      )
    ) +
    labs(
      title = "Published literature composition vs publication bias",
      x = "Publication bias parameter",
      y = "Percent of studies",
      color = NULL
    ) +
    scale_y_continuous(limits = c(0, 100)) +
    theme_classic() +
    theme(legend.position = "right")
}

##############################################################################
#### MARGINAL AND SCATTER PLOTS ####
##############################################################################

# Collect plot names in the order they should print, and accumulate spaghetti data
plot_order <- character(0)
spaghetti_pdp <- setNames(
  vector("list", length(outcomes)),
  vapply(outcomes, `[[`, "", "var")
)

# One pair of figures (marginal + scatter) per dataset × outcome
for (dataset in datasets) {
  sweep_output <- readRDS(dataset$path)
  sweep_results <- sweep_output$results
  param_config <- sweep_output$meta$param_config
  dataset_label <- if (dataset$id == "focal") {
    focal_scenario_label
  } else {
    sweep_output$meta$scenario_label
  }
  if (dataset$id == "focal") {
    focal_n_timesteps <- sweep_output$meta$base_params$n_timesteps
  }

  for (outcome in outcomes) {
    outcome_var <- outcome$var
    outcome_label <- outcome$label
    fig_suffix <- paste(dataset$id, outcome_var, sep = "_")

    pdp_data <- marginal_pdp_data(
      sweep_results,
      param_config,
      outcome_var,
      dataset_label
    )

    # Scale progress to the focal sweep's duration so spaghetti axes match
    spaghetti_row <- pdp_data
    if (outcome_var == "total_scientific_progress") {
      spaghetti_row$y <- spaghetti_row$y *
        focal_n_timesteps / sweep_output$meta$base_params$n_timesteps
    }
    spaghetti_pdp[[outcome_var]] <- rbind(
      spaghetti_pdp[[outcome_var]],
      spaghetti_row
    )

    p_marginal <- make_marginal_plot(
      pdp_data,
      param_config,
      outcome_label,
      dataset_label
    )
    marginal_name <- paste0("fig_marginal_", fig_suffix)
    assign(marginal_name, p_marginal, envir = plot_env)
    plot_order <- c(plot_order, marginal_name)

    p_scatter <- make_scatter_plot(
      sweep_results,
      param_config,
      outcome_var,
      outcome_label,
      dataset_label
    )
    scatter_name <- paste0("fig_scatter_", fig_suffix)
    assign(scatter_name, p_scatter, envir = plot_env)
    plot_order <- c(plot_order, scatter_name)
  }
}

##############################################################################
#### SPAGHETTI MARGINAL OVERLAYS ####
##############################################################################

# One overlay per outcome: all scenarios on the same (denormalized) axes
for (outcome in outcomes) {
  outcome_var <- outcome$var
  outcome_label <- outcome$label
  p_spaghetti <- make_spaghetti_marginal_plot(
    spaghetti_pdp[[outcome_var]],
    outcome_label
  )
  spaghetti_name <- paste0("fig_spaghetti_marginal_", outcome_var)
  assign(spaghetti_name, p_spaghetti, envir = plot_env)
  plot_order <- c(plot_order, spaghetti_name)
}

##############################################################################
#### FOCAL BATCH STABILITY ####
##############################################################################

# Overlay each focal batch's marginal fit with the pooled-all-sims line
if (length(focal_batch_files) > 0L) {
  focal_pooled <- readRDS(focal_path)
  focal_pooled_results <- focal_pooled$results
  focal_param_config <- focal_pooled$meta$param_config

  for (outcome in outcomes) {
    outcome_var <- outcome$var
    outcome_label <- outcome$label

    batch_pdp <- do.call(
      rbind,
      lapply(focal_batch_files, function(path) {
        batch_id <- sub("^batch_(.+)\\.rds$", "\\1", basename(path))
        batch_output <- readRDS(path)
        marginal_pdp_data(
          batch_output$results,
          batch_output$meta$param_config,
          outcome_var,
          paste0("Batch ", batch_id)
        )
      })
    )

    pooled_pdp <- marginal_pdp_data(
      focal_pooled_results,
      focal_param_config,
      outcome_var,
      "Pooled"
    )
    pooled_pdp$dataset <- NULL

    p_batch_stability <- make_focal_batch_stability_plot(
      batch_pdp,
      pooled_pdp,
      outcome_label,
      length(focal_batch_files)
    )
    batch_stability_name <- paste0("fig_focal_batch_stability_", outcome_var)
    assign(batch_stability_name, p_batch_stability, envir = plot_env)
    plot_order <- c(plot_order, batch_stability_name)
  }
} else {
  message(
    "Skipping focal batch-stability plots: no batch_XX.rds files in ",
    focal_dir
  )
}

##############################################################################
#### NULL-BIN HEATMAPS ####
##############################################################################

# Sample size × publication bias, faceted by base-null third (focal sweep only)
focal_for_heatmaps <- readRDS(focal_path)
for (outcome in outcomes) {
  heatmap_name <- paste0("fig_heatmap_null_bins_", outcome$var)
  assign(
    heatmap_name,
    make_null_bin_heatmaps(
      focal_for_heatmaps$results,
      focal_for_heatmaps$meta$param_config,
      outcome$var,
      outcome$label,
      "Focal sweep",
      highlight_region = highlight_region
    ),
    envir = plot_env
  )
  plot_order <- c(plot_order, heatmap_name)
}

##############################################################################
#### PUBLICATION COMPOSITION ####
##############################################################################

if (file.exists(publication_bias_path)) {
  pub_bias_output <- readRDS(publication_bias_path)
  fig_publication_composition <- make_publication_composition_plot(
    pub_bias_output$results
  )
  assign(
    "fig_publication_composition",
    fig_publication_composition,
    envir = plot_env
  )
  plot_order <- c(plot_order, "fig_publication_composition")
} else {
  message(
    "Skipping publication composition plot: ",
    publication_bias_path,
    " not found."
  )
}

##############################################################################
#### PRINT FIGURES ####
##############################################################################

for (plot_name in plot_order) {
  print(get(plot_name, envir = plot_env))
}

##############################################################################
#### HIGHLIGHTED REGION SUMMARY ####
##############################################################################

# Restrict focal sims to the red boxes on the null-bin heatmaps
highlight_sims <- focal_for_heatmaps$results |>
  mutate(
    null_bin = cut(
      base_null_probability,
      breaks = c(0, 1 / 3, 2 / 3, 1),
      labels = c("0-0.33", "0.33-0.66", "0.66-1"),
      include.lowest = TRUE
    )
  ) |>
  filter(
    null_bin %in% highlight_region$null_bins,
    between(
      nonsig_logistic_midpoint,
      min(highlight_region$nonsig_logistic_midpoint),
      max(highlight_region$nonsig_logistic_midpoint)
    ),
    between(
      hold_samples_constant_at,
      min(highlight_region$sample_size),
      max(highlight_region$sample_size)
    )
  )

# Mean/sd/min/max for one outcome, by null bin and combined
summarise_highlight_region <- function(data, var, outcome_label, as_proportion = FALSE) {
  vals <- if (as_proportion) 100 * data[[var]] else data[[var]]
  bind_rows(
    data |>
      mutate(.value = vals) |>
      group_by(null_bin) |>
      summarise(
        outcome = outcome_label,
        n = n(),
        mean = mean(.value, na.rm = TRUE),
        sd = sd(.value, na.rm = TRUE),
        min = min(.value, na.rm = TRUE),
        max = max(.value, na.rm = TRUE),
        .groups = "drop"
      ),
    data |>
      mutate(.value = vals) |>
      summarise(
        null_bin = "combined",
        outcome = outcome_label,
        n = n(),
        mean = mean(.value, na.rm = TRUE),
        sd = sd(.value, na.rm = TRUE),
        min = min(.value, na.rm = TRUE),
        max = max(.value, na.rm = TRUE)
      )
  )
}

highlight_region_summary <- bind_rows(
  summarise_highlight_region(
    highlight_sims,
    "mean_replication_rate",
    "Replicator share (%)",
    as_proportion = TRUE
  ),
  summarise_highlight_region(
    highlight_sims,
    "pct_published_are_replications",
    "Published studies that are replications (%)"
  ),
  summarise_highlight_region(
    highlight_sims,
    "rep_success_prepub",
    "Replication success, pre-publication (%)"
  ),
  summarise_highlight_region(
    highlight_sims,
    "rep_success_postpub",
    "Replication success, post-publication (%)"
  )
) |>
  select(outcome, null_bin, n, mean, sd, min, max)

cat(
  "\n=== Focal sweep highlighted region summary ===\n\n",
  "Simulations in the red boxes on the focal null-bin heatmaps: base null probability\n",
  "in the middle and upper thirds (0.33-0.66 and 0.66-1), publication bias 2-3,\n",
  "and sample size 40-50. Summarises replicator share (%), % of published studies\n",
  "that are replications, and replication success (pre- and post-publication) across\n",
  "focal sweep LHS points in that region, by null bin and combined.\n\n",
  sep = ""
)

print(highlight_region_summary)

##############################################################################
#### MANUSCRIPT INLINE VALUES ####
##############################################################################

# Pull the two highlighted null-bin means for a manuscript inline value
manuscript_highlight_null_bins <- function(summary, outcome_label) {
  bins <- summary |>
    filter(
      outcome == outcome_label,
      null_bin %in% highlight_region$null_bins
    )
  list(
    mean_pct_033_066 = bins$mean[bins$null_bin == "0.33-0.66"],
    mean_pct_066_1 = bins$mean[bins$null_bin == "0.66-1"]
  )
}

manuscript_highlight_replication <- manuscript_highlight_null_bins(
  highlight_region_summary,
  "Replicator share (%)"
)
manuscript_highlight_published_are_replications <- manuscript_highlight_null_bins(
  highlight_region_summary,
  "Published studies that are replications (%)"
)
manuscript_highlight_success_prepub <- manuscript_highlight_null_bins(
  highlight_region_summary,
  "Replication success, pre-publication (%)"
)
manuscript_highlight_success_postpub <- manuscript_highlight_null_bins(
  highlight_region_summary,
  "Replication success, post-publication (%)"
)

# Format a number as a percentage string for the manuscript
manuscript_format_pct <- function(x, digits = 0) {
  paste0(format(round(x, digits), nsmall = digits, trim = TRUE), "%")
}

##############################################################################
#### REALISTIC CONDITION MONTE CARLO ####
##############################################################################

# Monte Carlo summary (mean, sd, MCSE, ± 2*MCSE)
monte_carlo_stats <- function(x, label, as_percent = FALSE) {
  if (as_percent) {
    x <- 100 * x
  }
  x <- x[!is.na(x)]
  n <- length(x)
  s <- sd(x)
  m <- mean(x)
  mcse <- s / sqrt(n)
  half_width <- 2 * mcse
  data.frame(
    outcome = label,
    n = n,
    mean = m,
    sd = s,
    min = min(x),
    max = max(x),
    monte_carlo_se = mcse,
    relative_mc_error_pct = if (m != 0) 100 * mcse / abs(m) else NA_real_,
    ci_lower = m - half_width,
    ci_upper = m + half_width,
    estimate = sprintf("%s ± %s", signif(m, 4), signif(half_width, 4)),
    row.names = NULL
  )
}

if (file.exists(realistic_montecarlo_path)) {
  montecarlo_results <- readRDS(realistic_montecarlo_path)$results
  mc_summary <- rbind(
    monte_carlo_stats(
      montecarlo_results$mean_replication_rate,
      "Replicator share (%)",
      as_percent = TRUE
    ),
    monte_carlo_stats(
      montecarlo_results$total_scientific_progress,
      "Total scientific progress"
    )
  )

  cat(
    "\n=== Realistic condition Monte Carlo summary ===\n\n",
    "Monte Carlo standard error (MCSE): sd(replicates) / sqrt(n). Simulation noise\n",
    "in the estimated mean across stochastic replicates (seed-only variation).\n\n",
    "Relative MC error (%): 100 * MCSE / |mean|. MC precision relative to the\n",
    "estimated mean; smaller values mean the mean is estimated more precisely.\n\n",
    "Mean ± 2*MCSE: approximate 95% uncertainty interval for the Monte Carlo mean\n",
    "(mean minus/plus twice the MCSE). Reported as 'estimate' below.\n\n",
    sep = ""
  )

  print(mc_summary, digits = 4, row.names = FALSE)

  cat("\nEstimates (mean ± 2*MCSE):\n")
  for (i in seq_len(nrow(mc_summary))) {
    cat(
      "  ",
      mc_summary$outcome[i],
      ": ",
      mc_summary$estimate[i],
      "\n",
      sep = ""
    )
  }
  cat("\n")

  # Store replicator-share stats for manuscript inline values
  rep_stats <- mc_summary[1, ]
  manuscript_stochastic_variability <- list(
    n_replicates = rep_stats$n,
    mean_pct = rep_stats$mean,
    sd_pct = rep_stats$sd,
    se_pct = rep_stats$monte_carlo_se,
    two_se_pct = 2 * rep_stats$monte_carlo_se
  )
} else {
  message(
    "Skipping realistic condition Monte Carlo summary: ",
    realistic_montecarlo_path,
    " not found. Run R/manuscript_analyses/realistic_condition_montecarlo.R."
  )
  stop(
    "manuscript_stochastic_variability not available. ",
    "Run R/manuscript_analyses/realistic_condition_montecarlo.R first."
  )
}
