##############################################################################
# Analyze sweep results
#
# Description: Loads sweep output (RDS with meta + results) and produces
# partial dependency plots, scatterplots by param, and an optional three-way
# interaction line plot. Set loading option and (if needed) path at the top.
##############################################################################

library(here)
library(dplyr)
library(ggplot2)
library(patchwork)

##############################################################################
#### LOAD SWEEP OUTPUT ####
##############################################################################
# Option 1: use_latest_sweep = TRUE loads the path written by 04_run_sweep.R
# Option 2: use_latest_sweep = FALSE and set sweep_rds_path manually
use_latest_sweep <- TRUE
sweep_rds_path <- "output/sweep_results_2026-06-05-1158.rds" # used when use_latest_sweep is FALSE
# Latest novelty path:
# Latest truth path:

# Load results file
if (use_latest_sweep) {
  last_path_file <- here("output/last_sweep_path.txt")
  if (!file.exists(last_path_file)) {
    stop(
      "use_latest_sweep is TRUE but ",
      last_path_file,
      " not found. Run 04_run_sweep.R first or set use_latest_sweep = FALSE and set sweep_rds_path."
    )
  }
  sweep_rds_path <- readLines(last_path_file, n = 1L)
}
sweep_rds_path <- here(sweep_rds_path)
if (!file.exists(sweep_rds_path)) {
  stop("Sweep RDS not found: ", sweep_rds_path)
}

sweep_output <- readRDS(sweep_rds_path)
sweep_results <- sweep_output$results
param_config <- sweep_output$meta$param_config

##############################################################################
#### THREE-WAY LINE PLOT CONFIG ####
##############################################################################
# Only used when the sweep has at least 3 params and all three below are in it.
# Binning for the two line factors: 3 levels (Low/Medium/High) by splitting
# each param's sweep range into 3 equal chunks.
threeway_x_param <- "nonsig_logistic_midpoint"
threeway_line1_param <- "hold_samples_constant_at"
threeway_line2_param <- "base_null_probability"

# check if any of these are non-swept parameters and give a warning
if (
  !all(
    c(threeway_x_param, threeway_line1_param, threeway_line2_param) %in%
      colnames(sweep_output$results)
  )
) {
  stop(
    "One or more of the three-way line plot parameters are not swept in these results."
  )
}

##############################################################################
#### SETUP FROM METADATA ####
##############################################################################

# Get parameter names, labels, colors, and ranges from metadata
param_names <- names(param_config)
param_labels <- sapply(param_config, `[[`, "label")
param_colors <- sapply(param_config, `[[`, "color")
param_ranges <- lapply(param_config, function(x) c(x$min, x$max))

# Normalize parameters to 0-1 for combined partial dependency plot
sweep_norm <- sweep_results
for (i in seq_along(param_names)) {
  r <- param_ranges[[i]]
  sweep_norm[[paste0(param_names[i], "_norm")]] <-
    (sweep_results[[param_names[i]]] - r[1]) / (r[2] - r[1])
}

# Outcome variables and labels
outcomes <- list(
  list(
    var = "mean_replication_rate",
    label = "Replicator share (%)",
    range = c(0, 100),
    as_percent = TRUE
  ),
  list(
    var = "total_scientific_progress",
    label = "Total scientific progress",
    range = c(-Inf, Inf)
  ),
  list(
    var = "mean_replication_published",
    label = "% replication studies published",
    range = c(0, 1)
  ),
  list(
    var = "mean_original_published",
    label = "% original studies published",
    range = c(0, 1)
  ),
  list(
    var = "perc_resources_published",
    label = "% of time resources published",
    range = c(0, 100)
  ),
  list(
    var = "rep_success_prepub",
    label = "% of successful (all) replications",
    range = c(0, 100)
  ),
  list(
    var = "rep_success_postpub",
    label = "% of successful (published) replications",
    range = c(0, 100)
  )
)

# Function to scale outcome data to percent if needed
scale_outcome_data <- function(data, outcome) {
  out <- data
  if (isTRUE(outcome$as_percent)) {
    out[[outcome$var]] <- 100 * out[[outcome$var]]
  }
  out
}

# Use the current script environment so figures are available after source()
plot_env <- environment()

##############################################################################
#### PLOTS PER OUTCOME ####
##############################################################################
for (outcome in outcomes) {
  # Set outcome variable, label, and results, scale if needed
  outcome_var <- outcome$var
  outcome_label <- outcome$label
  plot_results <- scale_outcome_data(sweep_results, outcome)
  plot_norm <- scale_outcome_data(sweep_norm, outcome)

  # -------------------------------------------------------
  # Partial dependency (all params)
  # -------------------------------------------------------
  pdp_data <- data.frame()
  # Loop through each parameter and fit a loess model
  for (i in seq_along(param_names)) {
    norm_col <- paste0(param_names[i], "_norm")
    fit <- loess(
      as.formula(paste(outcome_var, "~ get(norm_col)")),
      data = plot_norm,
      span = 0.75
    )
    grid_x <- seq(0, 1, length.out = 100)
    pdp_data <- rbind(
      pdp_data,
      data.frame(
        param = param_labels[i],
        x_norm = grid_x,
        y = predict(fit, newdata = setNames(data.frame(grid_x), norm_col))
      )
    )
  }
  
  # Plot the partial dependency plot
  p_pdp <- ggplot(pdp_data, aes(x = x_norm, y = y, color = param)) +
    geom_line(linewidth = 1.2) +
    scale_color_manual(values = setNames(param_colors, param_labels)) +
    labs(
      x = "Parameter Value (normalized)",
      y = outcome_label,
      color = NULL
    ) +
    theme_classic() +
    theme(legend.position = "bottom")
  assign(paste0("fig_marginal_", outcome_var), p_pdp, envir = plot_env)

  print(p_pdp)

  # -------------------------------------------------------
  # Individual scatterplots (one per sweep param)
  # ------------------------------------------------------- 

  # Function to make a scatterplot for a given parameter
  make_scatter <- function(i) {
    p <- ggplot(
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
    p
  }

  # One scatter per sweep param; grid uses up to 3 columns (works for 1, 2, 3+ params)
  n_params <- length(param_names)
  scatter_list <- lapply(seq_len(n_params), make_scatter)
  p_scatters <- wrap_plots(scatter_list, ncol = min(3L, n_params))
  assign(paste0("fig_scatter_", outcome_var), p_scatters, envir = plot_env)
  print(p_scatters)

  # -------------------------------------------------------
  # Three-way interaction line graph (optional)
  # ------------------------------------------------------- 
  # Needs 3 params in the sweep so we can use one for x and two for line factors.
  # Function to make a three-way interaction line graph
  make_threeway <- function() {
    p <- ggplot(
      plot_results,
      aes(x = .data[[threeway_x_param]], y = .data[[outcome_var]])
    ) +
      geom_smooth(method = "loess", se = FALSE, linewidth = 1) +
      labs(x = x_label, y = outcome_label) +
      theme_classic()
  }
  # Set parameters for the three-way interaction line graph
  threeway_params <- c(
    threeway_x_param,
    threeway_line1_param,
    threeway_line2_param
  )
  # Check if we have at least 3 params and all three are swept
  if (length(param_names) >= 3L && all(threeway_params %in% param_names)) {
    line1_label <- param_config[[threeway_line1_param]]$label
    line2_label <- param_config[[threeway_line2_param]]$label
    x_label <- param_config[[threeway_x_param]]$label

    # Bin each line param into 3 levels by splitting its sweep range into equal thirds
    r1 <- param_ranges[[threeway_line1_param]]
    r2 <- param_ranges[[threeway_line2_param]]
    breaks1 <- c(
      r1[1],
      r1[1] + (r1[2] - r1[1]) / 3,
      r1[1] + 2 * (r1[2] - r1[1]) / 3,
      r1[2]
    )
    breaks2 <- c(
      r2[1],
      r2[1] + (r2[2] - r2[1]) / 3,
      r2[1] + 2 * (r2[2] - r2[1]) / 3,
      r2[2]
    )

    col1 <- plot_results[[threeway_line1_param]]
    col2 <- plot_results[[threeway_line2_param]]

    sweep_binned <- plot_results |>
      mutate(
        line1_level = cut(
          col1,
          breaks = breaks1,
          labels = c("Low", "Medium", "High"),
          include.lowest = TRUE
        ),
        line2_level = cut(
          col2,
          breaks = breaks2,
          labels = c("Low", "Medium", "High"),
          include.lowest = TRUE
        )
      ) |>
      filter(!is.na(line1_level), !is.na(line2_level))

    threeway_data <- sweep_binned |>
      group_by(
        x = .data[[threeway_x_param]],
        line1_level,
        line2_level
      ) |>
      summarise(y = mean(.data[[outcome_var]], na.rm = TRUE), .groups = "drop")

    # Plot the three-way interaction line graph
    p_threeway <- ggplot(
      threeway_data,
      aes(x = x, y = y, color = line1_level, linetype = line2_level)
    ) +
      geom_smooth(method = "loess", se = FALSE, linewidth = 1) +
      scale_color_manual(
        values = c(
          "Low" = "#F6DFA6", # light amber
          "Medium" = "#F0B429", # medium amber
          "High" = "#C98700" # darker amber
        ),
        name = line1_label
      ) +
      scale_linetype_manual(
        values = c("Low" = "dotted", "Medium" = "dashed", "High" = "solid"),
        name = line2_label,
        guide = guide_legend(override.aes = list(color = "black"))
      ) +
      labs(x = x_label, y = outcome_label) +
      theme_classic() +
      theme(
        legend.position = "bottom",
        legend.text = element_text(color = "black"),
        legend.title = element_text(color = "black"),
        legend.box = "vertical"
      )
    print(p_threeway)
  }
  assign(paste0("fig_interaction_", outcome_var), p_threeway, envir = plot_env)
}

##############################################################################
#### HEATMAPS: SAMPLE SIZE x PUBLICATION BIAS BY BASE NULL RANGE ####
##############################################################################
# Pick which swept variable goes on each heatmap axis
x_axis_variable <- "nonsig_logistic_midpoint"
y_axis_variable <- "hold_samples_constant_at"
# Pick which swept variable defines the value ranges to filter by
binned_variable <- "base_null_probability"

# Define the two ranges we want to show for the binned variable
binned_variable_ranges <- list(
  `0.8_to_1.0` = c(0.7, 1.0),
  `0.3_to_0.5` = c(0.2, 0.5)
)

# Build one heatmap per outcome and per selected range
for (outcome in outcomes) {
  outcome_var <- outcome$var
  outcome_label <- outcome$label
  outcome_range <- outcome$range
  plot_results <- scale_outcome_data(sweep_results, outcome)

  for (range_name in names(binned_variable_ranges)) {
    range_vals <- binned_variable_ranges[[range_name]]

    # Keep rows in range; plot will bin into visible x/y squares
    heatmap_data <- plot_results |>
      filter(
        .data[[binned_variable]] >= range_vals[1],
        .data[[binned_variable]] <= range_vals[2],
        !is.na(.data[[outcome_var]])
      )

    # Draw and label the heatmap using 2D summary bins
    p_heatmap <- ggplot(
      heatmap_data,
      aes(
        x = .data[[x_axis_variable]],
        y = .data[[y_axis_variable]],
        z = .data[[outcome_var]]
      )
    ) +
      stat_summary_2d(fun = function(z) mean(z, na.rm = TRUE), bins = 16)
    if (all(is.finite(outcome_range))) {
      p_heatmap <- p_heatmap +
        scale_fill_viridis_c(
          option = "D",
          limits = outcome_range,
          oob = scales::squish
        )
    } else {
      p_heatmap <- p_heatmap + scale_fill_viridis_c(option = "D")
    }
    p_heatmap <- p_heatmap +
      labs(
        x = param_config[[x_axis_variable]]$label,
        y = param_config[[y_axis_variable]]$label,
        fill = outcome_label,
        title = paste0(
          param_config[[binned_variable]]$label,
          " in [",
          range_vals[1],
          ", ",
          range_vals[2],
          "]"
        )
      ) +
      theme_classic()

    # Print and store each heatmap for later use
    print(p_heatmap)
    assign(
      paste0(
        "fig_heatmap_",
        outcome_var,
        "_",
        binned_variable,
        "_",
        range_name
      ),
      p_heatmap,
      envir = plot_env
    )
  }
}
