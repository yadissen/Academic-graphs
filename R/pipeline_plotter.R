# ============================================================================
# Pipeline Profile Plotter — ggplot2 Implementation
# ============================================================================
# Recreates all features from pipeline_plotter6.html using ggplot2:
#   - Multi-series line plots with spline smoothing
#   - Flow regime banding
#   - Reference lines (horizontal / vertical)
#   - Annotations with leader lines
#   - Customizable markers, colors, legend position
#   - Publication-ready export (PDF, SVG, PNG)
#
# Usage:
#   source("R/theme_academic.R")
#   source("R/pipeline_plotter.R")
#   data <- read_pipeline_data("Finding Minimum Pipeline Diameter.xlsx")
#   plot_pipeline_profile(data, ...)
# ============================================================================

library(ggplot2)
library(readxl)
library(dplyr)
library(tidyr)

# Source the theme (relative to project root)
if (!exists("theme_academic", mode = "function")) {
  source(file.path(dirname(sys.frame(1)$ofile %||% "."), "theme_academic.R"))
}


# ── Data Import ──────────────────────────────────────────────────────────────

#' Read pipeline data from an Excel or CSV file
#'
#' Automatically detects the X-axis column (looks for "length") and treats
#' all other numeric columns as Y-series.
#'
#' @param file_path  Path to .xlsx, .xls, or .csv file
#' @param sheet      Sheet name or index (default 1)
#' @param x_col      Name or pattern for the X-axis column (default "length")
#'
#' @return A list with components:
#'   - long: data in long format (x, series, value)
#'   - wide: original wide-format data
#'   - x_name: detected X column name
#'   - series_names: character vector of Y-series names
read_pipeline_data <- function(file_path, sheet = 1, x_col = "length") {

  ext <- tolower(tools::file_ext(file_path))

  if (ext %in% c("xlsx", "xls")) {
    wide <- readxl::read_excel(file_path, sheet = sheet)
  } else if (ext == "csv") {
    wide <- read.csv(file_path, stringsAsFactors = FALSE)
  } else {
    stop("Unsupported file format: ", ext)
  }

  # Find X column
  x_idx <- grep(x_col, names(wide), ignore.case = TRUE)[1]
  if (is.na(x_idx)) {
    message("No column matching '", x_col, "' found. Using first column as X.")
    x_idx <- 1
  }

  x_name <- names(wide)[x_idx]
  y_names <- setdiff(names(wide), x_name)

  # Convert to numeric (handles comma decimals)
  for (col in names(wide)) {
    if (is.character(wide[[col]])) {
      wide[[col]] <- as.numeric(gsub(",", ".", wide[[col]]))
    }
  }

  # Pivot to long format
  long <- wide %>%
    tidyr::pivot_longer(
      cols = all_of(y_names),
      names_to = "series",
      values_to = "value"
    ) %>%
    rename(x = !!x_name) %>%
    filter(!is.na(x), !is.na(value))

  # Preserve series order as they appear in the file
  long$series <- factor(long$series, levels = y_names)

  list(
    long = long,
    wide = wide,
    x_name = x_name,
    series_names = y_names
  )
}


# ── Flow Regime Detection ────────────────────────────────────────────────────

#' Check if a series contains flow regime codes (small integers 1–10)
is_flow_regime <- function(values) {
  vals <- na.omit(values)
  all(vals == floor(vals)) && all(vals >= 1 & vals <= 10) && length(unique(vals)) <= 10
}

#' Map integer flow regime codes to labels
regime_label <- function(code) {
  labels <- c(
    "1" = "Stratified", "2" = "Wavy", "3" = "Slug",
    "4" = "Annular", "5" = "Bubble", "6" = "Dispersed",
    "7" = "Regime 7", "8" = "Regime 8", "9" = "Regime 9", "10" = "Regime 10"
  )
  labels[as.character(code)]
}


# ── Main Plotting Function ──────────────────────────────────────────────────

#' Create a publication-quality pipeline profile plot
#'
#' @param data        Output from read_pipeline_data(), OR a data.frame in
#'                    long format with columns: x, series, value
#' @param title       Chart title (default NULL = no title)
#' @param x_label     X-axis label (default "Pipeline Length [m]")
#' @param y_label     Y-axis label (default "Pressure [bara]")
#' @param smooth      Use spline smoothing (default TRUE)
#' @param show_points Show data point markers (default TRUE)
#' @param marker_nth  Show every Nth marker; NULL = all (default NULL)
#' @param colors      Named or unnamed character vector of colors
#' @param shapes      Numeric vector of ggplot2 shape codes
#' @param line_types  Line type values (1=solid, 2=dashed, etc.)
#' @param ref_lines   Data.frame with columns: axis ("x" or "y"), value, label
#'                    (optional), color (optional)
#' @param annotations Data.frame with columns: x, y, label, and optionally
#'                    color, direction ("ur","ul","lr","ll")
#' @param regime_bands Logical; draw flow regime background bands (default FALSE)
#' @param x_limits    Numeric vector c(min, max) or NULL for auto
#' @param y_limits    Numeric vector c(min, max) or NULL for auto
#' @param legend_pos  Legend position (default "bottom")
#' @param grid        Grid lines: "none", "major", "both" (default "none")
#' @param base_size   Base font size (default 11)
#' @param line_width  Line width (default 0.8)
#' @param point_size  Point size (default 2.5)
#'
#' @return A ggplot2 object
plot_pipeline_profile <- function(data,
                                  title = NULL,
                                  x_label = "Pipeline Length [m]",
                                  y_label = "Pressure [bara]",
                                  smooth = TRUE,
                                  show_points = TRUE,
                                  marker_nth = NULL,
                                  colors = NULL,
                                  shapes = NULL,
                                  line_types = NULL,
                                  ref_lines = NULL,
                                  annotations = NULL,
                                  regime_bands = FALSE,
                                  x_limits = NULL,
                                  y_limits = NULL,
                                  legend_pos = "bottom",
                                  grid = "none",
                                  base_size = 11,
                                  line_width = 0.8,
                                  point_size = 2.5) {

  # Accept either list from read_pipeline_data or raw data.frame

if (is.list(data) && !is.data.frame(data)) {
    df <- data$long
  } else {
    df <- data
  }

  n_series <- length(unique(df$series))

  # Defaults
  if (is.null(colors))     colors <- unname(academic_colors)[seq_len(n_series)]
  if (is.null(shapes))     shapes <- academic_shapes[seq_len(n_series)]
  if (is.null(line_types)) line_types <- rep(1, n_series)

  # ── Filter markers for nth display ──
  if (!is.null(marker_nth) && marker_nth > 1) {
    df_points <- df %>%
      group_by(series) %>%
      mutate(row_n = row_number()) %>%
      filter(row_n == 1 | row_n %% marker_nth == 0 | row_n == n()) %>%
      ungroup() %>%
      select(-row_n)
  } else {
    df_points <- df
  }

  # ── Base plot ──
  p <- ggplot(df, aes(x = x, y = value, color = series, shape = series,
                       linetype = series)) +
    theme_academic(base_size = base_size, grid = grid, legend_pos = legend_pos)

  # ── Flow regime banding (background rectangles) ──
  if (regime_bands) {
    regime_df <- df %>%
      filter(is_flow_regime(value)) %>%
      group_by(series) %>%
      mutate(regime_code = value) %>%
      ungroup()

    if (nrow(regime_df) > 0) {
      # Build run-length bands
      bands <- regime_df %>%
        arrange(x) %>%
        mutate(regime_name = regime_label(regime_code)) %>%
        group_by(series) %>%
        mutate(
          run = cumsum(c(1, diff(regime_code) != 0)),
          .groups = "drop"
        ) %>%
        group_by(series, run, regime_name) %>%
        summarise(xmin = min(x), xmax = max(x), .groups = "drop")

      for (i in seq_len(nrow(bands))) {
        rn <- bands$regime_name[i]
        fill_col <- regime_fills[rn]
        border_col <- regime_borders[rn]
        if (!is.na(fill_col)) {
          p <- p + annotate("rect",
                            xmin = bands$xmin[i], xmax = bands$xmax[i],
                            ymin = -Inf, ymax = Inf,
                            fill = fill_col, color = border_col,
                            linewidth = 0.3, alpha = 1)
        }
      }
    }
  }

  # ── Reference lines ──
  if (!is.null(ref_lines) && nrow(ref_lines) > 0) {
    for (i in seq_len(nrow(ref_lines))) {
      rl <- ref_lines[i, ]
      lc <- if (!is.null(rl$color) && !is.na(rl$color)) rl$color else "#666666"

      if (rl$axis == "y") {
        p <- p + geom_hline(yintercept = rl$value, linetype = "dashed",
                            color = lc, linewidth = 0.5)
      } else {
        p <- p + geom_vline(xintercept = rl$value, linetype = "dashed",
                            color = lc, linewidth = 0.5)
      }

      if (!is.null(rl$label) && !is.na(rl$label) && nchar(rl$label) > 0) {
        if (rl$axis == "y") {
          p <- p + annotate("text", x = -Inf, y = rl$value, label = rl$label,
                            hjust = -0.1, vjust = -0.5, size = 3,
                            color = lc, fontface = "italic")
        } else {
          p <- p + annotate("text", x = rl$value, y = Inf, label = rl$label,
                            hjust = -0.1, vjust = 1.5, size = 3,
                            color = lc, fontface = "italic", angle = 90)
        }
      }
    }
  }

  # ── Lines ──
  if (smooth) {
    p <- p + geom_smooth(method = "loess", se = FALSE, linewidth = line_width,
                         span = 0.3, formula = y ~ x)
  } else {
    p <- p + geom_line(linewidth = line_width)
  }

  # ── Points / Markers ──
  if (show_points) {
    p <- p + geom_point(data = df_points, size = point_size, stroke = 0.4)
  }

  # ── Scales ──
  p <- p +
    scale_color_manual(values = colors) +
    scale_shape_manual(values = shapes) +
    scale_linetype_manual(values = line_types)

  # ── Axis limits ──
  if (!is.null(x_limits)) p <- p + scale_x_continuous(limits = x_limits)
  if (!is.null(y_limits)) p <- p + scale_y_continuous(limits = y_limits)

  # ── Labels ──
  p <- p + labs(
    title = title,
    x = x_label,
    y = y_label,
    color = NULL,
    shape = NULL,
    linetype = NULL
  )

  # ── Annotations ──
  if (!is.null(annotations) && nrow(annotations) > 0) {
    for (i in seq_len(nrow(annotations))) {
      ann <- annotations[i, ]
      ac <- if (!is.null(ann$color) && !is.na(ann$color)) ann$color else "#333333"

      # Direction offsets for leader line
      dir <- if (!is.null(ann$direction)) ann$direction else "ur"
      x_range <- diff(range(df$x, na.rm = TRUE))
      y_range <- diff(range(df$value, na.rm = TRUE))
      dx <- x_range * 0.06 * ifelse(grepl("l", dir), -1, 1)
      dy <- y_range * 0.08 * ifelse(grepl("l", substr(dir, 1, 1)), -1, 1)
      # "ur"=up-right, "ul"=up-left, "lr"=lower-right, "ll"=lower-left
      if (startsWith(dir, "u")) dy <- abs(dy) else dy <- -abs(dy)

      p <- p +
        annotate("segment",
                 x = ann$x, y = ann$y,
                 xend = ann$x + dx, yend = ann$y + dy,
                 color = ac, linewidth = 0.4,
                 arrow = arrow(length = unit(0.15, "cm"), type = "closed")) +
        annotate("label",
                 x = ann$x + dx, y = ann$y + dy,
                 label = ann$label, color = ac,
                 size = 3, fill = "white", label.size = 0.3,
                 fontface = "italic")
    }
  }

  p
}


# ── Quick Plot Wrappers ─────────────────────────────────────────────────────

#' Quick line plot from an Excel file
#'
#' A one-call convenience function: reads data and produces a plot.
#'
#' @param file_path Path to the data file
#' @param sheet     Sheet name or number
#' @param ...       Additional arguments passed to plot_pipeline_profile()
#'
#' @return A ggplot2 object
quick_pipeline_plot <- function(file_path, sheet = 1, ...) {
  data <- read_pipeline_data(file_path, sheet = sheet)
  plot_pipeline_profile(data, ...)
}
