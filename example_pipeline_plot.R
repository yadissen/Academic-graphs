# ============================================================================
# Example: Pipeline Profile Plots — Publication Quality
# ============================================================================
# This script demonstrates how to create professional academic graphs
# using the R/ggplot2 pipeline plotter.
#
# Run: source("example_pipeline_plot.R")
# ============================================================================

library(ggplot2)
library(dplyr)

source("R/theme_academic.R")
source("R/pipeline_plotter.R")

# ── 1. Load data from your Excel file ───────────────────────────────────────

data <- read_pipeline_data("Finding Minimum Pipeline Diameter.xlsx", sheet = 1)

cat("X column:", data$x_name, "\n")
cat("Series found:", paste(data$series_names, collapse = ", "), "\n")
cat("Rows:", nrow(data$long), "\n\n")


# ── 2. Basic pipeline profile (simplest call) ───────────────────────────────

p1 <- plot_pipeline_profile(
  data,
  title    = "Pipeline Pressure Profile",
  x_label  = "Pipeline Length [m]",
  y_label  = "Pressure [bara]"
)

print(p1)
save_academic(p1, "01_basic_profile")


# ── 3. Styled with reference lines and annotations ──────────────────────────

ref_lines <- data.frame(
  axis  = c("y", "y"),
  value = c(10, 50),
  label = c("Min. operating pressure", "Design pressure"),
  color = c("#8b1a4a", "#2a4f6e"),
  stringsAsFactors = FALSE
)

annotations <- data.frame(
  x         = c(500),
  y         = c(30),
  label     = c("Critical section"),
  color     = c("#8b3a1e"),
  direction = c("ur"),
  stringsAsFactors = FALSE
)

p2 <- plot_pipeline_profile(
  data,
  title       = "Pipeline Pressure Profile with Annotations",
  x_label     = "Pipeline Length [m]",
  y_label     = "Pressure [bara]",
  ref_lines   = ref_lines,
  annotations = annotations,
  smooth      = FALSE,
  legend_pos  = "bottom",
  grid        = "major"
)

print(p2)
save_academic(p2, "02_annotated_profile")


# ── 4. Customized markers and colors ────────────────────────────────────────

p3 <- plot_pipeline_profile(
  data,
  title       = "Pipeline Pressure Profile",
  x_label     = "Pipeline Length [m]",
  y_label     = "Pressure [bara]",
  smooth      = TRUE,
  show_points = TRUE,
  marker_nth  = 5,            # Show every 5th marker
  point_size  = 3,
  line_width  = 0.9,
  legend_pos  = c(0.85, 0.85) # Inside plot area (top-right)
)

print(p3)
save_academic(p3, "03_custom_markers")


# ── 5. Straight lines (no spline) with no markers ───────────────────────────

p4 <- plot_pipeline_profile(
  data,
  title       = "Pipeline Pressure Profile (Linear)",
  x_label     = "Pipeline Length [m]",
  y_label     = "Pressure [bara]",
  smooth      = FALSE,
  show_points = FALSE,
  line_width  = 1.0,
  legend_pos  = "right"
)

print(p4)
save_academic(p4, "04_linear_no_markers")


# ── 6. Using synthetic demo data (for testing without the Excel file) ───────

demo_data <- data.frame(
  x = rep(seq(0, 1000, by = 50), 3),
  series = rep(c("4-inch Pipeline", "6-inch Pipeline", "8-inch Pipeline"),
               each = 21),
  value = c(
    # 4-inch: steeper pressure drop
    80 - seq(0, 1000, by = 50) * 0.045 + rnorm(21, 0, 1.5),
    # 6-inch: moderate
    80 - seq(0, 1000, by = 50) * 0.025 + rnorm(21, 0, 1.0),
    # 8-inch: gentle
    80 - seq(0, 1000, by = 50) * 0.012 + rnorm(21, 0, 0.8)
  )
)
demo_data$series <- factor(demo_data$series,
                           levels = c("4-inch Pipeline",
                                      "6-inch Pipeline",
                                      "8-inch Pipeline"))

demo_refs <- data.frame(
  axis  = c("y", "x"),
  value = c(40, 500),
  label = c("Min. operating pressure", "Midpoint"),
  color = c("#8b1a4a", "#666666"),
  stringsAsFactors = FALSE
)

demo_ann <- data.frame(
  x = c(200, 800),
  y = c(72, 55),
  label = c("Inlet region", "Pressure recovery"),
  color = c("#2a4f6e", "#2e6b45"),
  direction = c("ur", "ul"),
  stringsAsFactors = FALSE
)

p5 <- plot_pipeline_profile(
  demo_data,
  title       = "Pressure Drop Comparison by Pipe Diameter",
  x_label     = "Pipeline Length [m]",
  y_label     = expression(P[T]~"[bara]"),
  ref_lines   = demo_refs,
  annotations = demo_ann,
  smooth      = TRUE,
  show_points = TRUE,
  marker_nth  = 3,
  legend_pos  = "bottom",
  grid        = "major"
)

print(p5)
save_academic(p5, "05_demo_comparison")


# ── 7. Multi-panel figure using patchwork ────────────────────────────────────

if (requireNamespace("patchwork", quietly = TRUE)) {
  library(patchwork)

  # Two panels side by side
  panel_a <- plot_pipeline_profile(
    demo_data,
    title       = "(a) Smooth Spline Fit",
    x_label     = "Pipeline Length [m]",
    y_label     = "Pressure [bara]",
    smooth      = TRUE,
    show_points = TRUE,
    marker_nth  = 4,
    point_size  = 2,
    legend_pos  = "none",
    base_size   = 10
  )

  panel_b <- plot_pipeline_profile(
    demo_data,
    title       = "(b) Linear Interpolation",
    x_label     = "Pipeline Length [m]",
    y_label     = "Pressure [bara]",
    smooth      = FALSE,
    show_points = FALSE,
    line_width  = 0.7,
    legend_pos  = "bottom",
    base_size   = 10
  )

  combined <- panel_a + panel_b +
    plot_layout(ncol = 2) +
    plot_annotation(
      title = "Pipeline Pressure Profile Comparison",
      theme = theme(
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
      )
    )

  print(combined)
  save_academic(combined, "06_multi_panel", width = 14, height = 5.5)
}


cat("\n=== All plots saved to output/ directory ===\n")
cat("Formats: PDF (vector), SVG (vector), PNG (300 dpi)\n")
