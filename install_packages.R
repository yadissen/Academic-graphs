# ============================================================================
# Install Required Packages for Academic Graph Production
# ============================================================================
# Run this script once to install all dependencies.

packages <- c(
  "ggplot2",      # Core plotting engine
  "readxl",       # Read Excel files (.xlsx, .xls)
  "dplyr",        # Data manipulation
  "tidyr",        # Data tidying
  "scales",       # Axis formatting and color utilities
  "ggrepel",      # Smart label placement (no overlap)
  "patchwork",    # Combine multiple plots
  "svglite",      # High-quality SVG export
  "ragg",         # High-quality PNG/TIFF export (AGG backend)
  "extrafont",    # System font access
  "showtext",     # Google Fonts and custom font rendering
  "shiny",        # Interactive web app framework
  "colourpicker"  # Color picker widget for Shiny
)

# Install missing packages
installed <- rownames(installed.packages())
to_install <- packages[!packages %in% installed]

if (length(to_install) > 0) {
  cat("Installing:", paste(to_install, collapse = ", "), "\n")
  install.packages(to_install, repos = "https://cloud.r-project.org")
} else {
  cat("All packages are already installed.\n")
}

cat("\nDone. You can now source the R scripts.\n")
