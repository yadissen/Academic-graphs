# ═══════════════════════════════════════════════════════════
#  Academic Graph Studio — Dependency Installer
# ═══════════════════════════════════════════════════════════

packages <- c(
  "shiny",         # Web framework

"bslib",         # Modern Bootstrap 5 UI
  "ggplot2",       # Publication-quality plots
  "readxl",        # Excel file reading
  "svglite",       # SVG export
  "plotly",        # Interactive plot preview
  "colourpicker",  # Colour picker widget
  "scales",        # Axis scale utilities
  "splines"        # Spline interpolation (base R)
)

cat("Academic Graph Studio — Installing dependencies\n")
cat(strrep("\u2500", 50), "\n\n")

for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat("\u25b6 Installing", pkg, "...\n")
    install.packages(pkg, repos = "https://cloud.r-project.org")
    if (requireNamespace(pkg, quietly = TRUE)) {
      cat("  \u2713", pkg, "installed successfully\n")
    } else {
      cat("  \u2717 Failed to install", pkg, "\n")
    }
  } else {
    cat("  \u2713", pkg, "already installed\n")
  }
}

cat("\n", strrep("\u2500", 50), "\n")
cat("All dependencies ready.\n\n")
cat("To launch the app:\n")
cat("  shiny::runApp('app.R')\n\n")
cat("Or in RStudio: open app.R and click 'Run App'\n")
