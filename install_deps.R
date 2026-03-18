# Install required packages for the Academic Pipeline Profile Plotter
packages <- c("shiny", "ggplot2", "readxl", "svglite", "splines")

for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat("Installing", pkg, "...\n")
    install.packages(pkg, repos = "https://cloud.r-project.org")
  } else {
    cat(pkg, "already installed.\n")
  }
}

cat("\nAll dependencies installed. Run the app with:\n")
cat("  shiny::runApp('app.R')\n")
