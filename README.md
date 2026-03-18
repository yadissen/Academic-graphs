# Academic Graphs — R Pipeline Profile Plotter

Publication-quality pipeline profile plots using **R** and **ggplot2**, migrated from the original HTML/Canvas implementation.

## Quick Start

### 1. Install R packages

```r
source("install_packages.R")
```

### 2. Launch the interactive browser app

```bash
Rscript app.R
```

This opens a **Shiny web app** in your browser — a full graph editor where you can:
- Upload Excel/CSV files (or load demo data)
- Toggle smooth splines, markers, grid lines
- Add reference lines and annotations interactively
- Adjust axis ranges, legend position, font sizes
- Download publication-ready PDF, SVG, or PNG

### 3. Or run the script examples

```r
source("example_pipeline_plot.R")
```

Graphs are saved to the `output/` directory in **PDF**, **SVG**, and **PNG** (300 dpi) formats.

## Project Structure

```
Academic-graphs/
├── R/
│   ├── theme_academic.R      # ggplot2 theme, palettes, export helpers
│   └── pipeline_plotter.R    # Main plotting functions + data import
├── output/                   # Generated figures (PDF, SVG, PNG)
├── app.R                     # Interactive browser app (Shiny)
├── install_packages.R        # One-time package installer
├── example_pipeline_plot.R   # Example usage with all features
├── pipeline_plotter6.html    # Original HTML/Canvas version
└── Finding Minimum Pipeline Diameter.xlsx
```

## Features

| Feature | Description |
|---------|-------------|
| **Multi-series plots** | Up to 12 series with distinct colors and markers |
| **Spline smoothing** | LOESS smoothing or straight-line interpolation |
| **Marker control** | All points, every Nth, or none |
| **Reference lines** | Horizontal and vertical dashed lines with labels |
| **Annotations** | Text labels with leader arrows (4 directions) |
| **Flow regime bands** | Background bands for stratified/wavy/slug/annular/bubble/dispersed |
| **Academic theme** | Clean, minimal theme following journal conventions |
| **Multi-panel figures** | Combine plots with `patchwork` |
| **Export** | PDF (vector), SVG (vector), PNG (300 dpi), TIFF |

## Usage

### Basic plot from an Excel file

```r
source("R/theme_academic.R")
source("R/pipeline_plotter.R")

data <- read_pipeline_data("your_data.xlsx")
p <- plot_pipeline_profile(data, title = "Pressure Profile", y_label = "P [bara]")
save_academic(p, "my_plot")
```

### One-liner

```r
p <- quick_pipeline_plot("your_data.xlsx", title = "Pressure Drop")
```

### Adding reference lines and annotations

```r
refs <- data.frame(
  axis = c("y", "x"), value = c(40, 500),
  label = c("Min pressure", "Midpoint"), color = c("#8b1a4a", "#666")
)
anns <- data.frame(
  x = 200, y = 72, label = "Critical point",
  color = "#2a4f6e", direction = "ur"
)
p <- plot_pipeline_profile(data, ref_lines = refs, annotations = anns)
```

### Customizing the theme

```r
# Use the theme on any ggplot
ggplot(df, aes(x, y)) +
  geom_line() +
  theme_academic(base_size = 12, grid = "major", legend_pos = "bottom")
```

### Exporting

```r
save_academic(p, "figure_1", width = 8, height = 5, formats = c("pdf", "svg", "png"))
```

## Color Palette

The 12-color academic palette is colorblind-friendly and designed for print:

| Name | Hex | Use |
|------|-----|-----|
| Navy | `#2a4f6e` | Primary series |
| Terracotta | `#8b3a1e` | Secondary series |
| Forest | `#2e6b45` | Tertiary series |
| Violet | `#5b2d8e` | Fourth series |
| Teal | `#1a6b6b` | Fifth series |
| Amber | `#7a5318` | Sixth series |

## Requirements

- **R** >= 4.0
- Core: `ggplot2`, `readxl`, `dplyr`, `tidyr`, `scales`
- Optional: `patchwork` (multi-panel), `svglite` (SVG), `ragg` (PNG), `ggrepel` (labels)
