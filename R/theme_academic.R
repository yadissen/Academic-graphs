# ============================================================================
# Academic Publication Theme for ggplot2
# ============================================================================
# A clean, professional theme designed for journal-quality figures.
# Inspired by Nature, Science, and engineering journal standards.
#
# Usage:
#   source("R/theme_academic.R")
#   ggplot(...) + theme_academic()
# ============================================================================

library(ggplot2)

# ── Academic Color Palettes ──────────────────────────────────────────────────

# Primary palette: 12 distinct, colorblind-friendly academic colors
academic_colors <- c(
  "navy"       = "#2a4f6e",
  "terracotta" = "#8b3a1e",
  "forest"     = "#2e6b45",
  "violet"     = "#5b2d8e",
  "teal"       = "#1a6b6b",
  "amber"      = "#7a5318",
  "crimson"    = "#8b1a4a",
  "olive"      = "#3d5a1e",
  "slate"      = "#1e3a6e",
  "gold"       = "#8b6b00",
  "plum"       = "#4a1a2e",
  "emerald"    = "#006b4e"
)

# Marker shape cycle (ggplot2 shape codes)
#   16=filled circle, 15=filled square, 17=filled triangle,
#   18=filled diamond, 1=open circle, 0=open square,
#   2=open triangle, 5=open diamond, 3=plus, 4=cross,
#   6=inverted triangle, 8=asterisk
academic_shapes <- c(16, 15, 17, 18, 1, 0, 2, 5, 3, 4, 6, 8)

# Flow regime palette (background band fills)
regime_fills <- c(
  "Stratified" = "#2a4f6e1F",
  "Wavy"       = "#2e6b451F",
  "Slug"       = "#8b3a1e1F",
  "Annular"    = "#7a53181F",
  "Bubble"     = "#5b2d8e1F",
  "Dispersed"  = "#1a6b6b1F"
)

regime_borders <- c(
  "Stratified" = "#2a4f6e80",
  "Wavy"       = "#2e6b4580",
  "Slug"       = "#8b3a1e80",
  "Annular"    = "#7a531880",
  "Bubble"     = "#5b2d8e80",
  "Dispersed"  = "#1a6b6b80"
)


# ── Scale Functions ──────────────────────────────────────────────────────────

#' Discrete color scale using the academic palette
#' @param ... Additional arguments passed to discrete_scale
scale_color_academic <- function(...) {
  scale_color_manual(values = unname(academic_colors), ...)
}

#' Discrete fill scale using the academic palette
#' @param ... Additional arguments passed to discrete_scale
scale_fill_academic <- function(...) {
  scale_fill_manual(values = unname(academic_colors), ...)
}

#' Discrete shape scale using the academic marker cycle
#' @param ... Additional arguments passed to scale_shape_manual
scale_shape_academic <- function(...) {
  scale_shape_manual(values = academic_shapes, ...)
}


# ── Theme ────────────────────────────────────────────────────────────────────

#' Clean academic theme for publication-quality figures
#'
#' @param base_size   Base font size in points (default 11)
#' @param base_family Base font family (default "Helvetica")
#' @param grid        Show grid lines: "none", "major", "both" (default "none")
#' @param border      Draw plot border (default TRUE)
#' @param legend_pos  Legend position: "right", "bottom", "top", "none",
#'                    or a two-element numeric vector c(x, y)
#'
#' @return A ggplot2 theme object
theme_academic <- function(base_size = 11,
                           base_family = "",
                           grid = "none",
                           border = TRUE,
                           legend_pos = "right") {

  # Start from a minimal base
  th <- theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    theme(
      # ── Plot ──
      plot.title       = element_text(size = rel(1.3), face = "bold",
                                      hjust = 0.5, margin = margin(b = 10)),
      plot.subtitle    = element_text(size = rel(0.9), hjust = 0.5,
                                      color = "#555555",
                                      margin = margin(b = 12)),
      plot.caption     = element_text(size = rel(0.7), hjust = 1,
                                      color = "#777777",
                                      margin = margin(t = 8)),
      plot.margin      = margin(t = 15, r = 15, b = 10, l = 10),
      plot.background  = element_rect(fill = "white", color = NA),

      # ── Panel ──
      panel.background = element_rect(fill = "white", color = NA),
      panel.border     = if (border) {
        element_rect(fill = NA, color = "#333333", linewidth = 0.5)
      } else {
        element_blank()
      },
      panel.spacing    = unit(1.2, "lines"),

      # ── Grid ──
      panel.grid.major = if (grid %in% c("major", "both")) {
        element_line(color = "#e8e8e8", linewidth = 0.3)
      } else {
        element_blank()
      },
      panel.grid.minor = if (grid == "both") {
        element_line(color = "#f0f0f0", linewidth = 0.2)
      } else {
        element_blank()
      },

      # ── Axes ──
      axis.line        = if (!border) {
        element_line(color = "#333333", linewidth = 0.5)
      } else {
        element_blank()
      },
      axis.ticks       = element_line(color = "#333333", linewidth = 0.3),
      axis.ticks.length = unit(3, "pt"),
      axis.title.x     = element_text(size = rel(1.0),
                                      margin = margin(t = 8)),
      axis.title.y     = element_text(size = rel(1.0), angle = 90,
                                      margin = margin(r = 8)),
      axis.text        = element_text(size = rel(0.85), color = "#333333"),

      # ── Legend ──
      legend.position    = legend_pos,
      legend.background  = element_rect(fill = "white", color = "#cccccc",
                                        linewidth = 0.3),
      legend.key         = element_rect(fill = "white", color = NA),
      legend.key.size    = unit(1.2, "lines"),
      legend.title       = element_text(size = rel(0.85), face = "bold"),
      legend.text        = element_text(size = rel(0.8)),
      legend.margin      = margin(4, 6, 4, 6),
      legend.spacing     = unit(0.4, "lines"),

      # ── Facets ──
      strip.background = element_rect(fill = "#f5f5f5", color = "#cccccc",
                                       linewidth = 0.3),
      strip.text       = element_text(size = rel(0.9), face = "bold",
                                      margin = margin(4, 4, 4, 4))
    )

  th
}


# ── Export Helpers ────────────────────────────────────────────────────────────

#' Save a ggplot in multiple publication-ready formats
#'
#' Exports to PDF (vector), SVG (vector), PNG (300 dpi), and optionally TIFF.
#'
#' @param plot      A ggplot2 object
#' @param filename  Base filename without extension
#' @param path      Output directory (default "output")
#' @param width     Width in inches (default 8)
#' @param height    Height in inches (default 5)
#' @param dpi       Resolution for raster formats (default 300)
#' @param formats   Character vector of formats: "pdf", "svg", "png", "tiff"
save_academic <- function(plot,
                          filename,
                          path = "output",
                          width = 8,
                          height = 5,
                          dpi = 300,
                          formats = c("pdf", "svg", "png")) {

  if (!dir.exists(path)) dir.create(path, recursive = TRUE)

  for (fmt in formats) {
    fp <- file.path(path, paste0(filename, ".", fmt))

    if (fmt == "svg") {
      # Use svglite for cleaner SVG output
      if (requireNamespace("svglite", quietly = TRUE)) {
        ggsave(fp, plot = plot, device = svglite::svglite,
               width = width, height = height)
      } else {
        ggsave(fp, plot = plot, width = width, height = height)
      }
    } else if (fmt == "png") {
      # Use ragg for superior anti-aliasing
      if (requireNamespace("ragg", quietly = TRUE)) {
        ggsave(fp, plot = plot, device = ragg::agg_png,
               width = width, height = height, dpi = dpi)
      } else {
        ggsave(fp, plot = plot, width = width, height = height, dpi = dpi)
      }
    } else if (fmt == "tiff") {
      ggsave(fp, plot = plot, width = width, height = height, dpi = dpi,
             compression = "lzw")
    } else {
      ggsave(fp, plot = plot, width = width, height = height, dpi = dpi)
    }

    cat("Saved:", fp, "\n")
  }
}
