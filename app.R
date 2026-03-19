# ═══════════════════════════════════════════════════════════════════════════════
#  Academic Graph Studio — Premium R Shiny Application
#  Publication-quality scientific visualisation (Nature / Science / IEEE ready)
# ═══════════════════════════════════════════════════════════════════════════════

# ── Dependencies ──────────────────────────────────────────────────────────────
library(shiny)
library(bslib)
library(ggplot2)
library(readxl)
library(svglite)
library(plotly)
library(colourpicker)
library(scales)
library(splines)

# ═══════════════════════════════════════════════════════════════════════════════
#  CONSTANTS & PALETTES
# ═══════════════════════════════════════════════════════════════════════════════

# ── Journal colour palettes ───────────────────────────────────────────────────
PALETTES <- list(
  "Nature" = c("#E64B35", "#4DBBD5", "#00A087", "#3C5488",
               "#F39B7F", "#8491B4", "#91D1C2", "#DC0000",
               "#7E6148", "#B09C85"),
  "Science (AAAS)" = c("#3B4992", "#EE0000", "#008B45", "#631879",
                        "#008280", "#BB0021", "#5F559B", "#A20056",
                        "#808180", "#1B1919"),
  "Lancet" = c("#00468B", "#ED0000", "#42B540", "#0099B4",
               "#925E9F", "#FDAF91", "#AD002A", "#ADB6B6",
               "#1B1919", "#E6A800"),
  "NEJM" = c("#BC3C29", "#0072B5", "#E18727", "#20854E",
             "#7876B1", "#6F99AD", "#FFDC91", "#EE4C97",
             "#8C564B", "#BCBD22"),
  "IEEE" = c("#0072BD", "#D95319", "#EDB120", "#7E2F8E",
             "#77AC30", "#4DBEEE", "#A2142F", "#5E5E5E",
             "#2E8B57", "#FF6347"),
  "Classic Academic" = c("#8b3a1e", "#2a4f6e", "#2e6b45", "#7a5318",
                         "#5b2d8e", "#1a6b6b", "#8b1a4a", "#3d5a1e",
                         "#1e3a6e", "#8b6b00"),
  "Grayscale" = c("#000000", "#404040", "#808080", "#B0B0B0",
                  "#2A2A2A", "#5A5A5A", "#9A9A9A", "#C8C8C8",
                  "#1A1A1A", "#707070"),
  "Colorblind Safe" = c("#0072B2", "#D55E00", "#009E73", "#CC79A7",
                        "#F0E442", "#56B4E9", "#E69F00", "#000000",
                        "#999999", "#882255")
)

# ── Marker shapes (academic gold standard) ────────────────────────────────────
# Shapes 21-25 are fill+border shapes: THE standard for publication-quality
# plots. They allow color fill with a distinct black border for maximum clarity.
# Shapes 0-14 are outline-only; 15-20 are solid without border control.
MARKER_SHAPES <- c(
  "Circle"       = 21L,  # ● filled circle with border
  "Square"       = 22L,  # ■ filled square with border
  "Triangle"     = 24L,  # ▲ filled triangle with border
  "Diamond"      = 23L,  # ◆ filled diamond with border
  "Inv Triangle" = 25L,  # ▼ filled inverted triangle with border
  "Cross"        = 4L,   # ✕ open cross
  "Plus"         = 3L,   # + plus
  "Asterisk"     = 8L,   # * asterisk
  "Circle Open"  = 1L,   # ○ open circle
  "Square Open"  = 0L,   # □ open square
  "Triangle Open" = 2L,  # △ open triangle
  "Diamond Open" = 5L    # ◇ open diamond
)

# Shapes 21-25 use fill aesthetic (color = border, fill = interior)
FILLED_SHAPES <- c(21L, 22L, 23L, 24L, 25L)

# ── Line type options ─────────────────────────────────────────────────────────
LINE_TYPES <- c(
  "Solid" = "solid", "Dashed" = "dashed", "Dotted" = "dotted",
  "Dot-Dash" = "dotdash", "Long Dash" = "longdash", "Two Dash" = "twodash"
)

# ── Journal export presets ────────────────────────────────────────────────────
JOURNAL_PRESETS <- list(
  "Custom"  = list(w = 10,    h = 6,    dpi = 300,  fmt = "svg"),
  "Nature (single col)"   = list(w = 3.5,  h = 2.625, dpi = 300, fmt = "pdf"),
  "Nature (double col)"   = list(w = 7.08, h = 4.5,   dpi = 300, fmt = "pdf"),
  "Science (single col)"  = list(w = 3.5,  h = 2.5,   dpi = 300, fmt = "pdf"),
  "Science (double col)"  = list(w = 7.25, h = 5,     dpi = 300, fmt = "pdf"),
  "IEEE (single col)"     = list(w = 3.5,  h = 2.625, dpi = 600, fmt = "tiff"),
  "IEEE (double col)"     = list(w = 7.16, h = 4.5,   dpi = 600, fmt = "tiff"),
  "Elsevier (single col)" = list(w = 3.54, h = 2.65,  dpi = 300, fmt = "pdf"),
  "Elsevier (full page)"  = list(w = 7.48, h = 5.5,   dpi = 300, fmt = "pdf"),
  "SPE"                   = list(w = 6.75, h = 4.5,   dpi = 300, fmt = "pdf"),
  "PowerPoint (16:9)"     = list(w = 13.3, h = 7.5,   dpi = 150, fmt = "png"),
  "Poster (A0)"           = list(w = 16,   h = 10,    dpi = 300, fmt = "png")
)

# ── Flow regime definitions ───────────────────────────────────────────────────
FLOW_REGIMES <- list(
  `1` = list(label = "Stratified", fill = "#2a4f6e"),
  `2` = list(label = "Wavy",       fill = "#2e6b45"),
  `3` = list(label = "Slug",       fill = "#8b3a1e"),
  `4` = list(label = "Annular",    fill = "#7a5318"),
  `5` = list(label = "Bubble",     fill = "#5b2d8e"),
  `6` = list(label = "Dispersed",  fill = "#1a6b6b")
)


# ═══════════════════════════════════════════════════════════════════════════════
#  THEME ENGINE
# ═══════════════════════════════════════════════════════════════════════════════

theme_academic <- function(base_size = 14, base_family = "sans",
                           grid = "none", border = TRUE,
                           ticks_inward = TRUE) {
  # Inward ticks: negative length draws them inside the plot area
  tick_len <- if (ticks_inward) unit(-4, "pt") else unit(4, "pt")

  t <- theme_classic(base_size = base_size, base_family = base_family) %+replace%
    theme(
      text             = element_text(color = "#1a1714"),
      plot.title       = element_text(size = rel(1.25), face = "bold",
                                      hjust = 0.5, margin = margin(b = 10)),
      plot.subtitle    = element_text(size = rel(0.85), face = "italic",
                                      hjust = 0.5, color = "#666666",
                                      margin = margin(b = 8)),
      axis.title       = element_text(size = rel(1.0), face = "plain"),
      axis.title.x     = element_text(margin = margin(t = 10)),
      axis.title.y     = element_text(margin = margin(r = 10), angle = 90),
      axis.text        = element_text(size = rel(0.85), color = "#333333"),
      axis.text.x      = element_text(margin = if (ticks_inward) margin(t = 8) else margin(t = 4)),
      axis.text.y      = element_text(margin = if (ticks_inward) margin(r = 8) else margin(r = 4)),
      axis.line        = element_line(color = "#1a1a1a", linewidth = 0.5),
      axis.ticks       = element_line(color = "#1a1a1a", linewidth = 0.35),
      axis.ticks.length = tick_len,
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid       = element_blank(),
      plot.background  = element_rect(fill = "white", color = NA),
      legend.background = element_rect(fill = alpha("white", 0.95),
                                       color = "#cccccc", linewidth = 0.3),
      legend.key       = element_rect(fill = "white", color = NA),
      legend.key.size  = unit(1.1, "lines"),
      legend.text      = element_text(size = rel(0.78)),
      legend.title     = element_blank(),
      legend.margin    = margin(4, 6, 4, 6),
      plot.margin      = margin(12, 12, 12, 12),
      strip.background = element_rect(fill = "#f0f0f0", color = "#cccccc"),
      strip.text       = element_text(size = rel(0.9), face = "bold")
    )

  if (border) {
    t <- t + theme(
      panel.border = element_rect(fill = NA, color = "#1a1a1a", linewidth = 0.7),
      axis.line    = element_blank()
    )
  }

  if (grid == "major") {
    t <- t + theme(
      panel.grid.major = element_line(color = "#e8e8e8", linewidth = 0.3)
    )
  } else if (grid == "both") {
    t <- t + theme(
      panel.grid.major = element_line(color = "#e8e8e8", linewidth = 0.3),
      panel.grid.minor = element_line(color = "#f2f2f2", linewidth = 0.15)
    )
  } else if (grid == "x") {
    t <- t + theme(
      panel.grid.major.x = element_line(color = "#e8e8e8", linewidth = 0.3)
    )
  } else if (grid == "y") {
    t <- t + theme(
      panel.grid.major.y = element_line(color = "#e8e8e8", linewidth = 0.3)
    )
  }

  t
}

# ── Parse _{sub} and ^{sup} to plotmath expressions ──────────────────────────
parse_label <- function(s) {
  if (is.null(s) || nchar(trimws(s)) == 0) return("")
  # Replace _{...} with [...]  and ^{...} with [...] for plotmath
  has_markup <- grepl("(_\\{|\\^\\{)", s)
  if (!has_markup) return(s)
  expr_str <- s
  expr_str <- gsub("_\\{([^}]*)\\}", "[\\1]", expr_str, perl = TRUE)
  expr_str <- gsub("\\^\\{([^}]*)\\}", "^{\\1}", expr_str, perl = TRUE)
  # Wrap plain text segments in quotes for plotmath
  expr_str <- gsub("([A-Za-z][A-Za-z0-9 .,]*)", "\"\\1\"", expr_str, perl = TRUE)
  # Clean double-quoted numbers
  expr_str <- gsub("\"([0-9.]+)\"", "\\1", expr_str, perl = TRUE)
  tryCatch(parse(text = expr_str)[[1]], error = function(e) s)
}

# ── Legend position helpers ───────────────────────────────────────────────────
leg_pos <- function(pos) {
  switch(pos,
    "Top Left" = c(0.02, 0.98), "Top Center" = c(0.50, 0.98),
    "Top Right" = c(0.98, 0.98), "Mid Left" = c(0.02, 0.50),
    "Mid Right" = c(0.98, 0.50), "Bottom Left" = c(0.02, 0.02),
    "Bottom Center" = c(0.50, 0.02), "Bottom Right" = c(0.98, 0.02),
    "Hidden" = "none", c(0.98, 0.02))
}
leg_just <- function(pos) {
  switch(pos,
    "Top Left" = c(0,1), "Top Center" = c(0.5,1), "Top Right" = c(1,1),
    "Mid Left" = c(0,0.5), "Mid Right" = c(1,0.5),
    "Bottom Left" = c(0,0), "Bottom Center" = c(0.5,0), "Bottom Right" = c(1,0),
    c(1,0))
}


# ═══════════════════════════════════════════════════════════════════════════════
#  UI
# ═══════════════════════════════════════════════════════════════════════════════

ui <- page_navbar(
  title = tags$span(
    tags$span("Academic Graph Studio",
              style = "font-weight:700;letter-spacing:-0.02em;"),
    tags$span(" \u2014 Publication-Quality Visualisation",
              style = "font-weight:300;font-size:0.75em;opacity:0.7;")
  ),
  id = "main_nav",
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    bg = "#fdfcfa", fg = "#1a1714",
    primary = "#2a4f6e", secondary = "#8b3a1e",
    success = "#2e6b45", info = "#1a6b6b",
    base_font = font_google("Inter"),
    heading_font = font_google("Inter"),
    code_font = font_google("JetBrains Mono"),
    "navbar-bg" = "#1a1714",
    "border-radius" = "4px"
  ),
  header = tags$head(tags$style(HTML("
    .card { border-color: #d4cec4 !important; }
    .card-header { background: #f7f4ef !important; border-bottom: 1px solid #d4cec4 !important;
                   font-size: 0.72rem; text-transform: uppercase; letter-spacing: 0.1em;
                   font-weight: 700; color: #7a7060; padding: 10px 16px; }
    .form-label { font-size: 0.68rem !important; text-transform: uppercase;
                  letter-spacing: 0.08em; color: #7a7060 !important; font-weight: 600 !important; }
    .form-control, .form-select { font-size: 0.82rem; border-color: #c8bfb0; }
    .form-control:focus, .form-select:focus { border-color: #2a4f6e; box-shadow: 0 0 0 2px rgba(42,79,110,0.15); }
    .btn-academic { background: #1a1714; color: #f7f4ef; font-size: 0.72rem; letter-spacing: 0.07em;
                    text-transform: uppercase; border: none; padding: 8px 16px; border-radius: 3px; }
    .btn-academic:hover { background: #8b3a1e; color: #f7f4ef; }
    .btn-export { background: #2a4f6e; color: #fff; font-size: 0.72rem; letter-spacing: 0.07em;
                  text-transform: uppercase; border: none; padding: 8px 20px; }
    .btn-export:hover { background: #1a3a5e; color: #fff; }
    .btn-ghost { background: transparent; border: 1px solid #c8bfb0; color: #7a7060;
                 font-size: 0.68rem; letter-spacing: 0.07em; text-transform: uppercase; }
    .btn-ghost:hover { border-color: #8b3a1e; color: #8b3a1e; }
    .series-chip { display: inline-flex; align-items: center; gap: 6px; padding: 5px 10px;
                   margin: 2px; border-radius: 3px; font-size: 0.72rem; border: 1px solid #d4cec4;
                   background: #f7f4ef; cursor: pointer; transition: all 0.15s; }
    .series-chip:hover { border-color: #2a4f6e; }
    .series-chip.hidden { opacity: 0.35; }
    .series-swatch { width: 20px; height: 3px; border-radius: 2px; }
    .plot-container { background: white; border: 1px solid #d4cec4; border-radius: 4px;
                      padding: 8px; min-height: 500px; }
    .stat-card { background: #f7f4ef; border: 1px solid #d4cec4; border-radius: 4px;
                 padding: 12px 16px; text-align: center; }
    .stat-value { font-size: 1.5rem; font-weight: 700; color: #1a1714; }
    .stat-label { font-size: 0.65rem; text-transform: uppercase; letter-spacing: 0.1em;
                  color: #7a7060; margin-top: 2px; }
    .accordion-button { font-size: 0.72rem; text-transform: uppercase; letter-spacing: 0.08em;
                        font-weight: 600; color: #7a7060; padding: 10px 16px; }
    .accordion-button:not(.collapsed) { background: #f7f4ef; color: #1a1714; }
    .ref-item { background: #f7f4ef; border: 1px solid #d4cec4; border-radius: 3px;
                padding: 6px 10px; margin-top: 4px; font-size: 0.75rem;
                display: flex; align-items: center; gap: 8px; }
    .color-swatch { width: 14px; height: 14px; border-radius: 2px; border: 1px solid #d4cec4; display: inline-block; }
    .tab-content { padding-top: 0 !important; }
    .plotly .modebar { opacity: 0.4 !important; }
    .plotly .modebar:hover { opacity: 1 !important; }
  "))),

  # ═══════════════════════════════════════════════════════
  #  TAB 1: MAIN PLOTTER
  # ═══════════════════════════════════════════════════════
  nav_panel("Graph Studio", icon = icon("chart-line"),
    layout_sidebar(
      fillable = TRUE,
      sidebar = sidebar(
        width = 340,
        id = "sidebar",

        # ── DATA IMPORT ────────────────────────────────────
        accordion(
          id = "acc_sidebar",
          open = c("Import Data", "Series"),

          accordion_panel("Import Data", icon = icon("file-import"),
            fileInput("file_input", NULL,
                      accept = c(".xlsx", ".xls", ".csv"),
                      placeholder = "Drop .xlsx or .csv here"),
            uiOutput("sheet_selector"),
            uiOutput("column_selectors"),
            actionButton("load_btn", "Load into chart",
                         class = "btn-academic w-100", icon = icon("chart-line"))
          ),

          # ── SERIES MANAGEMENT ──────────────────────────────
          accordion_panel("Series", icon = icon("layer-group"),
            uiOutput("series_panel"),
            div(class = "d-flex gap-2 mt-2",
              actionButton("show_all", "Show All", class = "btn-ghost flex-fill"),
              actionButton("hide_all", "Hide All", class = "btn-ghost flex-fill")
            )
          ),

          # ── LABELS & TITLE ─────────────────────────────────
          accordion_panel("Labels & Title", icon = icon("font"),
            textInput("chart_title", "Chart title", value = "Pipeline Profile"),
            textInput("chart_subtitle", "Subtitle (optional)", value = ""),
            textInput("xlabel", "X axis label", value = "Pipeline Length [m]"),
            textInput("ylabel", "Y axis label", value = "Pressure [bara]"),
            helpText("Tip: _{...} = subscript, ^{...} = superscript",
                     style = "font-size:0.62rem;color:#999;font-style:italic;"),
            numericInput("title_size", "Title font size", value = 16, min = 8, max = 32, step = 1),
            numericInput("axis_label_size", "Axis label size", value = 14, min = 8, max = 28, step = 1),
            numericInput("axis_text_size", "Tick label size", value = 12, min = 6, max = 24, step = 1)
          ),

          # ── APPEARANCE ─────────────────────────────────────
          accordion_panel("Appearance", icon = icon("palette"),
            selectInput("palette", "Colour palette",
                        choices = names(PALETTES), selected = "Classic Academic"),
            radioButtons("curve_type", "Curve fitting",
                         choices = c("Smooth (loess)" = "loess",
                                     "Natural spline" = "spline",
                                     "Straight lines" = "linear",
                                     "Step" = "step",
                                     "None (markers only)" = "none"),
                         selected = "loess"),
            conditionalPanel("input.curve_type == 'loess'",
              sliderInput("loess_span", "Smoothness", min = 0.1, max = 1.5,
                          value = 0.35, step = 0.05)
            ),
            selectInput("line_type", "Line style", choices = LINE_TYPES, selected = "solid"),
            sliderInput("line_weight", "Line weight", min = 0.3, max = 3, value = 0.9, step = 0.1),
            hr(),
            radioButtons("marker_mode", "Markers",
                         choices = c("All" = "all", "Every N" = "nth",
                                     "First & Last" = "ends", "None" = "none"),
                         selected = "all", inline = TRUE),
            conditionalPanel("input.marker_mode == 'nth'",
              sliderInput("marker_nth", "Show every Nth point",
                          min = 2, max = 100, value = 10, step = 1)
            ),
            sliderInput("marker_size", "Marker size", min = 1, max = 8, value = 2.8, step = 0.2),
            hr(),
            selectInput("grid_lines", "Grid lines",
                        choices = c("None" = "none", "Major" = "major",
                                    "Major + Minor" = "both",
                                    "X only" = "x", "Y only" = "y"),
                        selected = "none"),
            checkboxInput("show_border", "Plot border", value = TRUE),
            radioButtons("tick_dir", "Tick marks",
                         choices = c("Inward" = "in", "Outward" = "out", "Both" = "both"),
                         selected = "in", inline = TRUE)
          ),

          # ── AXIS RANGES ────────────────────────────────────
          accordion_panel("Axis Ranges", icon = icon("arrows-left-right"),
            fluidRow(
              column(6, numericInput("xmin", "X min", value = NA, step = 0.1)),
              column(6, numericInput("xmax", "X max", value = NA, step = 0.1))
            ),
            fluidRow(
              column(6, numericInput("ymin", "Y min", value = NA, step = 0.1)),
              column(6, numericInput("ymax", "Y max", value = NA, step = 0.1))
            ),
            checkboxInput("y2_enable", "Enable secondary Y axis", value = FALSE),
            conditionalPanel("input.y2_enable",
              textInput("y2label", "Y2 axis label", value = "Temperature [\u00b0C]"),
              uiOutput("y2_series_selector")
            ),
            actionButton("reset_ranges", "Reset to auto", class = "btn-ghost w-100")
          ),

          # ── REFERENCE LINES ────────────────────────────────
          accordion_panel("Reference Lines", icon = icon("grip-lines"),
            fluidRow(
              column(6, selectInput("ref_axis", "Axis",
                                    choices = c("X (vertical)" = "x", "Y (horizontal)" = "y"))),
              column(6, numericInput("ref_value", "Value", value = NA, step = 0.1))
            ),
            textInput("ref_label", "Label", value = ""),
            fluidRow(
              column(6, selectInput("ref_linetype", "Style", choices = LINE_TYPES, selected = "dashed")),
              column(6, colourInput("ref_color", "Color", value = "#8b3a1e", showColour = "both"))
            ),
            actionButton("add_ref", "Add line", class = "btn-ghost w-100", icon = icon("plus")),
            uiOutput("ref_lines_list")
          ),

          # ── ANNOTATIONS ────────────────────────────────────
          accordion_panel("Annotations", icon = icon("comment-dots"),
            fluidRow(
              column(4, numericInput("ann_x", "X", value = NA, step = 0.1)),
              column(4, numericInput("ann_y", "Y", value = NA, step = 0.1)),
              column(4, colourInput("ann_color", "Col", value = "#8b3a1e", showColour = "background"))
            ),
            textInput("ann_text", "Text", value = ""),
            selectInput("ann_arrow", "Arrow",
                        choices = c("None" = "none", "Up-Right" = "ur", "Up-Left" = "ul",
                                    "Down-Right" = "dr", "Down-Left" = "dl"),
                        selected = "ur"),
            actionButton("add_ann", "Add annotation", class = "btn-ghost w-100", icon = icon("plus")),
            uiOutput("annotations_list")
          ),

          # ── LEGEND ─────────────────────────────────────────
          accordion_panel("Legend", icon = icon("list"),
            selectInput("legend_pos", "Position",
                        choices = c("Top Left", "Top Center", "Top Right",
                                    "Mid Left", "Mid Right",
                                    "Bottom Left", "Bottom Center", "Bottom Right",
                                    "Hidden"),
                        selected = "Bottom Right"),
            numericInput("legend_size", "Text size", value = 10, min = 6, max = 20, step = 1),
            numericInput("legend_cols", "Columns", value = 1, min = 1, max = 5, step = 1)
          ),

          # ── TOP AXIS ANNOTATIONS ─────────────────────────────
          accordion_panel("Top Axis Annotations", icon = icon("arrow-up-short-wide"),
            helpText("Show associated values above the plot at each X position (e.g. mass, watercut per year).",
                     style = "font-size:0.65rem;color:#999;font-style:italic;margin-bottom:8px;"),
            checkboxInput("top_ann_enable", "Enable top-axis annotations", value = FALSE),
            conditionalPanel("input.top_ann_enable",
              uiOutput("top_ann_col_selectors"),
              numericInput("top_ann_size", "Font size", value = 3, min = 1.5, max = 8, step = 0.25),
              numericInput("top_ann_angle", "Text angle", value = 0, min = 0, max = 90, step = 15),
              colourInput("top_ann_color", "Text colour", value = "#555555", showColour = "both"),
              checkboxInput("top_ann_ticks", "Show tick marks on top axis", value = TRUE),
              hr(),
              tags$p("Manual entries (if no column selected):",
                     style = "font-size:0.65rem;color:#999;margin-bottom:6px;"),
              fluidRow(
                column(4, numericInput("top_ann_x", "X", value = NA, step = 0.1)),
                column(4, textInput("top_ann_row1", "Row 1", value = "")),
                column(4, textInput("top_ann_row2", "Row 2", value = ""))
              ),
              actionButton("add_top_ann", "Add", class = "btn-ghost w-100", icon = icon("plus")),
              uiOutput("top_ann_manual_list")
            )
          )
        ) # end accordion
      ), # end sidebar

      # ── MAIN CONTENT ─────────────────────────────────────
      layout_column_wrap(
        width = 1,

        # Toolbar
        card(
          card_header(
            class = "d-flex align-items-center gap-3 flex-wrap",
            div(class = "d-flex align-items-center gap-2 flex-grow-1",
              selectInput("journal_preset", NULL,
                          choices = names(JOURNAL_PRESETS),
                          selected = "Custom", width = "180px"),
              numericInput("export_width", "W (in)", value = 10,
                           min = 2, max = 24, step = 0.25, width = "85px"),
              numericInput("export_height", "H (in)", value = 6,
                           min = 2, max = 16, step = 0.25, width = "85px"),
              numericInput("export_dpi", "DPI", value = 300,
                           min = 72, max = 1200, step = 50, width = "80px"),
              selectInput("export_format", "Format",
                          choices = c("SVG" = "svg", "PDF" = "pdf",
                                      "PNG" = "png", "TIFF" = "tiff",
                                      "EPS" = "eps"),
                          selected = "svg", width = "80px")
            ),
            div(class = "d-flex align-items-center gap-2",
              textInput("export_filename", NULL, value = "figure_1",
                        placeholder = "filename", width = "140px"),
              downloadButton("download_plot", "Export",
                             class = "btn-export", icon = icon("download"))
            )
          )
        ),

        # Interactive preview
        card(
          card_body(
            class = "plot-container p-1",
            div(
              style = "position:relative;",
              plotlyOutput("interactive_plot", height = "620px"),
              div(style = "position:absolute;top:8px;right:12px;z-index:10;",
                actionButton("refresh_plot", "", icon = icon("sync"),
                             class = "btn btn-sm btn-ghost",
                             title = "Refresh plot")
              )
            )
          )
        )
      )
    ) # end layout_sidebar
  ), # end nav_panel

  # ═══════════════════════════════════════════════════════
  #  TAB 2: STATIC PUBLICATION PREVIEW
  # ═══════════════════════════════════════════════════════
  nav_panel("Publication Preview", icon = icon("file-pdf"),
    card(
      card_header("Exact export preview (static ggplot2 — what you'll get in the file)"),
      card_body(
        class = "plot-container text-center",
        plotOutput("static_plot", height = "650px", width = "100%")
      )
    )
  ),

  # ═══════════════════════════════════════════════════════
  #  TAB 3: DATA TABLE
  # ═══════════════════════════════════════════════════════
  nav_panel("Data", icon = icon("table"),
    card(
      card_header("Imported data"),
      card_body(
        tableOutput("data_table")
      )
    )
  )
)


# ═══════════════════════════════════════════════════════════════════════════════
#  SERVER
# ═══════════════════════════════════════════════════════════════════════════════

server <- function(input, output, session) {

  # ── Reactive state ──────────────────────────────────────────────────────────
  rv <- reactiveValues(
    raw_data       = NULL,
    sheet_names    = NULL,
    series_data    = list(),
    ref_lines      = list(),
    annotations    = list(),
    top_anns       = list(),   # manual top-axis annotations
    is_flow_regime = FALSE,
    plot_counter   = 0         # force refresh
  )

  # ── Journal preset ─────────────────────────────────────────────────────────
  observeEvent(input$journal_preset, {
    preset <- JOURNAL_PRESETS[[input$journal_preset]]
    if (!is.null(preset) && input$journal_preset != "Custom") {
      updateNumericInput(session, "export_width",  value = preset$w)
      updateNumericInput(session, "export_height", value = preset$h)
      updateNumericInput(session, "export_dpi",    value = preset$dpi)
      updateSelectInput(session, "export_format",  selected = preset$fmt)
    }
  })

  # ══════════════════════════════════════════════════════
  #  FILE UPLOAD
  # ══════════════════════════════════════════════════════
  observeEvent(input$file_input, {
    req(input$file_input)
    f <- input$file_input
    ext <- tolower(tools::file_ext(f$name))
    tryCatch({
      if (ext == "csv") {
        df <- read.csv(f$datapath, stringsAsFactors = FALSE, check.names = FALSE)
        rv$raw_data <- list(Sheet1 = df)
        rv$sheet_names <- "Sheet1"
      } else {
        sheets <- excel_sheets(f$datapath)
        rv$sheet_names <- sheets
        rv$raw_data <- setNames(
          lapply(sheets, function(s) {
            as.data.frame(read_excel(f$datapath, sheet = s, col_names = TRUE,
                                     .name_repair = "minimal"))
          }),
          sheets
        )
      }
      showNotification(
        paste0("\u2713 Loaded \"", f$name, "\" \u2014 ", length(rv$sheet_names), " sheet(s)"),
        type = "message", duration = 4
      )
    }, error = function(e) {
      showNotification(paste0("\u2717 ", e$message), type = "error")
    })
  })

  # ── Sheet selector ──────────────────────────────────────────────────────────
  output$sheet_selector <- renderUI({
    req(rv$sheet_names)
    if (length(rv$sheet_names) > 1)
      selectInput("sheet_sel", "Sheet", choices = rv$sheet_names)
  })

  current_sheet <- reactive({
    req(rv$raw_data)
    nm <- if (!is.null(input$sheet_sel)) input$sheet_sel else rv$sheet_names[1]
    rv$raw_data[[nm]]
  })

  # ── Column selectors ───────────────────────────────────────────────────────
  output$column_selectors <- renderUI({
    df <- current_sheet()
    req(df)
    cols <- colnames(df)
    x_guess <- grep("length|distance|depth|x$", cols, ignore.case = TRUE, value = TRUE)
    x_default <- if (length(x_guess) > 0) x_guess[1] else cols[1]
    num_cols <- cols[vapply(df, function(c) {
      is.numeric(c) || all(!is.na(suppressWarnings(as.numeric(na.omit(c)))))
    }, logical(1))]
    y_default <- setdiff(num_cols, x_default)
    tagList(
      selectInput("col_x", "X column", choices = cols, selected = x_default),
      selectizeInput("col_y", "Y series (select multiple)", choices = cols,
                     selected = if (length(y_default) > 0) y_default else cols[min(2, length(cols))],
                     multiple = TRUE,
                     options = list(plugins = list("remove_button")))
    )
  })

  # ── Y2 axis series selector ────────────────────────────────────────────────
  output$y2_series_selector <- renderUI({
    series <- rv$series_data
    if (length(series) == 0) return(NULL)
    labels <- vapply(series, function(s) s$label, character(1))
    selectizeInput("y2_series", "Series on Y2 axis",
                   choices = labels, multiple = TRUE,
                   options = list(plugins = list("remove_button")))
  })

  # ══════════════════════════════════════════════════════
  #  LOAD DATA INTO CHART
  # ══════════════════════════════════════════════════════
  observeEvent(input$load_btn, {
    df <- current_sheet()
    req(df, input$col_x, input$col_y)

    pal <- PALETTES[[input$palette]]
    series_list <- list()
    x_col <- input$col_x
    y_cols <- input$col_y
    shape_cycle <- unname(MARKER_SHAPES)

    has_regime <- any(grepl("regime|flow.?regime", y_cols, ignore.case = TRUE))
    rv$is_flow_regime <- has_regime

    for (i in seq_along(y_cols)) {
      y_col <- y_cols[i]
      x_vals <- suppressWarnings(as.numeric(df[[x_col]]))
      y_vals <- suppressWarnings(as.numeric(df[[y_col]]))
      valid <- !is.na(x_vals) & !is.na(y_vals)
      x_vals <- x_vals[valid]; y_vals <- y_vals[valid]
      if (length(x_vals) == 0) next

      # Clean label
      label <- y_col
      m <- regmatches(label, regexpr('"([^"]+)\\.ppl"', label, perl = TRUE))
      if (length(m) > 0 && nchar(m) > 0) label <- gsub('^"|"$', "", gsub("\\.ppl", "", m))

      is_regime <- grepl("regime|flow.?regime", y_col, ignore.case = TRUE)

      series_list[[length(series_list) + 1]] <- list(
        label     = label,
        color     = pal[((i - 1) %% length(pal)) + 1],
        shape     = as.integer(shape_cycle[((i - 1) %% length(shape_cycle)) + 1]),
        linetype  = "solid",
        x         = x_vals,
        y         = y_vals,
        visible   = TRUE,
        is_regime = is_regime,
        on_y2     = FALSE
      )
    }

    rv$series_data <- series_list

    # Auto-set labels
    sheet_nm <- if (!is.null(input$sheet_sel)) input$sheet_sel else rv$sheet_names[1]
    updateTextInput(session, "chart_title", value = sheet_nm)
    updateTextInput(session, "export_filename",
                    value = tolower(gsub("[^A-Za-z0-9]+", "_", sheet_nm)))
    if (length(y_cols) > 0) {
      yl <- gsub('"[^"]*"', "", y_cols[1])
      yl <- trimws(gsub("\\(PIPELINE\\)", "", yl))
      if (nchar(yl) > 0) updateTextInput(session, "ylabel", value = yl)
    }

    rv$plot_counter <- rv$plot_counter + 1
    showNotification(paste0("\u2713 ", length(series_list), " series loaded"), type = "message")
  })

  # ── Re-apply palette when changed ──────────────────────────────────────────
  observeEvent(input$palette, {
    pal <- PALETTES[[input$palette]]
    series <- rv$series_data
    if (length(series) > 0) {
      for (i in seq_along(series)) {
        rv$series_data[[i]]$color <- pal[((i - 1) %% length(pal)) + 1]
      }
    }
  })

  # ══════════════════════════════════════════════════════
  #  SERIES PANEL — editable names, colours, visibility
  # ══════════════════════════════════════════════════════
  output$series_panel <- renderUI({
    series <- rv$series_data
    if (length(series) == 0)
      return(tags$p("No series loaded yet", style = "font-size:0.78rem;color:#999;font-style:italic;"))

    tagList(lapply(seq_along(series), function(i) {
      s <- series[[i]]
      div(
        style = paste0(
          "display:flex;align-items:center;gap:5px;padding:5px 8px;margin-bottom:4px;",
          "background:#f7f4ef;border:1px solid #d4cec4;border-radius:3px;",
          if (!s$visible) "opacity:0.35;" else ""
        ),
        # Colour picker (small swatch)
        colourInput(paste0("series_color_", i), NULL, value = s$color,
                    showColour = "background", palette = "limited",
                    returnName = FALSE),
        tags$style(HTML(paste0(
          "#series_color_", i, " { width:24px !important; height:24px !important; ",
          "padding:0 !important; border:1px solid #ccc !important; border-radius:2px !important; ",
          "min-height:unset !important; } ",
          "#series_color_", i, " + .input-group-addon { display:none !important; }"
        ))),
        # Editable series name
        textInput(paste0("series_name_", i), NULL, value = s$label),
        tags$style(HTML(paste0(
          "#series_name_", i, " { font-size:0.72rem !important; padding:3px 6px !important; ",
          "height:auto !important; margin:0 !important; flex:1; min-width:0; }"
        ))),
        # Toggle visibility button
        actionButton(paste0("toggle_vis_", i),
                     if (s$visible) icon("eye") else icon("eye-slash"),
                     class = "btn btn-sm btn-ghost",
                     style = "padding:2px 6px;min-width:28px;",
                     title = if (s$visible) "Hide series" else "Show series")
      )
    }))
  })

  # ── Series property observers ───────────────────────

  # Track how many series observers we've wired up so far
  rv_obs <- reactiveValues(n_series_obs = 0L)

  # Whenever the series count grows, create observers for the NEW indices only.
  # Each observeEvent is created exactly once per index (never duplicated).
  observe({
    n <- length(rv$series_data)
    prev <- isolate(rv_obs$n_series_obs)
    if (n > prev) {
      lapply((prev + 1L):n, function(i) {
        # Name edit
        observeEvent(input[[paste0("series_name_", i)]], {
          new_name <- input[[paste0("series_name_", i)]]
          if (!is.null(new_name) && nchar(trimws(new_name)) > 0 &&
              new_name != rv$series_data[[i]]$label) {
            rv$series_data[[i]]$label <- new_name
          }
        }, ignoreInit = TRUE)

        # Colour edit
        observeEvent(input[[paste0("series_color_", i)]], {
          new_col <- input[[paste0("series_color_", i)]]
          if (!is.null(new_col) && new_col != rv$series_data[[i]]$color) {
            rv$series_data[[i]]$color <- new_col
          }
        }, ignoreInit = TRUE)

        # Visibility toggle
        observeEvent(input[[paste0("toggle_vis_", i)]], {
          rv$series_data[[i]]$visible <- !rv$series_data[[i]]$visible
        }, ignoreInit = TRUE)
      })
      rv_obs$n_series_obs <- n
    }
  })

  observeEvent(input$show_all, {
    for (i in seq_along(rv$series_data)) rv$series_data[[i]]$visible <- TRUE
  })
  observeEvent(input$hide_all, {
    for (i in seq_along(rv$series_data)) rv$series_data[[i]]$visible <- FALSE
  })

  # ══════════════════════════════════════════════════════
  #  REFERENCE LINES
  # ══════════════════════════════════════════════════════
  observeEvent(input$add_ref, {
    req(input$ref_value)
    rv$ref_lines <- c(rv$ref_lines, list(list(
      axis     = input$ref_axis,
      value    = input$ref_value,
      label    = input$ref_label,
      color    = input$ref_color,
      linetype = input$ref_linetype
    )))
    updateNumericInput(session, "ref_value", value = NA)
    updateTextInput(session, "ref_label", value = "")
  })

  output$ref_lines_list <- renderUI({
    refs <- rv$ref_lines
    if (length(refs) == 0) return(NULL)
    tagList(lapply(seq_along(refs), function(i) {
      r <- refs[[i]]
      div(class = "ref-item",
        span(class = "color-swatch", style = paste0("background:", r$color)),
        span(paste0(if (r$axis == "x") "X=" else "Y=", r$value,
                    if (nchar(r$label) > 0) paste0("  \u2014 ", r$label)),
             style = "flex:1;color:#666;"),
        actionButton(paste0("rm_ref_", i), icon("xmark"),
                     class = "btn btn-sm btn-ghost", style = "padding:2px 6px;")
      )
    }))
  })

  observe({
    lapply(seq_along(rv$ref_lines), function(i) {
      observeEvent(input[[paste0("rm_ref_", i)]], {
        rv$ref_lines <- rv$ref_lines[-i]
      }, ignoreInit = TRUE, once = TRUE)
    })
  })

  # ══════════════════════════════════════════════════════
  #  ANNOTATIONS
  # ══════════════════════════════════════════════════════
  observeEvent(input$add_ann, {
    req(input$ann_x, input$ann_y, nchar(input$ann_text) > 0)
    rv$annotations <- c(rv$annotations, list(list(
      x = input$ann_x, y = input$ann_y,
      text = input$ann_text, color = input$ann_color,
      arrow = input$ann_arrow
    )))
    updateNumericInput(session, "ann_x", value = NA)
    updateNumericInput(session, "ann_y", value = NA)
    updateTextInput(session, "ann_text", value = "")
  })

  output$annotations_list <- renderUI({
    anns <- rv$annotations
    if (length(anns) == 0) return(NULL)
    tagList(lapply(seq_along(anns), function(i) {
      a <- anns[[i]]
      div(class = "ref-item",
        span(class = "color-swatch", style = paste0("background:", a$color, ";border-radius:50%;")),
        span(paste0("(", a$x, ", ", a$y, ") ", a$text),
             style = "flex:1;color:#666;font-size:0.7rem;"),
        actionButton(paste0("rm_ann_", i), icon("xmark"),
                     class = "btn btn-sm btn-ghost", style = "padding:2px 6px;")
      )
    }))
  })

  observe({
    lapply(seq_along(rv$annotations), function(i) {
      observeEvent(input[[paste0("rm_ann_", i)]], {
        rv$annotations <- rv$annotations[-i]
      }, ignoreInit = TRUE, once = TRUE)
    })
  })

  # ── Reset ranges ────────────────────────────────────────────────────────────
  observeEvent(input$reset_ranges, {
    updateNumericInput(session, "xmin", value = NA)
    updateNumericInput(session, "xmax", value = NA)
    updateNumericInput(session, "ymin", value = NA)
    updateNumericInput(session, "ymax", value = NA)
  })

  # ══════════════════════════════════════════════════════
  #  TOP AXIS ANNOTATIONS
  # ══════════════════════════════════════════════════════
  output$top_ann_col_selectors <- renderUI({
    df <- current_sheet()
    req(df)
    cols <- c("(none)" = "", colnames(df))
    tagList(
      selectInput("top_ann_col1", "Row 1 column (e.g. Mass)", choices = cols),
      textInput("top_ann_label1", "Row 1 label", value = "Mass"),
      selectInput("top_ann_col2", "Row 2 column (e.g. Watercut)", choices = cols),
      textInput("top_ann_label2", "Row 2 label", value = "Watercut")
    )
  })

  observeEvent(input$add_top_ann, {
    req(input$top_ann_x)
    rv$top_anns <- c(rv$top_anns, list(list(
      x = input$top_ann_x,
      row1 = input$top_ann_row1 %||% "",
      row2 = input$top_ann_row2 %||% ""
    )))
    updateNumericInput(session, "top_ann_x", value = NA)
    updateTextInput(session, "top_ann_row1", value = "")
    updateTextInput(session, "top_ann_row2", value = "")
  })

  output$top_ann_manual_list <- renderUI({
    anns <- rv$top_anns
    if (length(anns) == 0) return(NULL)
    tagList(lapply(seq_along(anns), function(i) {
      a <- anns[[i]]
      div(class = "ref-item",
        span(paste0("x=", a$x, ": ", a$row1, " / ", a$row2),
             style = "flex:1;color:#666;font-size:0.68rem;"),
        actionButton(paste0("rm_topann_", i), icon("xmark"),
                     class = "btn btn-sm btn-ghost", style = "padding:2px 6px;")
      )
    }))
  })

  observe({
    lapply(seq_along(rv$top_anns), function(i) {
      observeEvent(input[[paste0("rm_topann_", i)]], {
        rv$top_anns <- rv$top_anns[-i]
      }, ignoreInit = TRUE, once = TRUE)
    })
  })

  # ── Refresh plot ────────────────────────────────────────────────────────────
  observeEvent(input$refresh_plot, { rv$plot_counter <- rv$plot_counter + 1 })

  # ══════════════════════════════════════════════════════
  #  BUILD THE GGPLOT
  # ══════════════════════════════════════════════════════
  build_plot <- reactive({
    # Touch reactive dependencies
    rv$plot_counter
    series <- rv$series_data
    visible <- Filter(function(s) s$visible, series)

    # ── Empty state ─────────────────────────────────────
    if (length(visible) == 0) {
      p <- ggplot() + theme_academic(base_size = 14) +
        annotate("text", x = 0.5, y = 0.5,
                 label = "Import data to begin plotting",
                 size = 5.5, color = "#999999", fontface = "italic") +
        xlim(0, 1) + ylim(0, 1) +
        theme(axis.title = element_blank(), axis.text = element_blank(),
              axis.ticks = element_blank(), panel.border = element_blank(),
              axis.line = element_blank())
      return(p)
    }

    # ── Determine Y2 series ─────────────────────────────
    y2_labels <- if (input$y2_enable && !is.null(input$y2_series)) input$y2_series else character(0)

    # ── Build data frame ────────────────────────────────
    all_dfs <- lapply(visible, function(s) {
      data.frame(x = s$x, y = s$y, series = s$label, stringsAsFactors = FALSE)
    })
    plot_df <- do.call(rbind, all_dfs)
    plot_df$series <- factor(plot_df$series,
                             levels = vapply(visible, function(s) s$label, character(1)))

    # Maps
    color_map <- setNames(vapply(visible, function(s) s$color, character(1)),
                          vapply(visible, function(s) s$label, character(1)))
    shape_map <- setNames(
      vapply(visible, function(s) as.integer(s$shape), integer(1)),
      vapply(visible, function(s) s$label, character(1))
    )

    # ── Base plot ───────────────────────────────────────
    font_size <- input$axis_text_size %||% 12
    tick_inward <- (input$tick_dir %||% "in") != "out"
    p <- ggplot(plot_df, aes(x = x, y = y, color = series, shape = series)) +
      theme_academic(base_size = font_size,
                     grid = input$grid_lines %||% "none",
                     border = input$show_border %||% TRUE,
                     ticks_inward = tick_inward)

    # Handle "both" tick direction (inward + outward)
    if ((input$tick_dir %||% "in") == "both") {
      p <- p + theme(axis.ticks.length = unit(-4, "pt"))
      # Add outward ticks via a second axis with no labels
      # This is handled by minor axis overlay later
    }

    # ── Flow regime bands ───────────────────────────────
    if (rv$is_flow_regime) {
      regime_s <- Filter(function(s) s$is_regime, visible)
      if (length(regime_s) > 0) {
        rs <- regime_s[[1]]
        ord <- order(rs$x)
        for (j in seq_len(length(ord) - 1)) {
          code <- as.character(round(rs$y[ord[j]]))
          fr <- FLOW_REGIMES[[code]]
          if (!is.null(fr))
            p <- p + annotate("rect", xmin = rs$x[ord[j]], xmax = rs$x[ord[j+1]],
                              ymin = -Inf, ymax = Inf, fill = fr$fill, alpha = 0.08)
        }
      }
    }

    # ── Reference lines ─────────────────────────────────
    for (ref in rv$ref_lines) {
      if (ref$axis == "x") {
        p <- p + geom_vline(xintercept = ref$value, linetype = ref$linetype,
                            color = ref$color, linewidth = 0.6)
      } else {
        p <- p + geom_hline(yintercept = ref$value, linetype = ref$linetype,
                            color = ref$color, linewidth = 0.6)
      }
      if (nchar(ref$label) > 0) {
        p <- p + annotate("text",
          x = if (ref$axis == "x") ref$value else -Inf,
          y = if (ref$axis == "y") ref$value else Inf,
          label = ref$label, color = ref$color, size = 3.2,
          hjust = if (ref$axis == "x") -0.1 else -0.05,
          vjust = if (ref$axis == "y") -0.5 else 1.5)
      }
    }

    # ── Curves ──────────────────────────────────────────
    curve_type <- input$curve_type %||% "loess"
    lt <- input$line_type %||% "solid"
    lw <- input$line_weight %||% 0.9

    if (curve_type != "none") {
      for (s in visible) {
        sdf <- data.frame(x = s$x, y = s$y)
        sdf <- sdf[order(sdf$x), ]
        if (nrow(sdf) < 2) next

        if (curve_type == "loess" && nrow(sdf) >= 4) {
          span_val <- input$loess_span %||% 0.35
          p <- p + geom_smooth(data = sdf, aes(x = x, y = y), inherit.aes = FALSE,
                               method = "loess", formula = y ~ x, se = FALSE,
                               color = s$color, linetype = lt, linewidth = lw,
                               span = span_val, show.legend = FALSE)
        } else if (curve_type == "spline" && nrow(sdf) >= 4) {
          # Natural cubic spline interpolation
          n_interp <- max(200, nrow(sdf) * 10)
          xnew <- seq(min(sdf$x), max(sdf$x), length.out = n_interp)
          sp <- tryCatch(splinefun(sdf$x, sdf$y, method = "natural"),
                         error = function(e) NULL)
          if (!is.null(sp)) {
            spline_df <- data.frame(x = xnew, y = sp(xnew))
            p <- p + geom_line(data = spline_df, aes(x = x, y = y), inherit.aes = FALSE,
                               color = s$color, linetype = lt, linewidth = lw,
                               show.legend = FALSE)
          } else {
            p <- p + geom_line(data = sdf, aes(x = x, y = y), inherit.aes = FALSE,
                               color = s$color, linetype = lt, linewidth = lw,
                               show.legend = FALSE)
          }
        } else if (curve_type == "step") {
          p <- p + geom_step(data = sdf, aes(x = x, y = y), inherit.aes = FALSE,
                             color = s$color, linetype = lt, linewidth = lw,
                             show.legend = FALSE)
        } else {
          # Linear or fallback
          p <- p + geom_line(data = sdf, aes(x = x, y = y), inherit.aes = FALSE,
                             color = s$color, linetype = lt, linewidth = lw,
                             show.legend = FALSE)
        }
      }
    }

    # ── Markers ─────────────────────────────────────────
    mk_mode <- input$marker_mode %||% "all"
    mk_size <- input$marker_size %||% 2.8

    if (mk_mode != "none") {
      # Build a combined marker data frame with series factor for legend mapping
      marker_dfs <- list()
      for (s in visible) {
        sdf <- data.frame(x = s$x, y = s$y, series = s$label)
        sdf <- sdf[order(sdf$x), ]

        if (mk_mode == "nth") {
          nth <- input$marker_nth %||% 10
          idx <- seq(1, nrow(sdf), by = nth)
          idx <- sort(unique(c(1, idx, nrow(sdf))))
          sdf <- sdf[idx, ]
        } else if (mk_mode == "ends") {
          sdf <- sdf[c(1, nrow(sdf)), ]
        }
        marker_dfs[[length(marker_dfs) + 1]] <- sdf
      }
      marker_df <- do.call(rbind, marker_dfs)
      marker_df$series <- factor(marker_df$series,
                                  levels = vapply(visible, function(s) s$label, character(1)))

      # Use mapped aesthetics so the legend is generated automatically
      # Map fill = series so shapes 21-25 get filled interiors
      p <- p + geom_point(data = marker_df,
                          aes(x = x, y = y, color = series, shape = series, fill = series),
                          size = mk_size, stroke = 0.5)
    } else {
      # No markers — still need a mapped layer for legend
      p <- p + geom_point(data = plot_df,
                          aes(x = x, y = y, color = series),
                          alpha = 0, size = 0, show.legend = TRUE)
    }

    # ── Annotations ─────────────────────────────────────
    for (ann in rv$annotations) {
      x_rng <- range(plot_df$x, na.rm = TRUE)
      y_rng <- range(plot_df$y, na.rm = TRUE)
      dx <- diff(x_rng) * 0.04
      dy <- diff(y_rng) * 0.06

      # Arrow direction
      arr <- ann$arrow %||% "ur"
      nudge_x <- dx * ifelse(grepl("l", arr), -1, 1)
      nudge_y <- dy * ifelse(grepl("d", arr), -1, 1)

      if (arr != "none") {
        p <- p + annotate("segment",
          x = ann$x, y = ann$y,
          xend = ann$x + nudge_x * 0.6, yend = ann$y + nudge_y * 0.6,
          color = ann$color, linewidth = 0.4,
          arrow = arrow(length = unit(4, "pt"), type = "closed"))
      }
      p <- p + annotate("point", x = ann$x, y = ann$y,
                        size = 3.5, color = ann$color, shape = 16)
      p <- p + annotate("label", x = ann$x + nudge_x, y = ann$y + nudge_y,
                        label = ann$text, color = ann$color,
                        fill = alpha("white", 0.92), label.size = 0.25,
                        size = 3.2, label.padding = unit(3, "pt"))
    }

    # ── Scales ──────────────────────────────────────────
    p <- p + scale_color_manual(values = color_map) +
             scale_shape_manual(values = shape_map)

    # Build proper legend: line swatch + marker for each series
    # Use fill map for shapes 21-25 (filled shapes use fill, not color)
    fill_vals <- setNames(
      vapply(visible, function(s) {
        if (s$shape %in% FILLED_SHAPES) s$color else NA_character_
      }, character(1)),
      vapply(visible, function(s) s$label, character(1))
    )

    p <- p + scale_fill_manual(values = fill_vals, guide = "none")

    # Legend override: show colored line + correct per-series marker shape
    legend_overrides <- list(
      size = mk_size + 0.5,
      stroke = 0.5,
      linetype = lt,
      linewidth = lw,
      shape = vapply(visible, function(s) as.integer(s$shape), integer(1)),
      fill = vapply(visible, function(s) {
        if (s$shape %in% FILLED_SHAPES) s$color else NA_character_
      }, character(1))
    )

    if (mk_mode == "none") {
      legend_overrides$shape <- NA
      legend_overrides$size <- 0
    }

    p <- p + guides(
      color = guide_legend(
        ncol = input$legend_cols %||% 1,
        override.aes = legend_overrides
      ),
      shape = "none"
    )

    # ── Labels ──────────────────────────────────────────
    title_sz <- input$title_size %||% 16
    label_sz <- input$axis_label_size %||% 14
    leg_sz   <- input$legend_size %||% 10

    x_lab <- parse_label(input$xlabel)
    y_lab <- parse_label(input$ylabel)
    t_lab <- parse_label(input$chart_title)
    st_lab <- if (nchar(input$chart_subtitle %||% "") > 0) input$chart_subtitle else NULL

    p <- p + labs(x = x_lab, y = y_lab, title = t_lab, subtitle = st_lab)

    # Font size overrides
    p <- p + theme(
      plot.title    = element_text(size = title_sz, face = "bold", hjust = 0.5,
                                   margin = margin(b = 8)),
      axis.title    = element_text(size = label_sz),
      axis.text     = element_text(size = font_size),
      legend.text   = element_text(size = leg_sz)
    )

    # ── Axis ranges ─────────────────────────────────────
    x_lim <- c(if (!is.na(input$xmin)) input$xmin else NA,
               if (!is.na(input$xmax)) input$xmax else NA)
    y_lim <- c(if (!is.na(input$ymin)) input$ymin else NA,
               if (!is.na(input$ymax)) input$ymax else NA)

    # ── Smart X axis breaks: use integer breaks when data are integer-like ────
    x_all <- plot_df$x
    x_is_integer <- all(x_all == round(x_all), na.rm = TRUE) &&
                    diff(range(x_all, na.rm = TRUE)) <= 50
    x_breaks_fn <- if (x_is_integer) {
      x_range <- range(x_all, na.rm = TRUE)
      # Use every integer as a break
      function(lim) seq(ceiling(lim[1]), floor(lim[2]), by = max(1, round(diff(lim)/20)))
    } else {
      waiver()
    }
    x_labels_fn <- if (x_is_integer) {
      function(x) as.character(as.integer(x))
    } else {
      waiver()
    }

    # Only add x scale here if top-axis annotations won't override it later
    top_ann_active <- isTRUE(input$top_ann_enable) && isTRUE(input$top_ann_ticks)
    if (!top_ann_active) {
      if (!all(is.na(x_lim)))
        p <- p + scale_x_continuous(limits = x_lim, expand = expansion(mult = 0.02),
                                    breaks = x_breaks_fn, labels = x_labels_fn)
      else
        p <- p + scale_x_continuous(expand = expansion(mult = 0.02),
                                    breaks = x_breaks_fn, labels = x_labels_fn)
    }

    if (!all(is.na(y_lim)))
      p <- p + scale_y_continuous(limits = y_lim, expand = expansion(mult = 0.02))
    else
      p <- p + scale_y_continuous(expand = expansion(mult = 0.02))

    # ── Top-axis annotations ─────────────────────────────
    if (isTRUE(input$top_ann_enable)) {
      top_ann_sz    <- input$top_ann_size %||% 3
      top_ann_angle <- input$top_ann_angle %||% 0
      top_ann_col   <- input$top_ann_color %||% "#555555"
      top_ann_hjust <- if (top_ann_angle > 0) 0.5 else 0.5
      top_ann_vjust <- 1

      # Build top annotation data from columns or manual entries
      top_df <- NULL

      col1 <- input$top_ann_col1 %||% ""
      col2 <- input$top_ann_col2 %||% ""
      label1 <- input$top_ann_label1 %||% ""
      label2 <- input$top_ann_label2 %||% ""

      df <- tryCatch(current_sheet(), error = function(e) NULL)

      if (!is.null(df) && (nchar(col1) > 0 || nchar(col2) > 0)) {
        x_col <- input$col_x
        if (!is.null(x_col) && x_col %in% colnames(df)) {
          x_vals <- suppressWarnings(as.numeric(df[[x_col]]))

          rows1 <- if (nchar(col1) > 0 && col1 %in% colnames(df)) as.character(df[[col1]]) else rep("", length(x_vals))
          rows2 <- if (nchar(col2) > 0 && col2 %in% colnames(df)) as.character(df[[col2]]) else rep("", length(x_vals))

          valid <- !is.na(x_vals)
          if (any(valid)) {
            top_df <- data.frame(
              x = x_vals[valid],
              row1 = rows1[valid],
              row2 = rows2[valid],
              stringsAsFactors = FALSE
            )
            # Deduplicate by x (take first occurrence)
            top_df <- top_df[!duplicated(top_df$x), ]
          }
        }
      }

      # Add manual entries
      manual_anns <- rv$top_anns
      if (length(manual_anns) > 0) {
        manual_df <- data.frame(
          x = vapply(manual_anns, function(a) a$x, numeric(1)),
          row1 = vapply(manual_anns, function(a) a$row1, character(1)),
          row2 = vapply(manual_anns, function(a) a$row2, character(1)),
          stringsAsFactors = FALSE
        )
        top_df <- if (is.null(top_df)) manual_df else rbind(top_df, manual_df)
      }

      if (!is.null(top_df) && nrow(top_df) > 0) {
        y_upper <- max(plot_df$y, na.rm = TRUE)
        y_range <- diff(range(plot_df$y, na.rm = TRUE))

        has_row1 <- any(nchar(top_df$row1) > 0)
        has_row2 <- any(nchar(top_df$row2) > 0)
        both_rows <- has_row1 && has_row2

        # Build combined label: stack row1 / row2 with padding for readability
        top_df$combined <- mapply(function(r1, r2) {
          parts <- c()
          if (nchar(r1) > 0) parts <- c(parts, trimws(r1))
          if (nchar(r2) > 0) parts <- c(parts, trimws(r2))
          paste(parts, collapse = "\n")
        }, top_df$row1, top_df$row2, USE.NAMES = FALSE)

        # Header title for the top axis
        header_parts <- c()
        if (nchar(label1) > 0) header_parts <- c(header_parts, label1)
        if (nchar(label2) > 0) header_parts <- c(header_parts, label2)
        top_title <- paste(header_parts, collapse = "  /  ")

        # Use sec_axis for top tick marks
        if (isTRUE(input$top_ann_ticks)) {
          top_breaks <- top_df$x
          top_labels <- top_df$combined
          p <- p + scale_x_continuous(
            limits = if (!all(is.na(x_lim))) x_lim else NULL,
            expand = expansion(mult = 0.02),
            breaks = x_breaks_fn, labels = x_labels_fn,
            sec.axis = sec_axis(
              transform = ~ .,
              name = top_title,
              breaks = top_breaks,
              labels = top_labels
            )
          )
          # Increase lineheight when both rows are present so they don't overlap
          lh <- if (both_rows) 1.35 else 1.0
          p <- p + theme(
            axis.text.x.top = element_text(
              size = top_ann_sz * 2.5, color = top_ann_col,
              angle = top_ann_angle, hjust = top_ann_hjust,
              vjust = 0, lineheight = lh,
              margin = margin(b = 4)
            ),
            axis.title.x.top = element_text(
              size = top_ann_sz * 2.8, color = top_ann_col,
              face = "italic", margin = margin(b = 2)
            ),
            axis.ticks.x.top = element_line(color = "#999999", linewidth = 0.3),
            axis.ticks.length.x.top = unit(3, "pt"),
            # Extra top margin so the two-row labels don't clip
            plot.margin = margin(t = if (both_rows) 8 else 5, r = 12, b = 12, l = 12)
          )
        } else {
          # No tick marks — annotate text above plot area with clip off
          for (k in seq_len(nrow(top_df))) {
            p <- p + annotate("text",
              x = top_df$x[k], y = Inf,
              label = top_df$combined[k],
              color = top_ann_col, size = top_ann_sz,
              angle = top_ann_angle, hjust = 0.5, vjust = -0.3,
              lineheight = if (both_rows) 1.2 else 0.85)
          }
          if (nchar(top_title) > 0) {
            # Add title annotation above everything
            x_mid <- mean(range(top_df$x, na.rm = TRUE))
            p <- p + annotate("text",
              x = x_mid, y = Inf,
              label = top_title,
              color = top_ann_col, size = top_ann_sz * 1.1,
              fontface = "italic", hjust = 0.5, vjust = -1.8)
          }
          p <- p + coord_cartesian(clip = "off") +
            theme(plot.margin = margin(t = if (both_rows) 40 else 30, r = 12, b = 12, l = 12))
        }
      }
    }

    # ── Legend position ─────────────────────────────────
    lp <- input$legend_pos %||% "Bottom Right"
    if (lp == "Hidden") {
      p <- p + theme(legend.position = "none")
    } else {
      pos <- leg_pos(lp)
      jst <- leg_just(lp)
      p <- p + theme(
        legend.position = pos,
        legend.justification = jst,
        legend.position.inside = pos
      )
    }

    p
  })

  # ══════════════════════════════════════════════════════
  #  RENDER PLOTS
  # ══════════════════════════════════════════════════════

  # Interactive (plotly) preview
  output$interactive_plot <- renderPlotly({
    p <- build_plot()
    ggplotly(p, tooltip = c("x", "y", "colour")) %>%
      plotly::config(
        displayModeBar = TRUE,
        modeBarButtonsToAdd = list("hoverclosest", "hovercompare"),
        modeBarButtonsToRemove = list("lasso2d", "select2d"),
        displaylogo = FALSE
      ) %>%
      plotly::layout(
        hoverlabel = list(bgcolor = "white", font = list(family = "Inter", size = 12)),
        legend = list(font = list(family = "Inter", size = 11))
      )
  })

  # Static ggplot2 preview (publication exact)
  output$static_plot <- renderPlot({
    build_plot()
  }, res = 96, execOnResize = TRUE)

  # ══════════════════════════════════════════════════════
  #  DATA TABLE
  # ══════════════════════════════════════════════════════
  output$data_table <- renderTable({
    df <- current_sheet()
    req(df)
    head(df, 100)
  }, striped = TRUE, hover = TRUE, bordered = TRUE, spacing = "s",
     width = "100%")

  # ══════════════════════════════════════════════════════
  #  EXPORT
  # ══════════════════════════════════════════════════════
  output$download_plot <- downloadHandler(
    filename = function() {
      paste0(input$export_filename, ".", input$export_format)
    },
    content = function(file) {
      p <- build_plot()
      w   <- input$export_width
      h   <- input$export_height
      dpi <- input$export_dpi
      fmt <- input$export_format

      if (fmt == "eps") {
        ggsave(file, plot = p, width = w, height = h, device = cairo_ps, bg = "white")
      } else {
        ggsave(file, plot = p, width = w, height = h, dpi = dpi,
               device = fmt, bg = "white")
      }
    }
  )
}


# ═══════════════════════════════════════════════════════════════════════════════
#  LAUNCH
# ═══════════════════════════════════════════════════════════════════════════════
shinyApp(ui = ui, server = server)
