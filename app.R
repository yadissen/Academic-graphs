# ═══════════════════════════════════════════════════════════
#  Academic Pipeline Profile Plotter — R Shiny
#  Gold-standard publication-quality graphs
# ═══════════════════════════════════════════════════════════

library(shiny)
library(ggplot2)
library(readxl)
library(svglite)
library(splines)

# ── Academic colour palette (12 distinct, print-safe) ─────
SERIES_COLORS <- c(
  "#8b3a1e", "#2a4f6e", "#2e6b45", "#7a5318",

  "#5b2d8e", "#1a6b6b", "#8b1a4a", "#3d5a1e",
  "#1e3a6e", "#8b6b00", "#4a1a2e", "#006b4e"
)

# ── Academic marker shapes ────────────────────────────────
MARKER_SHAPES <- c(16, 15, 17, 18, 25, 4, 1, 0, 2, 5, 3, 6)
# circle, square, triangle-up, diamond, triangle-down, cross,
# circle-open, square-open, triangle-up-open, diamond-open, plus, triangle-down-open

# ── Flow regime definitions ───────────────────────────────
FLOW_REGIMES <- list(
  `1` = list(label = "Stratified", color = "rgba(42,79,110,0.12)",  fill = "#2a4f6e"),
  `2` = list(label = "Wavy",       color = "rgba(46,107,69,0.12)",  fill = "#2e6b45"),
  `3` = list(label = "Slug",       color = "rgba(139,58,30,0.12)",  fill = "#8b3a1e"),
  `4` = list(label = "Annular",    color = "rgba(122,83,24,0.12)",  fill = "#7a5318"),
  `5` = list(label = "Bubble",     color = "rgba(91,45,142,0.12)",  fill = "#5b2d8e"),
  `6` = list(label = "Dispersed",  color = "rgba(26,107,107,0.12)", fill = "#1a6b6b")
)

# ── Academic ggplot2 theme ────────────────────────────────
theme_academic <- function(base_size = 14) {
  theme_classic(base_size = base_size) %+replace%
    theme(
      # Text
      text = element_text(family = "Helvetica", color = "#1a1714"),
      plot.title = element_text(size = rel(1.3), face = "bold",
                                hjust = 0.5, margin = margin(b = 12)),
      plot.subtitle = element_text(size = rel(0.9), face = "italic",
                                   hjust = 0.5, color = "#7a7060",
                                   margin = margin(b = 10)),
      # Axes
      axis.title = element_text(size = rel(1.1), face = "plain"),
      axis.title.x = element_text(margin = margin(t = 10)),
      axis.title.y = element_text(margin = margin(r = 10)),
      axis.text = element_text(size = rel(0.9), color = "#1a1714"),
      axis.line = element_line(color = "#1a1a1a", linewidth = 0.6),
      axis.ticks = element_line(color = "#1a1a1a", linewidth = 0.4),
      axis.ticks.length = unit(4, "pt"),
      # Panel
      panel.background = element_rect(fill = "white", color = NA),
      panel.border = element_rect(fill = NA, color = "#1a1a1a", linewidth = 0.8),
      panel.grid = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      # Legend
      legend.background = element_rect(fill = "white", color = "#bbbbbb", linewidth = 0.4),
      legend.key = element_rect(fill = "white", color = NA),
      legend.key.size = unit(1.2, "lines"),
      legend.text = element_text(size = rel(0.8)),
      legend.title = element_blank(),
      legend.margin = margin(6, 8, 6, 8),
      # Margins
      plot.margin = margin(15, 15, 15, 15)
    )
}

# ── Parse subscript/superscript markup for plotmath ───────
parse_label <- function(s) {
  if (is.null(s) || s == "") return(expression())
  # Convert _{...} to subscript and ^{...} to superscript
  # Use plotmath expressions
  s <- gsub("\\^\\{([^}]*)\\}", "~superscript('\\1')", s, perl = TRUE)
  s <- gsub("_\\{([^}]*)\\}", "~subscript('\\1')", s, perl = TRUE)
  # If no special markup, return as-is
  if (!grepl("superscript|subscript", s)) {
    return(s)
  }
  tryCatch(parse(text = s), error = function(e) s)
}

# ── Legend position mapping ───────────────────────────────
legend_pos_map <- function(pos) {
  switch(pos,
    "top-left"    = c(0.02, 0.98),
    "top-center"  = c(0.50, 0.98),
    "top-right"   = c(0.98, 0.98),
    "mid-left"    = c(0.02, 0.50),
    "mid-right"   = c(0.98, 0.50),
    "bot-left"    = c(0.02, 0.02),
    "bot-center"  = c(0.50, 0.02),
    "bot-right"   = c(0.98, 0.02),
    "hidden"      = "none",
    c(0.98, 0.02)
  )
}

legend_just_map <- function(pos) {
  switch(pos,
    "top-left"    = c(0, 1),
    "top-center"  = c(0.5, 1),
    "top-right"   = c(1, 1),
    "mid-left"    = c(0, 0.5),
    "mid-right"   = c(1, 0.5),
    "bot-left"    = c(0, 0),
    "bot-center"  = c(0.5, 0),
    "bot-right"   = c(1, 0),
    c(1, 0)
  )
}


# ═══════════════════════════════════════════════════════════
#  UI
# ═══════════════════════════════════════════════════════════
ui <- fluidPage(
  # ── Custom CSS for academic styling ─────────────────────
  tags$head(
    tags$style(HTML("
      :root {
        --bg:     #f7f4ef;
        --paper:  #fdfcfa;
        --ink:    #1a1714;
        --rule:   #c8bfb0;
        --accent: #8b3a1e;
        --blue:   #2a4f6e;
        --green:  #2e6b45;
        --muted:  #7a7060;
      }
      body {
        background: var(--bg) !important;
        font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;
        color: var(--ink);
      }
      .main-header {
        text-align: center;
        padding: 24px 0 16px;
      }
      .main-header h1 {
        font-size: 1.9rem;
        font-weight: 600;
        letter-spacing: -0.01em;
        color: var(--ink);
        margin: 0;
      }
      .main-header p {
        font-size: 0.95rem;
        font-style: italic;
        color: var(--muted);
        margin-top: 4px;
      }
      .sidebar-panel {
        background: var(--paper) !important;
        border: 1px solid var(--rule) !important;
        border-radius: 4px;
      }
      .well {
        background: var(--paper) !important;
        border: 1px solid var(--rule) !important;
        border-radius: 4px;
      }
      .panel-title {
        font-size: 0.65rem;
        text-transform: uppercase;
        letter-spacing: 0.12em;
        color: var(--muted);
        margin-bottom: 8px;
        padding-bottom: 6px;
        border-bottom: 1px solid #e4ddd4;
        font-weight: 600;
      }
      .btn-academic {
        background: var(--ink);
        color: var(--bg);
        font-size: 0.75rem;
        letter-spacing: 0.07em;
        text-transform: uppercase;
        border: none;
        padding: 6px 14px;
        border-radius: 2px;
        cursor: pointer;
        width: 100%;
        transition: background 0.15s;
      }
      .btn-academic:hover {
        background: var(--accent);
        color: var(--bg);
      }
      .btn-export {
        background: #8b6b1e;
        color: #fff;
        border: none;
        font-size: 0.72rem;
        letter-spacing: 0.07em;
        text-transform: uppercase;
        padding: 7px 16px;
        border-radius: 2px;
        cursor: pointer;
        transition: background 0.15s;
      }
      .btn-export:hover {
        background: #6b4e0e;
        color: #fff;
      }
      .btn-ghost {
        background: transparent;
        border: 1px solid var(--rule);
        color: var(--muted);
        font-size: 0.68rem;
        letter-spacing: 0.07em;
        text-transform: uppercase;
        padding: 5px 12px;
        border-radius: 2px;
        transition: all 0.12s;
      }
      .btn-ghost:hover {
        border-color: var(--accent);
        color: var(--accent);
      }
      .chart-wrap {
        background: var(--paper);
        border: 1px solid var(--rule);
        border-radius: 4px;
        padding: 16px;
      }
      label {
        font-size: 0.68rem !important;
        text-transform: uppercase;
        letter-spacing: 0.1em;
        color: var(--muted) !important;
        font-weight: 600 !important;
      }
      .form-control {
        font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;
        font-size: 0.82rem;
        border: 1px solid var(--rule);
        background: var(--bg);
        color: var(--ink);
        border-radius: 2px;
      }
      .form-control:focus {
        border-color: var(--blue);
        box-shadow: none;
      }
      .selectize-input {
        font-size: 0.82rem !important;
        border: 1px solid var(--rule) !important;
        background: var(--bg) !important;
        border-radius: 2px !important;
        box-shadow: none !important;
      }
      .series-badge {
        display: inline-block;
        padding: 2px 8px;
        margin: 2px;
        border-radius: 2px;
        font-size: 0.72rem;
        font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;
      }
      .ref-line-item {
        background: var(--bg);
        border: 1px solid var(--rule);
        border-radius: 2px;
        padding: 5px 8px;
        margin-top: 5px;
        font-size: 0.75rem;
      }
      hr {
        border-top: 1px solid #e4ddd4;
      }
      .nav-tabs > li > a {
        font-size: 0.72rem;
        text-transform: uppercase;
        letter-spacing: 0.07em;
        color: var(--muted);
      }
      .nav-tabs > li.active > a {
        color: var(--ink);
        border-bottom-color: var(--accent);
      }
      .shiny-input-container { margin-bottom: 8px; }
    "))
  ),

  # ── Header ──────────────────────────────────────────────
  div(class = "main-header",
    h1("Pipeline Profile Plotter"),
    tags$p("Multi-series import \u00b7 Pressure, Temperature & Flow profiles \u00b7 Publication-quality export")
  ),

  # ── Layout ──────────────────────────────────────────────
  sidebarLayout(
    # ── Sidebar ───────────────────────────────────────────
    sidebarPanel(width = 3, class = "sidebar-panel",
      # Import panel
      div(class = "panel-title", "IMPORT"),
      fileInput("file_input", NULL,
                accept = c(".xlsx", ".xls", ".csv"),
                placeholder = "Drag & drop or browse (.xlsx/.csv)"),
      uiOutput("sheet_selector"),
      uiOutput("column_selectors"),
      actionButton("load_btn", "Load into chart", class = "btn-academic"),
      hr(),

      # Axis Labels
      div(class = "panel-title", "AXIS LABELS"),
      textInput("chart_title", "Chart title", value = "Pipeline Profile"),
      textInput("xlabel", "X axis", value = "Pipeline Length [m]"),
      helpText("Use _{...} for subscript, ^{...} for superscript", style = "font-size:0.65rem;color:#7a7060;font-style:italic;"),
      textInput("ylabel", "Y axis", value = "y"),
      hr(),

      # Curve options
      div(class = "panel-title", "SMOOTH CURVE"),
      radioButtons("show_spline", NULL,
                   choices = c("Show" = "show", "Hide" = "hide"),
                   selected = "show", inline = TRUE),
      radioButtons("spline_style", "Spline style",
                   choices = c("Solid" = "solid", "Dashed" = "dashed"),
                   selected = "solid", inline = TRUE),
      hr(),

      # Markers
      div(class = "panel-title", "MARKERS"),
      radioButtons("marker_mode", NULL,
                   choices = c("All" = "all", "Every N" = "nth", "None" = "none"),
                   selected = "all", inline = TRUE),
      conditionalPanel(
        condition = "input.marker_mode == 'nth'",
        sliderInput("marker_nth", "Show every Nth point",
                    min = 2, max = 50, value = 10, step = 1)
      ),
      hr(),

      # Axis Ranges
      div(class = "panel-title", "AXIS RANGES"),
      fluidRow(
        column(6, numericInput("xmin", "X min", value = NA, step = 0.1)),
        column(6, numericInput("xmax", "X max", value = NA, step = 0.1))
      ),
      fluidRow(
        column(6, numericInput("ymin", "Y min", value = NA, step = 0.1)),
        column(6, numericInput("ymax", "Y max", value = NA, step = 0.1))
      ),
      actionButton("reset_ranges", "Reset to auto", class = "btn-ghost", style = "width:100%;"),
      hr(),

      # Reference Lines
      div(class = "panel-title", "REFERENCE LINES"),
      fluidRow(
        column(6, selectInput("ref_axis", "Axis",
                              choices = c("X (vertical)" = "x", "Y (horizontal)" = "y"))),
        column(6, numericInput("ref_value", "Value", value = NA, step = 0.1))
      ),
      textInput("ref_label", "Label (optional)", value = ""),
      fluidRow(
        column(6, selectInput("ref_side", "Label side",
                              choices = c("Left" = "left", "Right" = "right"))),
        column(6, selectInput("ref_pos", "Label position",
                              choices = c("Above" = "above", "Below" = "below")))
      ),
      actionButton("add_ref", "Add line", class = "btn-ghost", style = "width:100%;"),
      uiOutput("ref_lines_list"),
      hr(),

      # Annotations
      div(class = "panel-title", "ANNOTATIONS"),
      fluidRow(
        column(6, numericInput("ann_x", "X", value = NA, step = 0.1)),
        column(6, numericInput("ann_y", "Y", value = NA, step = 0.1))
      ),
      textInput("ann_text", "Text", value = ""),
      selectInput("ann_color", "Color",
                  choices = setNames(SERIES_COLORS, paste0("Color ", 1:12)),
                  selected = SERIES_COLORS[1]),
      actionButton("add_ann", "Add annotation", class = "btn-ghost", style = "width:100%;"),
      uiOutput("annotations_list"),
      hr(),

      # Legend Position
      div(class = "panel-title", "LEGEND POSITION"),
      selectInput("legend_pos", NULL,
                  choices = c("Top Left" = "top-left", "Top Center" = "top-center",
                              "Top Right" = "top-right", "Mid Left" = "mid-left",
                              "Mid Right" = "mid-right", "Bottom Left" = "bot-left",
                              "Bottom Center" = "bot-center", "Bottom Right" = "bot-right",
                              "Hidden" = "hidden"),
                  selected = "bot-right"),
      hr(),

      # Series visibility
      div(class = "panel-title", "SERIES"),
      uiOutput("series_toggles"),
      fluidRow(
        column(6, actionButton("show_all", "Show All", class = "btn-ghost", style = "width:100%;font-size:0.65rem;")),
        column(6, actionButton("hide_all", "Hide All", class = "btn-ghost", style = "width:100%;font-size:0.65rem;"))
      )
    ),

    # ── Main panel ────────────────────────────────────────
    mainPanel(width = 9,
      div(class = "chart-wrap",
        # Toolbar
        fluidRow(
          column(4,
            textInput("export_filename", NULL, value = "Pipeline_Profile",
                      placeholder = "filename")
          ),
          column(2,
            selectInput("export_format", NULL,
                        choices = c("SVG" = "svg", "PDF" = "pdf", "PNG" = "png", "TIFF" = "tiff"),
                        selected = "svg")
          ),
          column(2,
            fluidRow(
              column(6, numericInput("export_width", "W (in)", value = 10, min = 4, max = 20, step = 0.5)),
              column(6, numericInput("export_height", "H (in)", value = 6, min = 3, max = 15, step = 0.5))
            )
          ),
          column(2,
            numericInput("export_dpi", "DPI", value = 300, min = 72, max = 1200, step = 50)
          ),
          column(2,
            downloadButton("download_plot", "Export", class = "btn-export",
                           style = "margin-top:25px;")
          )
        ),
        hr(),
        # Chart output
        plotOutput("main_plot", height = "600px", width = "100%")
      )
    )
  )
)


# ═══════════════════════════════════════════════════════════
#  SERVER
# ═══════════════════════════════════════════════════════════
server <- function(input, output, session) {

  # ── Reactive values ─────────────────────────────────────
  rv <- reactiveValues(
    raw_data = NULL,        # Raw imported data (list of data.frames per sheet)
    sheet_names = NULL,     # Sheet names
    series_data = list(),   # List of series: list(label, color, x, y, visible, shape)
    ref_lines = list(),     # Reference lines
    annotations = list(),   # Annotations
    is_flow_regime = FALSE  # Whether flow regime column detected
  )

  # ── File upload handling ────────────────────────────────
  observeEvent(input$file_input, {
    req(input$file_input)
    file <- input$file_input
    ext <- tolower(tools::file_ext(file$name))

    tryCatch({
      if (ext == "csv") {
        df <- read.csv(file$datapath, stringsAsFactors = FALSE, check.names = FALSE)
        rv$raw_data <- list(Sheet1 = df)
        rv$sheet_names <- "Sheet1"
      } else {
        sheets <- excel_sheets(file$datapath)
        rv$sheet_names <- sheets
        rv$raw_data <- setNames(
          lapply(sheets, function(s) {
            read_excel(file$datapath, sheet = s, col_names = TRUE, .name_repair = "minimal")
          }),
          sheets
        )
      }
      showNotification(
        paste0("\u2713 \"", file$name, "\" \u2014 ", length(rv$sheet_names), " sheet(s)"),
        type = "message", duration = 4
      )
    }, error = function(e) {
      showNotification(paste0("\u2717 ", e$message), type = "error")
    })
  })

  # ── Sheet selector UI ──────────────────────────────────
  output$sheet_selector <- renderUI({
    req(rv$sheet_names)
    if (length(rv$sheet_names) > 1) {
      selectInput("sheet_sel", "Sheet", choices = rv$sheet_names)
    }
  })

  # Get current sheet data
  current_sheet <- reactive({
    req(rv$raw_data)
    sheet_name <- if (!is.null(input$sheet_sel)) input$sheet_sel else rv$sheet_names[1]
    rv$raw_data[[sheet_name]]
  })

  # ── Column selector UI ─────────────────────────────────
  output$column_selectors <- renderUI({
    df <- current_sheet()
    req(df)
    cols <- colnames(df)

    # Auto-detect X column (look for "length")
    x_default <- grep("length", cols, ignore.case = TRUE, value = TRUE)
    x_default <- if (length(x_default) > 0) x_default[1] else cols[1]

    # Y columns: everything that's not an X/length column and is numeric
    numeric_cols <- cols[sapply(df, function(c) is.numeric(c) || all(grepl("^[0-9.eE+-]+$", na.omit(c))))]
    y_default <- setdiff(numeric_cols, x_default)

    tagList(
      selectInput("col_x", "X column (shared axis)", choices = cols, selected = x_default),
      selectizeInput("col_y", "Y series", choices = cols,
                     selected = if (length(y_default) > 0) y_default else cols[2],
                     multiple = TRUE,
                     options = list(plugins = list("remove_button")))
    )
  })

  # ── Load data into chart ────────────────────────────────
  observeEvent(input$load_btn, {
    df <- current_sheet()
    req(df, input$col_x, input$col_y)

    series_list <- list()
    x_col <- input$col_x
    y_cols <- input$col_y

    # Detect flow regime
    has_regime <- any(grepl("regime|flow.?regime", y_cols, ignore.case = TRUE))
    rv$is_flow_regime <- has_regime

    for (i in seq_along(y_cols)) {
      y_col <- y_cols[i]
      x_vals <- suppressWarnings(as.numeric(df[[x_col]]))
      y_vals <- suppressWarnings(as.numeric(df[[y_col]]))

      # Remove NAs
      valid <- !is.na(x_vals) & !is.na(y_vals)
      x_vals <- x_vals[valid]
      y_vals <- y_vals[valid]

      if (length(x_vals) == 0) next

      # Extract label (clean pipeline case names)
      label <- y_col
      case_match <- regmatches(label, regexpr('"([^"]+)\\.ppl"', label, perl = TRUE))
      if (length(case_match) > 0 && nchar(case_match) > 0) {
        label <- gsub('^"|"$', "", gsub("\\.ppl", "", case_match))
      }

      is_regime <- grepl("regime|flow.?regime", y_col, ignore.case = TRUE)

      series_list[[length(series_list) + 1]] <- list(
        label = label,
        color = SERIES_COLORS[((i - 1) %% length(SERIES_COLORS)) + 1],
        shape = MARKER_SHAPES[((i - 1) %% length(MARKER_SHAPES)) + 1],
        x = x_vals,
        y = y_vals,
        visible = TRUE,
        is_regime = is_regime
      )
    }

    rv$series_data <- series_list

    # Auto-set labels from sheet name
    sheet_name <- if (!is.null(input$sheet_sel)) input$sheet_sel else rv$sheet_names[1]
    updateTextInput(session, "chart_title", value = sheet_name)
    updateTextInput(session, "export_filename",
                    value = gsub("[\\/:*?\"<>|\\s]+", "_", sheet_name))

    # Auto-set Y label from first Y column
    if (length(y_cols) > 0) {
      y_label <- gsub('"[^"]*"', "", y_cols[1])
      y_label <- trimws(gsub("\\(PIPELINE\\)", "", y_label))
      if (nchar(y_label) > 0) {
        updateTextInput(session, "ylabel", value = y_label)
      }
    }

    showNotification(paste0("\u2713 Loaded ", length(series_list), " series"),
                     type = "message", duration = 3)
  })

  # ── Series toggle UI ───────────────────────────────────
  output$series_toggles <- renderUI({
    series <- rv$series_data
    if (length(series) == 0) return(tags$p("No series loaded", style = "font-size:0.75rem;color:#7a7060;font-style:italic;"))

    tagList(
      lapply(seq_along(series), function(i) {
        s <- series[[i]]
        div(style = paste0(
          "display:flex;align-items:center;gap:7px;padding:5px 7px;margin-bottom:4px;",
          "background:", if (s$visible) "#f7f4ef" else "#f7f4ef",
          ";border:1px solid #c8bfb0;border-radius:2px;",
          if (!s$visible) "opacity:0.4;" else ""
        ),
          span(style = paste0("width:22px;height:3px;border-radius:2px;background:", s$color, ";display:inline-block;")),
          span(s$label, style = "flex:1;font-size:0.72rem;overflow:hidden;text-overflow:ellipsis;white-space:nowrap;max-width:140px;"),
          actionButton(paste0("toggle_", i), if (s$visible) "ON" else "OFF",
                       class = "btn-ghost",
                       style = paste0("padding:2px 6px;font-size:0.62rem;min-width:32px;",
                                      if (s$visible) "background:#1a1714;color:#f7f4ef;border-color:#1a1714;" else ""))
        )
      })
    )
  })

  # ── Series toggle observers ─────────────────────────────
  observe({
    series <- rv$series_data
    lapply(seq_along(series), function(i) {
      observeEvent(input[[paste0("toggle_", i)]], {
        rv$series_data[[i]]$visible <- !rv$series_data[[i]]$visible
      }, ignoreInit = TRUE)
    })
  })

  # Show/Hide all
  observeEvent(input$show_all, {
    for (i in seq_along(rv$series_data)) rv$series_data[[i]]$visible <- TRUE
  })
  observeEvent(input$hide_all, {
    for (i in seq_along(rv$series_data)) rv$series_data[[i]]$visible <- FALSE
  })

  # ── Reset axis ranges ──────────────────────────────────
  observeEvent(input$reset_ranges, {
    updateNumericInput(session, "xmin", value = NA)
    updateNumericInput(session, "xmax", value = NA)
    updateNumericInput(session, "ymin", value = NA)
    updateNumericInput(session, "ymax", value = NA)
  })

  # ── Add reference line ──────────────────────────────────
  observeEvent(input$add_ref, {
    req(input$ref_value)
    rv$ref_lines <- c(rv$ref_lines, list(list(
      axis  = input$ref_axis,
      value = input$ref_value,
      label = input$ref_label,
      color = SERIES_COLORS[((length(rv$ref_lines)) %% length(SERIES_COLORS)) + 1],
      side  = input$ref_side,
      pos   = input$ref_pos
    )))
    updateNumericInput(session, "ref_value", value = NA)
    updateTextInput(session, "ref_label", value = "")
  })

  # ── Reference lines list UI ─────────────────────────────
  output$ref_lines_list <- renderUI({
    refs <- rv$ref_lines
    if (length(refs) == 0) return(NULL)
    tagList(
      lapply(seq_along(refs), function(i) {
        r <- refs[[i]]
        div(class = "ref-line-item",
          style = "display:flex;align-items:center;gap:7px;",
          span(style = paste0("width:22px;height:3px;border-radius:2px;background:", r$color,
                              ";display:inline-block;")),
          span(paste0(if (r$axis == "x") "X=" else "Y=", r$value,
                      if (nchar(r$label) > 0) paste0(" (", r$label, ")") else ""),
               style = "flex:1;color:#7a7060;"),
          actionButton(paste0("rm_ref_", i), "\u00d7",
                       class = "btn-ghost",
                       style = "padding:2px 6px;font-size:0.72rem;min-width:24px;")
        )
      })
    )
  })

  # Reference line remove observers
  observe({
    refs <- rv$ref_lines
    lapply(seq_along(refs), function(i) {
      observeEvent(input[[paste0("rm_ref_", i)]], {
        rv$ref_lines <- rv$ref_lines[-i]
      }, ignoreInit = TRUE, once = TRUE)
    })
  })

  # ── Add annotation ──────────────────────────────────────
  observeEvent(input$add_ann, {
    req(input$ann_x, input$ann_y, input$ann_text)
    rv$annotations <- c(rv$annotations, list(list(
      x     = input$ann_x,
      y     = input$ann_y,
      text  = input$ann_text,
      color = input$ann_color
    )))
    updateNumericInput(session, "ann_x", value = NA)
    updateNumericInput(session, "ann_y", value = NA)
    updateTextInput(session, "ann_text", value = "")
  })

  # ── Annotations list UI ─────────────────────────────────
  output$annotations_list <- renderUI({
    anns <- rv$annotations
    if (length(anns) == 0) return(NULL)
    tagList(
      lapply(seq_along(anns), function(i) {
        a <- anns[[i]]
        div(class = "ref-line-item",
          style = "display:flex;align-items:center;gap:6px;",
          span(style = paste0("width:8px;height:8px;border-radius:50%;background:", a$color,
                              ";display:inline-block;")),
          span(paste0("(", a$x, ", ", a$y, ") ", a$text),
               style = "flex:1;color:#7a7060;font-size:0.68rem;"),
          actionButton(paste0("rm_ann_", i), "\u00d7",
                       class = "btn-ghost",
                       style = "padding:2px 6px;font-size:0.72rem;min-width:24px;")
        )
      })
    )
  })

  # Annotation remove observers
  observe({
    anns <- rv$annotations
    lapply(seq_along(anns), function(i) {
      observeEvent(input[[paste0("rm_ann_", i)]], {
        rv$annotations <- rv$annotations[-i]
      }, ignoreInit = TRUE, once = TRUE)
    })
  })

  # ═══════════════════════════════════════════════════════
  #  BUILD THE PLOT
  # ═══════════════════════════════════════════════════════
  build_plot <- reactive({
    series <- rv$series_data
    visible_series <- Filter(function(s) s$visible, series)

    # Base plot
    p <- ggplot() + theme_academic(base_size = 14)

    if (length(visible_series) == 0) {
      # Empty plot with message
      p <- p +
        annotate("text", x = 0.5, y = 0.5, label = "Import data to begin",
                 size = 6, color = "#7a7060", fontface = "italic") +
        xlim(0, 1) + ylim(0, 1) +
        theme(axis.title = element_blank(), axis.text = element_blank(),
              axis.ticks = element_blank(), axis.line = element_blank(),
              panel.border = element_blank())
      return(p)
    }

    # ── Build combined data frame ─────────────────────────
    plot_df <- do.call(rbind, lapply(seq_along(visible_series), function(i) {
      s <- visible_series[[i]]
      data.frame(
        x = s$x, y = s$y,
        series = s$label,
        color = s$color,
        shape_id = s$shape,
        stringsAsFactors = FALSE
      )
    }))

    # Color and shape mappings
    color_map <- setNames(
      sapply(visible_series, function(s) s$color),
      sapply(visible_series, function(s) s$label)
    )
    shape_map <- setNames(
      sapply(visible_series, function(s) s$shape),
      sapply(visible_series, function(s) s$label)
    )

    # ── Flow regime banding ───────────────────────────────
    if (rv$is_flow_regime) {
      regime_series <- Filter(function(s) s$is_regime, visible_series)
      if (length(regime_series) > 0) {
        rs <- regime_series[[1]]
        ord <- order(rs$x)
        rx <- rs$x[ord]
        ry <- rs$y[ord]
        for (j in seq_len(length(rx) - 1)) {
          code <- as.character(round(ry[j]))
          regime <- FLOW_REGIMES[[code]]
          if (!is.null(regime)) {
            p <- p + annotate("rect",
              xmin = rx[j], xmax = rx[j + 1],
              ymin = -Inf, ymax = Inf,
              fill = regime$fill, alpha = 0.08
            )
          }
        }
      }
    }

    # ── Reference lines ───────────────────────────────────
    for (ref in rv$ref_lines) {
      if (ref$axis == "x") {
        p <- p + geom_vline(xintercept = ref$value, linetype = "dashed",
                            color = ref$color, linewidth = 0.7)
      } else {
        p <- p + geom_hline(yintercept = ref$value, linetype = "dashed",
                            color = ref$color, linewidth = 0.7)
      }
      # Label
      if (nchar(ref$label) > 0) {
        if (ref$axis == "x") {
          hjust_val <- if (ref$side == "right") -0.1 else 1.1
          p <- p + annotate("text", x = ref$value, y = Inf,
                            label = ref$label, color = ref$color,
                            hjust = hjust_val, vjust = if (ref$pos == "above") 1.5 else -0.5,
                            size = 3.5, family = "Helvetica")
        } else {
          hjust_val <- if (ref$side == "right") 1.05 else -0.05
          p <- p + annotate("text", x = if (ref$side == "right") Inf else -Inf,
                            y = ref$value,
                            label = ref$label, color = ref$color,
                            hjust = hjust_val, vjust = if (ref$pos == "above") -0.5 else 1.5,
                            size = 3.5, family = "Helvetica")
        }
      }
    }

    # ── Spline / lines ────────────────────────────────────
    show_spline <- (input$show_spline == "show")
    linetype_val <- if (input$spline_style == "dashed") "dashed" else "solid"

    if (show_spline && !rv$is_flow_regime) {
      # Smooth spline curves
      for (s in visible_series) {
        sdf <- data.frame(x = s$x, y = s$y)
        sdf <- sdf[order(sdf$x), ]
        if (nrow(sdf) >= 4) {
          p <- p + geom_smooth(
            data = sdf, aes(x = x, y = y),
            method = "loess", formula = y ~ x,
            se = FALSE, color = s$color,
            linetype = linetype_val,
            linewidth = 0.9, span = 0.3
          )
        } else if (nrow(sdf) >= 2) {
          p <- p + geom_line(
            data = sdf, aes(x = x, y = y),
            color = s$color, linetype = linetype_val,
            linewidth = 0.9
          )
        }
      }
    }

    if (rv$is_flow_regime) {
      # Step lines for flow regime
      for (s in visible_series) {
        sdf <- data.frame(x = s$x, y = s$y)
        sdf <- sdf[order(sdf$x), ]
        p <- p + geom_line(data = sdf, aes(x = x, y = y),
                           color = s$color, linewidth = 0.9)
      }
    }

    # ── Markers ───────────────────────────────────────────
    marker_mode <- input$marker_mode

    if (marker_mode != "none" && !rv$is_flow_regime) {
      for (s in visible_series) {
        sdf <- data.frame(x = s$x, y = s$y)
        sdf <- sdf[order(sdf$x), ]

        if (marker_mode == "nth") {
          nth <- input$marker_nth
          indices <- seq(1, nrow(sdf), by = nth)
          # Always include first and last
          indices <- sort(unique(c(1, indices, nrow(sdf))))
          sdf <- sdf[indices, ]
        }

        is_open <- s$shape %in% c(1, 0, 2, 5, 3, 6)
        p <- p + geom_point(
          data = sdf, aes(x = x, y = y),
          shape = s$shape, color = s$color,
          fill = if (is_open) "white" else s$color,
          size = 2.8, stroke = 0.8
        )
      }
    }

    # ── Annotations ───────────────────────────────────────
    for (ann in rv$annotations) {
      p <- p + annotate("point", x = ann$x, y = ann$y,
                        size = 4, color = ann$color)
      p <- p + annotate("label", x = ann$x, y = ann$y,
                        label = ann$text, color = ann$color,
                        fill = "white", label.size = 0.3,
                        size = 3.5, family = "Helvetica",
                        nudge_x = diff(range(plot_df$x, na.rm = TRUE)) * 0.03,
                        nudge_y = diff(range(plot_df$y, na.rm = TRUE)) * 0.05)
    }

    # ── Axis labels ───────────────────────────────────────
    x_label <- parse_label(input$xlabel)
    y_label <- parse_label(input$ylabel)
    title_label <- parse_label(input$chart_title)

    p <- p + labs(x = x_label, y = y_label, title = title_label)

    # ── Axis ranges ───────────────────────────────────────
    xlims <- c(
      if (!is.na(input$xmin)) input$xmin else NA,
      if (!is.na(input$xmax)) input$xmax else NA
    )
    ylims <- c(
      if (!is.na(input$ymin)) input$ymin else NA,
      if (!is.na(input$ymax)) input$ymax else NA
    )

    if (!all(is.na(xlims))) {
      p <- p + scale_x_continuous(limits = xlims)
    }
    if (!all(is.na(ylims))) {
      p <- p + scale_y_continuous(limits = ylims)
    }

    # ── Legend ─────────────────────────────────────────────
    # Build a manual legend when we have multiple series
    if (length(visible_series) > 1) {
      # We need to map series names to colors/shapes via a dummy aesthetic
      plot_df$series <- factor(plot_df$series, levels = sapply(visible_series, function(s) s$label))

      # Rebuild plot with aesthetics for legend
      p_new <- ggplot(plot_df, aes(x = x, y = y, color = series, shape = series)) +
        theme_academic(base_size = 14)

      # Re-add all layers (reference lines, flow regime, etc.)
      for (ref in rv$ref_lines) {
        if (ref$axis == "x") {
          p_new <- p_new + geom_vline(xintercept = ref$value, linetype = "dashed",
                                      color = ref$color, linewidth = 0.7)
        } else {
          p_new <- p_new + geom_hline(yintercept = ref$value, linetype = "dashed",
                                      color = ref$color, linewidth = 0.7)
        }
        if (nchar(ref$label) > 0) {
          if (ref$axis == "x") {
            hjust_val <- if (ref$side == "right") -0.1 else 1.1
            p_new <- p_new + annotate("text", x = ref$value, y = Inf,
                                      label = ref$label, color = ref$color,
                                      hjust = hjust_val, vjust = if (ref$pos == "above") 1.5 else -0.5,
                                      size = 3.5, family = "Helvetica")
          } else {
            hjust_val <- if (ref$side == "right") 1.05 else -0.05
            p_new <- p_new + annotate("text", x = if (ref$side == "right") Inf else -Inf,
                                      y = ref$value,
                                      label = ref$label, color = ref$color,
                                      hjust = hjust_val, vjust = if (ref$pos == "above") -0.5 else 1.5,
                                      size = 3.5, family = "Helvetica")
          }
        }
      }

      if (show_spline && !rv$is_flow_regime) {
        for (s in visible_series) {
          sdf <- data.frame(x = s$x, y = s$y, series = s$label)
          sdf <- sdf[order(sdf$x), ]
          if (nrow(sdf) >= 4) {
            p_new <- p_new + geom_smooth(
              data = sdf, aes(x = x, y = y, group = series),
              method = "loess", formula = y ~ x,
              se = FALSE, color = s$color,
              linetype = linetype_val,
              linewidth = 0.9, span = 0.3, show.legend = FALSE
            )
          } else if (nrow(sdf) >= 2) {
            p_new <- p_new + geom_line(
              data = sdf, aes(x = x, y = y, group = series),
              color = s$color, linetype = linetype_val,
              linewidth = 0.9, show.legend = FALSE
            )
          }
        }
      }

      if (rv$is_flow_regime) {
        for (s in visible_series) {
          sdf <- data.frame(x = s$x, y = s$y, series = s$label)
          sdf <- sdf[order(sdf$x), ]
          p_new <- p_new + geom_line(data = sdf, aes(x = x, y = y, group = series),
                                     color = s$color, linewidth = 0.9, show.legend = FALSE)
        }
      }

      if (marker_mode != "none" && !rv$is_flow_regime) {
        # Add points with legend
        for (s in visible_series) {
          sdf <- data.frame(x = s$x, y = s$y, series = s$label)
          sdf <- sdf[order(sdf$x), ]
          if (marker_mode == "nth") {
            nth <- input$marker_nth
            indices <- seq(1, nrow(sdf), by = nth)
            indices <- sort(unique(c(1, indices, nrow(sdf))))
            sdf <- sdf[indices, ]
          }
          is_open <- s$shape %in% c(1, 0, 2, 5, 3, 6)
          p_new <- p_new + geom_point(
            data = sdf, aes(x = x, y = y),
            shape = s$shape, color = s$color,
            fill = if (is_open) "white" else s$color,
            size = 2.8, stroke = 0.8, show.legend = FALSE
          )
        }
        # Invisible legend layer
        p_new <- p_new + geom_point(alpha = 0)
      } else if (marker_mode == "none") {
        p_new <- p_new + geom_point(alpha = 0, show.legend = TRUE)
      }

      # Annotations
      for (ann in rv$annotations) {
        p_new <- p_new + annotate("point", x = ann$x, y = ann$y,
                                  size = 4, color = ann$color)
        p_new <- p_new + annotate("label", x = ann$x, y = ann$y,
                                  label = ann$text, color = ann$color,
                                  fill = "white", label.size = 0.3,
                                  size = 3.5, family = "Helvetica",
                                  nudge_x = diff(range(plot_df$x, na.rm = TRUE)) * 0.03,
                                  nudge_y = diff(range(plot_df$y, na.rm = TRUE)) * 0.05)
      }

      # Scale overrides for legend
      fill_map <- setNames(
        sapply(visible_series, function(s) {
          if (s$shape %in% c(1, 0, 2, 5, 3, 6)) "white" else s$color
        }),
        sapply(visible_series, function(s) s$label)
      )

      p_new <- p_new +
        scale_color_manual(values = color_map) +
        scale_shape_manual(values = shape_map) +
        scale_fill_manual(values = fill_map) +
        guides(
          color = guide_legend(override.aes = list(
            shape = unname(shape_map),
            size = 3,
            linetype = 0
          )),
          shape = "none", fill = "none"
        )

      p_new <- p_new + labs(x = x_label, y = y_label, title = title_label)

      if (!all(is.na(xlims))) p_new <- p_new + scale_x_continuous(limits = xlims)
      if (!all(is.na(ylims))) p_new <- p_new + scale_y_continuous(limits = ylims)

      p <- p_new
    }

    # Legend position
    leg_pos <- input$legend_pos
    if (leg_pos == "hidden") {
      p <- p + theme(legend.position = "none")
    } else {
      pos <- legend_pos_map(leg_pos)
      just <- legend_just_map(leg_pos)
      p <- p + theme(
        legend.position = pos,
        legend.justification = just,
        legend.position.inside = pos
      )
    }

    p
  })

  # ── Render plot ─────────────────────────────────────────
  output$main_plot <- renderPlot({
    build_plot()
  }, res = 96)

  # ── Export download ─────────────────────────────────────
  output$download_plot <- downloadHandler(
    filename = function() {
      paste0(input$export_filename, ".", input$export_format)
    },
    content = function(file) {
      p <- build_plot()
      w <- input$export_width
      h <- input$export_height
      dpi <- input$export_dpi

      ggsave(file, plot = p, width = w, height = h, dpi = dpi,
             device = input$export_format, bg = "white")
    }
  )
}


# ═══════════════════════════════════════════════════════════
#  LAUNCH
# ═══════════════════════════════════════════════════════════
shinyApp(ui = ui, server = server)
