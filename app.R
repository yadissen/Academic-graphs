# ============================================================================
# Interactive Pipeline Profile Plotter — Shiny App
# ============================================================================
# A browser-based graph editor powered by ggplot2.
# Run with:   Rscript app.R
#         or: R -e "shiny::runApp('app.R', launch.browser = TRUE)"
# ============================================================================

library(shiny)
library(ggplot2)
library(readxl)
library(dplyr)
library(tidyr)

# Source core plotting functions
source("R/theme_academic.R")
source("R/pipeline_plotter.R")

# ── UI ───────────────────────────────────────────────────────────────────────

ui <- fluidPage(

  # ── Custom CSS for academic look ──
  tags$head(tags$style(HTML("
    body {
      background: #f7f4ef;
      font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;
      color: #1a1714;
    }
    .well {
      background: #fdfcfa;
      border: 1px solid #c8bfb0;
      border-radius: 4px;
    }
    h2 { font-size: 1.6rem; font-weight: 600; letter-spacing: -0.01em; }
    h4 { font-size: 0.85rem; text-transform: uppercase; letter-spacing: 0.1em;
         color: #7a7060; border-bottom: 1px solid #e4ddd4; padding-bottom: 6px; }
    .btn-primary {
      background: #2a4f6e; border-color: #2a4f6e;
    }
    .btn-primary:hover {
      background: #1e3a5a; border-color: #1e3a5a;
    }
    .btn-success {
      background: #2e6b45; border-color: #2e6b45;
    }
    .shiny-plot-output {
      background: white;
      border: 1px solid #c8bfb0;
      border-radius: 4px;
    }
    .sidebar-section {
      margin-bottom: 16px;
      padding: 12px;
      background: #fdfcfa;
      border: 1px solid #e4ddd4;
      border-radius: 4px;
    }
    .nav-tabs > li > a { font-size: 0.82rem; }
    #download_section .btn { margin: 3px 2px; }
  "))),

  titlePanel(
    div(
      h2("Pipeline Profile Plotter"),
      p("Interactive academic graph editor — powered by R / ggplot2",
        style = "font-style: italic; color: #7a7060; font-size: 0.9rem; margin-top: 2px;")
    )
  ),

  sidebarLayout(

    # ── SIDEBAR ──
    sidebarPanel(
      width = 3,

      tabsetPanel(
        id = "sidebar_tabs", type = "pills",

        # ── Tab 1: Import ──
        tabPanel("Import",
          h4("Data Import"),
          fileInput("file_input", "Upload Excel or CSV",
                    accept = c(".xlsx", ".xls", ".csv")),
          uiOutput("sheet_selector"),
          textInput("x_col_pattern", "X-column keyword", value = "length"),
          uiOutput("series_checkboxes"),
          hr(),
          actionButton("load_demo", "Load Demo Data", class = "btn-sm btn-success",
                       icon = icon("flask"))
        ),

        # ── Tab 2: Labels ──
        tabPanel("Labels",
          h4("Chart Labels"),
          textInput("chart_title", "Chart Title", value = ""),
          textInput("x_label", "X-axis Label", value = "Pipeline Length [m]"),
          textInput("y_label", "Y-axis Label", value = "Pressure [bara]"),
          helpText("Use _{x} for subscript and ^{x} for superscript in exported titles.")
        ),

        # ── Tab 3: Style ──
        tabPanel("Style",
          h4("Lines & Markers"),
          checkboxInput("smooth", "Smooth Spline (LOESS)", value = TRUE),
          conditionalPanel(
            "input.smooth == true",
            sliderInput("loess_span", "Smoothing Amount", min = 0.1, max = 1.0,
                        value = 0.3, step = 0.05)
          ),
          sliderInput("line_width", "Line Width", min = 0.3, max = 2.5,
                      value = 0.8, step = 0.1),
          hr(),
          checkboxInput("show_points", "Show Markers", value = TRUE),
          conditionalPanel(
            "input.show_points == true",
            radioButtons("marker_mode", NULL,
                         choices = c("All" = "all", "Every Nth" = "nth", "First & Last" = "fl"),
                         selected = "all", inline = TRUE),
            conditionalPanel(
              "input.marker_mode == 'nth'",
              sliderInput("marker_nth", "Show Every N", min = 2, max = 50,
                          value = 5, step = 1)
            ),
            sliderInput("point_size", "Marker Size", min = 1, max = 6,
                        value = 2.5, step = 0.5)
          ),

          h4("Grid & Theme"),
          radioButtons("grid_style", "Grid Lines",
                       choices = c("None" = "none", "Major" = "major", "Both" = "both"),
                       selected = "none", inline = TRUE),
          sliderInput("base_size", "Font Size", min = 8, max = 18,
                      value = 11, step = 1)
        ),

        # ── Tab 4: Reference Lines ──
        tabPanel("Ref Lines",
          h4("Reference Lines"),
          selectInput("ref_axis", "Axis", choices = c("Horizontal (Y)" = "y", "Vertical (X)" = "x")),
          numericInput("ref_value", "Value", value = NA),
          textInput("ref_label", "Label (optional)", value = ""),
          textInput("ref_color", "Color", value = "#8b1a4a"),
          actionButton("add_ref", "Add Reference Line", class = "btn-sm btn-primary",
                       icon = icon("plus")),
          hr(),
          uiOutput("ref_lines_list")
        ),

        # ── Tab 5: Annotations ──
        tabPanel("Annotate",
          h4("Annotations"),
          numericInput("ann_x", "X coordinate", value = NA),
          numericInput("ann_y", "Y coordinate", value = NA),
          textInput("ann_label", "Label", value = ""),
          textInput("ann_color", "Color", value = "#2a4f6e"),
          radioButtons("ann_dir", "Leader Direction",
                       choices = c("Up-Right" = "ur", "Up-Left" = "ul",
                                   "Down-Right" = "lr", "Down-Left" = "ll"),
                       selected = "ur", inline = TRUE),
          actionButton("add_ann", "Add Annotation", class = "btn-sm btn-primary",
                       icon = icon("plus")),
          hr(),
          uiOutput("annotations_list")
        ),

        # ── Tab 6: Axes & Legend ──
        tabPanel("Axes",
          h4("Axis Ranges"),
          fluidRow(
            column(6, numericInput("x_min", "X Min", value = NA)),
            column(6, numericInput("x_max", "X Max", value = NA))
          ),
          fluidRow(
            column(6, numericInput("y_min", "Y Min", value = NA)),
            column(6, numericInput("y_max", "Y Max", value = NA))
          ),
          actionButton("reset_axes", "Reset to Auto", class = "btn-sm"),

          h4("Legend"),
          selectInput("legend_pos", "Position", choices = c(
            "Bottom" = "bottom", "Right" = "right", "Top" = "top",
            "Top-Left" = "tl", "Top-Right" = "tr",
            "Bottom-Left" = "bl", "Bottom-Right" = "br",
            "Hidden" = "none"
          ), selected = "bottom")
        ),

        # ── Tab 7: Export ──
        tabPanel("Export",
          h4("Download"),
          textInput("export_filename", "Filename", value = "pipeline_plot"),
          fluidRow(
            column(6, numericInput("export_width", "Width (in)", value = 8, min = 3, max = 20)),
            column(6, numericInput("export_height", "Height (in)", value = 5, min = 2, max = 15))
          ),
          numericInput("export_dpi", "DPI (raster)", value = 300, min = 72, max = 600),
          div(id = "download_section",
            downloadButton("dl_pdf", "PDF", class = "btn-primary"),
            downloadButton("dl_svg", "SVG", class = "btn-primary"),
            downloadButton("dl_png", "PNG", class = "btn-primary")
          )
        )
      )
    ),

    # ── MAIN PANEL ──
    mainPanel(
      width = 9,
      plotOutput("main_plot", height = "600px", click = "plot_click"),
      fluidRow(
        column(6, verbatimTextOutput("click_info")),
        column(6, div(style = "text-align: right; padding: 10px;",
          actionButton("refresh_plot", "Refresh Plot", class = "btn-sm btn-primary",
                       icon = icon("sync"))
        ))
      )
    )
  )
)


# ── SERVER ───────────────────────────────────────────────────────────────────

server <- function(input, output, session) {

  # Reactive values
  rv <- reactiveValues(
    data_long = NULL,
    data_wide = NULL,
    x_name = NULL,
    series_names = character(0),
    selected_series = character(0),
    ref_lines = data.frame(axis = character(), value = numeric(),
                           label = character(), color = character(),
                           stringsAsFactors = FALSE),
    annotations = data.frame(x = numeric(), y = numeric(),
                             label = character(), color = character(),
                             direction = character(),
                             stringsAsFactors = FALSE),
    sheets = NULL
  )


  # ── File upload ──
  observeEvent(input$file_input, {
    req(input$file_input)
    fp <- input$file_input$datapath
    ext <- tolower(tools::file_ext(input$file_input$name))

    if (ext %in% c("xlsx", "xls")) {
      rv$sheets <- readxl::excel_sheets(fp)
      if (length(rv$sheets) > 1) {
        output$sheet_selector <- renderUI({
          selectInput("sheet_choice", "Select Sheet", choices = rv$sheets)
        })
      } else {
        output$sheet_selector <- renderUI(NULL)
      }
    }

    load_file_data(fp, ext)
  })

  load_file_data <- function(fp, ext = NULL) {
    if (is.null(ext)) ext <- tolower(tools::file_ext(fp))
    sheet <- if (!is.null(input$sheet_choice)) input$sheet_choice else 1

    tryCatch({
      result <- read_pipeline_data(fp, sheet = sheet,
                                   x_col = input$x_col_pattern)
      rv$data_long <- result$long
      rv$data_wide <- result$wide
      rv$x_name <- result$x_name
      rv$series_names <- result$series_names
      rv$selected_series <- result$series_names

      output$series_checkboxes <- renderUI({
        checkboxGroupInput("visible_series", "Series",
                           choices = result$series_names,
                           selected = result$series_names)
      })
    }, error = function(e) {
      showNotification(paste("Error reading file:", e$message), type = "error")
    })
  }

  observeEvent(input$sheet_choice, {
    req(input$file_input)
    load_file_data(input$file_input$datapath)
  })

  observeEvent(input$visible_series, {
    rv$selected_series <- input$visible_series
  }, ignoreNULL = FALSE)


  # ── Demo data ──
  observeEvent(input$load_demo, {
    set.seed(42)
    demo <- data.frame(
      x = rep(seq(0, 1000, by = 25), 3),
      series = rep(c("4-inch Pipeline", "6-inch Pipeline", "8-inch Pipeline"),
                   each = 41),
      value = c(
        80 - seq(0, 1000, by = 25) * 0.045 + rnorm(41, 0, 1.2),
        80 - seq(0, 1000, by = 25) * 0.025 + rnorm(41, 0, 0.9),
        80 - seq(0, 1000, by = 25) * 0.012 + rnorm(41, 0, 0.6)
      )
    )
    demo$series <- factor(demo$series,
                          levels = c("4-inch Pipeline", "6-inch Pipeline", "8-inch Pipeline"))

    rv$data_long <- demo
    rv$series_names <- levels(demo$series)
    rv$selected_series <- levels(demo$series)
    rv$x_name <- "x"

    output$series_checkboxes <- renderUI({
      checkboxGroupInput("visible_series", "Series",
                         choices = levels(demo$series),
                         selected = levels(demo$series))
    })

    updateTextInput(session, "chart_title", value = "Pressure Drop Comparison by Pipe Diameter")
    updateTextInput(session, "x_label", value = "Pipeline Length [m]")
    updateTextInput(session, "y_label", value = "Pressure [bara]")

    showNotification("Demo data loaded!", type = "message", duration = 3)
  })


  # ── Reference lines ──
  observeEvent(input$add_ref, {
    req(input$ref_value)
    new_row <- data.frame(
      axis  = input$ref_axis,
      value = input$ref_value,
      label = input$ref_label,
      color = input$ref_color,
      stringsAsFactors = FALSE
    )
    rv$ref_lines <- rbind(rv$ref_lines, new_row)
    updateNumericInput(session, "ref_value", value = NA)
    updateTextInput(session, "ref_label", value = "")
  })

  output$ref_lines_list <- renderUI({
    if (nrow(rv$ref_lines) == 0) return(p("No reference lines added.", style = "color: #999;"))
    tags$ul(style = "list-style: none; padding-left: 0;",
      lapply(seq_len(nrow(rv$ref_lines)), function(i) {
        rl <- rv$ref_lines[i, ]
        axis_label <- if (rl$axis == "y") "H" else "V"
        tags$li(style = "margin-bottom: 4px; font-size: 0.85rem;",
          span(style = paste0("color:", rl$color, "; font-weight: bold;"),
               paste0("[", axis_label, "] ")),
          paste0(rl$value, if (nchar(rl$label) > 0) paste0(" — ", rl$label) else ""),
          actionButton(paste0("rm_ref_", i), "x",
                       class = "btn-xs btn-danger",
                       style = "margin-left: 6px; padding: 0 5px; font-size: 0.7rem;")
        )
      })
    )
  })

  observe({
    lapply(seq_len(nrow(rv$ref_lines)), function(i) {
      observeEvent(input[[paste0("rm_ref_", i)]], {
        rv$ref_lines <- rv$ref_lines[-i, , drop = FALSE]
      }, ignoreInit = TRUE, once = TRUE)
    })
  })


  # ── Annotations ──
  observeEvent(input$add_ann, {
    req(input$ann_x, input$ann_y, input$ann_label)
    new_ann <- data.frame(
      x = input$ann_x, y = input$ann_y,
      label = input$ann_label, color = input$ann_color,
      direction = input$ann_dir,
      stringsAsFactors = FALSE
    )
    rv$annotations <- rbind(rv$annotations, new_ann)
    updateTextInput(session, "ann_label", value = "")
  })

  output$annotations_list <- renderUI({
    if (nrow(rv$annotations) == 0) return(p("No annotations added.", style = "color: #999;"))
    tags$ul(style = "list-style: none; padding-left: 0;",
      lapply(seq_len(nrow(rv$annotations)), function(i) {
        ann <- rv$annotations[i, ]
        tags$li(style = "margin-bottom: 4px; font-size: 0.85rem;",
          span(style = paste0("color:", ann$color, "; font-weight: bold;"), "\u25cf "),
          paste0('"', ann$label, '" at (', ann$x, ', ', ann$y, ')'),
          actionButton(paste0("rm_ann_", i), "x",
                       class = "btn-xs btn-danger",
                       style = "margin-left: 6px; padding: 0 5px; font-size: 0.7rem;")
        )
      })
    )
  })

  observe({
    lapply(seq_len(nrow(rv$annotations)), function(i) {
      observeEvent(input[[paste0("rm_ann_", i)]], {
        rv$annotations <- rv$annotations[-i, , drop = FALSE]
      }, ignoreInit = TRUE, once = TRUE)
    })
  })


  # ── Reset axes ──
  observeEvent(input$reset_axes, {
    updateNumericInput(session, "x_min", value = NA)
    updateNumericInput(session, "x_max", value = NA)
    updateNumericInput(session, "y_min", value = NA)
    updateNumericInput(session, "y_max", value = NA)
  })


  # ── Click info ──
  output$click_info <- renderText({
    req(input$plot_click)
    paste0("Clicked: x = ", round(input$plot_click$x, 2),
           ", y = ", round(input$plot_click$y, 2))
  })


  # ── Legend position helper ──
  get_legend_pos <- reactive({
    lp <- input$legend_pos
    switch(lp,
      "tl" = c(0.02, 0.98),
      "tr" = c(0.98, 0.98),
      "bl" = c(0.02, 0.02),
      "br" = c(0.98, 0.02),
      lp  # "bottom", "right", "top", "none" pass through
    )
  })


  # ── Build the plot ──
  build_plot <- reactive({
    req(rv$data_long)

    df <- rv$data_long
    if (length(rv$selected_series) > 0) {
      df <- df %>% filter(series %in% rv$selected_series)
    }
    if (nrow(df) == 0) return(ggplot() + theme_void() + ggtitle("No data to display"))

    # Marker nth
    mnth <- NULL
    if (input$show_points && input$marker_mode == "nth") {
      mnth <- input$marker_nth
    }

    # Axis limits
    xlim <- NULL; ylim <- NULL
    if (!is.na(input$x_min) || !is.na(input$x_max)) {
      xlim <- c(
        if (!is.na(input$x_min)) input$x_min else NA,
        if (!is.na(input$x_max)) input$x_max else NA
      )
    }
    if (!is.na(input$y_min) || !is.na(input$y_max)) {
      ylim <- c(
        if (!is.na(input$y_min)) input$y_min else NA,
        if (!is.na(input$y_max)) input$y_max else NA
      )
    }

    # Reference lines
    refs <- if (nrow(rv$ref_lines) > 0) rv$ref_lines else NULL
    anns <- if (nrow(rv$annotations) > 0) rv$annotations else NULL

    # Build plot
    n_series <- length(unique(df$series))
    colors <- unname(academic_colors)[seq_len(n_series)]
    shapes <- academic_shapes[seq_len(n_series)]

    # Filter markers
    if (input$show_points) {
      if (input$marker_mode == "nth" && !is.null(mnth) && mnth > 1) {
        df_points <- df %>%
          group_by(series) %>%
          mutate(row_n = row_number()) %>%
          filter(row_n == 1 | row_n %% mnth == 0 | row_n == n()) %>%
          ungroup() %>%
          select(-row_n)
      } else if (input$marker_mode == "fl") {
        df_points <- df %>%
          group_by(series) %>%
          filter(row_number() == 1 | row_number() == n()) %>%
          ungroup()
      } else {
        df_points <- df
      }
    }

    legend_pos <- get_legend_pos()
    legend_justification <- if (is.numeric(legend_pos)) legend_pos else NULL

    p <- ggplot(df, aes(x = x, y = value, color = series, shape = series,
                         linetype = series)) +
      theme_academic(base_size = input$base_size, grid = input$grid_style,
                     legend_pos = legend_pos)

    # Add legend justification for inside-plot positions
    if (is.numeric(legend_pos)) {
      p <- p + theme(legend.justification = legend_pos)
    }

    # Reference lines
    if (!is.null(refs)) {
      for (i in seq_len(nrow(refs))) {
        rl <- refs[i, ]
        if (rl$axis == "y") {
          p <- p + geom_hline(yintercept = rl$value, linetype = "dashed",
                              color = rl$color, linewidth = 0.5)
        } else {
          p <- p + geom_vline(xintercept = rl$value, linetype = "dashed",
                              color = rl$color, linewidth = 0.5)
        }
        if (nchar(rl$label) > 0) {
          if (rl$axis == "y") {
            p <- p + annotate("text", x = -Inf, y = rl$value, label = rl$label,
                              hjust = -0.1, vjust = -0.5, size = 3.2,
                              color = rl$color, fontface = "italic")
          } else {
            p <- p + annotate("text", x = rl$value, y = Inf, label = rl$label,
                              hjust = -0.1, vjust = 1.5, size = 3.2,
                              color = rl$color, fontface = "italic", angle = 90)
          }
        }
      }
    }

    # Lines
    if (input$smooth) {
      p <- p + geom_smooth(method = "loess", se = FALSE, linewidth = input$line_width,
                           span = input$loess_span, formula = y ~ x)
    } else {
      p <- p + geom_line(linewidth = input$line_width)
    }

    # Markers
    if (input$show_points) {
      p <- p + geom_point(data = df_points, size = input$point_size, stroke = 0.4)
    }

    # Scales
    p <- p +
      scale_color_manual(values = colors) +
      scale_shape_manual(values = shapes) +
      scale_linetype_manual(values = rep(1, n_series))

    # Axis limits
    if (!is.null(xlim)) p <- p + scale_x_continuous(limits = xlim)
    if (!is.null(ylim)) p <- p + scale_y_continuous(limits = ylim)

    # Labels
    p <- p + labs(
      title = if (nchar(input$chart_title) > 0) input$chart_title else NULL,
      x = input$x_label,
      y = input$y_label,
      color = NULL, shape = NULL, linetype = NULL
    )

    # Annotations
    if (!is.null(anns)) {
      x_range <- diff(range(df$x, na.rm = TRUE))
      y_range <- diff(range(df$value, na.rm = TRUE))
      for (i in seq_len(nrow(anns))) {
        ann <- anns[i, ]
        dir <- ann$direction
        dx <- x_range * 0.06 * ifelse(grepl("l$", dir), -1, 1)
        dy <- y_range * 0.08 * ifelse(startsWith(dir, "u"), 1, -1)

        p <- p +
          annotate("segment",
                   x = ann$x, y = ann$y,
                   xend = ann$x + dx, yend = ann$y + dy,
                   color = ann$color, linewidth = 0.4,
                   arrow = arrow(length = unit(0.15, "cm"), type = "closed")) +
          annotate("label",
                   x = ann$x + dx, y = ann$y + dy,
                   label = ann$label, color = ann$color,
                   size = 3.2, fill = "white", label.size = 0.3,
                   fontface = "italic")
      }
    }

    p
  })


  # ── Render ──
  output$main_plot <- renderPlot({
    build_plot()
  }, res = 96)


  # ── Downloads ──
  output$dl_pdf <- downloadHandler(
    filename = function() paste0(input$export_filename, ".pdf"),
    content = function(file) {
      ggsave(file, plot = build_plot(),
             width = input$export_width, height = input$export_height,
             device = "pdf")
    }
  )

  output$dl_svg <- downloadHandler(
    filename = function() paste0(input$export_filename, ".svg"),
    content = function(file) {
      if (requireNamespace("svglite", quietly = TRUE)) {
        ggsave(file, plot = build_plot(),
               width = input$export_width, height = input$export_height,
               device = svglite::svglite)
      } else {
        ggsave(file, plot = build_plot(),
               width = input$export_width, height = input$export_height,
               device = "svg")
      }
    }
  )

  output$dl_png <- downloadHandler(
    filename = function() paste0(input$export_filename, ".png"),
    content = function(file) {
      if (requireNamespace("ragg", quietly = TRUE)) {
        ggsave(file, plot = build_plot(),
               width = input$export_width, height = input$export_height,
               dpi = input$export_dpi, device = ragg::agg_png)
      } else {
        ggsave(file, plot = build_plot(),
               width = input$export_width, height = input$export_height,
               dpi = input$export_dpi, device = "png")
      }
    }
  )
}


# ── Launch ───────────────────────────────────────────────────────────────────

cat("\n========================================\n")
cat("  Pipeline Profile Plotter\n")
cat("  Opening in your browser...\n")
cat("========================================\n\n")

shiny::runApp(shinyApp(ui, server), launch.browser = TRUE)
