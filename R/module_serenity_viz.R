#' UI for Serenity Viz module
#'
#' @param id Serenity Viz module ID
#' @param dataset Passed in dataset for visualization
#' @param titlebar Show title bar with Done and Cancel buttons?
#' @param showcode Show code for plots?
#' @param height Specify height
#'
#' @return UI for Serenity Viz module
#'
#' @import shiny luminophor
#' @export
#'
serenityVizUI <- function(id, dataset, titlebar = FALSE, showcode = TRUE, height = NULL) {
  ns <- NS(id)

  addResourcePath("js", file.path(resourcePath, "js")) # Not sure why this is needed for shinyJS

  miniUI::miniPage(
    style = switch(!is.null(height), paste("height:", height), height),
    shinyjs::useShinyjs(),
    shinyjs::extendShinyjs(
      script = "js/shinyjs-funcs.js",
      functions = c("close_window", "toggleClass", "addClass", "removeClass", "myshow", "myhide")
    ),
    bsplus::use_bs_tooltip(),
    tags$head(includeCSS(file.path(resourcePath, "css", "app.css"))),
    tags$head(includeCSS(file.path(resourcePath, "css", "geom_icons.css"))),
    tags$head(includeCSS(file.path(resourcePath, "css", "misc_icons.css"))),
    tags$head(includeScript(file.path(resourcePath, "js", "serenity_viz.js"))),
    switch(titlebar,
           miniUI::gadgetTitleBar("Serenity Viz",
                                  left = miniUI::miniTitleBarCancelButton(ns("cancel")),
                                  right = miniUI::miniTitleBarButton(ns("done"), "Done", primary = TRUE)),
           NULL),
    luminophorOutput(ns("luminobox"), height="100%")
  )
}

#' Server code for Serenity Viz module
#'
#' @param input Shiny input
#' @param output Shiny output
#' @param session Shiny session
#' @param dataset Passed in dataset for visualization
#'
#' @importFrom magrittr %>%
#' @import shiny ggplot2 dplyr forcats luminophor
#' @export
#'
serenityVizServer <- function(input, output, session, dataset) {
  if (is.null(attr(dataset, "df_name"))) {
    attr(dataset, "df_name") <- deparse(substitute(dataset))
  }

  # Store log for warnings
  ggplot2_log <- reactiveVal("")

  output$luminobox <- renderLuminophor({
    luminophor() %>%
      addWidget(id = session$ns("widget-layers"),
                ui = layersUI(session$ns("layers")),
                title = "Layers",
                icon = icon("layer-group"),
                closable = FALSE) %>%
      addWidget(id = session$ns('widget-ggplot'),
                refwidgetID = session$ns('widget-layers'),
                insertmode = "split-left",
                relsize = 0.66,
                ui = tagList(
                  widgetHeader(
                    uiOutput(session$ns('widget-ggplot-header'))
                  ),
                  widgetBody(
                    miniUI::miniContentPanel(
                      class = "ggplot",
                      style = "padding: 19px;",
                      plotOutput(session$ns("viz"), height = "100%", click = "plot_click"),
                      shinyjs::hidden(
                        absolutePanel(id = session$ns("error-pane"),
                                      class = "error-pane",
                                      top = "20px",
                                      draggable = FALSE
                        )
                      )
                    )
                  )
                ),
                title = "Plot",
                icon = icon("image"),
                closable = FALSE) %>%
      addWidget(id = session$ns("widget-vars"),
                refwidgetID = session$ns("widget-ggplot"),
                insertmode = "split-left",
                relsize = 0.33,
                ui = dataUI(id = session$ns(attributes(dataset)$df_name)),
                title = "Variables",
                icon = icon("database"),
                closable = FALSE) %>%
      addWidget(id = session$ns("aesthetics"),
                refwidgetID = session$ns("widget-vars"),
                insertmode = "split-bottom",
                relsize = 0.66,
                ui = uiOutput(session$ns("aesthetics")),
                title = "Aesthetics",
                icon = icon("paint-brush"),
                closable = FALSE) %>%
      addWidget(id = session$ns("widget-computed-vars"),
                refwidgetID = session$ns("widget-vars"),
                insertmode = "split-right",
                ui = dataComputedUI(id = session$ns("computed-vars")),
                title = "Computed",
                icon = icon("calculator"),
                closable = FALSE) %>%
      addWidget(id = session$ns("widget-code"),
                refwidgetID = session$ns('widget-ggplot'),
                insertmode = "split-bottom",
                relsize = 0.25,
                ui = widgetBody(uiOutput(session$ns("code"),
                                         class="terminal-dark-theme")),
                title = "Code",
                icon = icon("code")) %>%
      addWidget(id = session$ns("widget-messages"),
                refwidgetID = session$ns("widget-code"),
                insertmode = "tab-after",
                ui = widgetBody(uiOutput(session$ns("log"),
                                         class="terminal-dark-theme")),
                title = "Messages",
                icon = icon("info")) %>%
      addWidget(id = session$ns("widget-labels"),
                refwidgetID = session$ns('widget-layers'),
                insertmode = "tab-after",
                ui = labelsUI(session$ns("labels")),
                title = "Labels",
                icon = icon("tags"),
                closable = FALSE)
  })

  output$`widget-ggplot-header` <- renderUI({
    tagList(
      prettyToggle(
        inputId = session$ns("maximize"),
        label_on = "",
        label_off = "",
        status_on = "default",
        status_off = "default",
        outline = TRUE,
        plain = TRUE,
        icon_on = icon("window-minimize"),
        icon_off = icon("window-maximize"),
        inline = TRUE
      )
    )
  })

  observeEvent(input$maximize, {
    if (input$maximize) {
      luminophor::luminophorProxy(session$ns('luminobox')) %>%
        luminophor::maximizeWidget(session$ns('widget-ggplot'))
    } else {
      luminophor::luminophorProxy(session$ns('luminobox')) %>%
        luminophor::minimizeWidget(session$ns('widget-ggplot'))
    }
  })

  # Data module
  subsetted_data <- callModule(module = dataServer,
                               id = attributes(dataset)$df_name,
                               dataset = dataset)

  # Layers module
  layers <- callModule(module = layersServer,
                       id = 'layers',
                       dataset = dataset)

  # Aesthetics UI
  output$aesthetics <- renderUI({
    req(layers$selected_layer())
    layerAestheticsUI(id = paste0(session$ns(paste0('layers-', layers$selected_layer())),'-aesthetics'))
  })

  # Handle stat changes
  observe({
    req(layers$selected_stat())
    dndselectr::updateDragZone(session,
                               id = paste(session$ns("computed-vars"), "computeddatazone", sep = "-"),
                               choices = dataInputChoices(stat_computed_vars[[layers$selected_stat()]]))
    if (is.null(stat_computed_vars[[layers$selected_stat()]])) {
      shinyjs::js$removeClass("hidden", "em.none-computed")
    } else {
      shinyjs::js$addClass("hidden", "em.none-computed")
    }
  })

  # _ Plot ====
  output$viz <- renderPlot({
    req(ggobj())
    failure <- FALSE
    # Try to plot.  If unsuccessful, pass error message to help pane.
    # We need the print statement here or we can't capture errors
    # See: https://aryoda.github.io/tutorials/tryCatchLog/tryCatchLog-intro-slides.html#/code-snippet-for-better-error-handling
    tryCatch(
      withCallingHandlers(
        withRestarts(
          print(ggobj()),
          muffleError = function() {
            failure <<- TRUE
            NULL
          }
        ),
        warning = function(w) {
          isolate(ggplot2_log(paste0("[", format(Sys.time(), "%X"), "] <span style='color:#C4A000'>**Warning**</span>: ", w$message, "<br/>", ggplot2_log())))
          invokeRestart("muffleWarning")
        },
        message = function(m) {
          isolate(ggplot2_log(paste0("[", format(Sys.time(), "%X"), "] <span style='color:#3d77c2'>**Message**</span>: ",  m$message, "<br/>", ggplot2_log())))
          invokeRestart("muffleMessage")
        },
        error = function(e) {
          if (nchar(e$message)) {
            isolate(ggplot2_log(paste0("[", format(Sys.time(), "%X"), "] <span style='color:#CC0000'>**Error**</span>: ", e$message, "<br/>", ggplot2_log())))
            shinyjs::show(id = "error-pane", anim = FALSE)
            shinyjs::html(id = "error-pane", html = e$message)
          }
          invokeRestart("muffleError")
        }),
      finally = {
        if (!failure) shinyjs::hide(id = "error-pane", anim = FALSE)
      }
    )
  })

  # _ Code ====
  output$code <- renderUI({
    req(ggcode())
    lines <- fansi::sgr_to_html(prettycode::highlight(ggcode(),
                                                      style = terminal_dark_theme()))
    HTML(
      paste0(
        purrr::map2(
          lines,
          purrr::map(gregexpr("^\\s+", lines), ~ attr(., "match.length")),
          ~ ifelse(.y > 0, stringr::str_replace(.x, "^\\s+", paste0(rep("&nbsp;", .y), collapse = "")), .x)
        ),
        collapse = "<br/>")
    )
  })
  outputOptions(output, "code", suspendWhenHidden = FALSE)  # Look into Shiny way of handling tabs

  output$log <- renderUI({
    req(ggplot2_log())
    HTML(markdown::markdownToHTML(text = ggplot2_log(), fragment.only = TRUE))
  })
  outputOptions(output, "log", suspendWhenHidden = FALSE)

  ggcode <- reactive({
    req(layers$code())
    code <- attributes(dataset)$df_name
    if (isTruthy(subsetted_data())) {
      code <- paste(code,
                    "%>%\n",
                    subsetted_data()
      )
    }

    if (isTruthy(layers$code())) {
      code <- paste(code,
                    "%>%\n",
                    layers$code()
      )
    }

    if (isTruthy(labs_code())) {
      code <- paste(code,
                    "+\n",
                    labs_code()
      )
    }

    return(styler::style_text(code))
  })

  ggobj <- reactive({
    req(ggcode())
    eval(parse(text=gsub(attributes(dataset)$df_name, "dataset", ggcode())))
  })

  # BEGIN: Labels module ----

  # _ label reactives ----
  xlabel <- reactive({
    req(ggobj())
    ggobj()$labels$x
  })

  ylabel <- reactive({
    req(ggobj())
    ggobj()$labels$y
  })

  labs_code <- callModule(module = labelsServer,
                          id = "labels",
                          xlabel = xlabel,
                          ylabel = ylabel)

  # END: Labels module ----

  # Events ----------------------

  # _ Done ====
  # User is done - tried this, but didn't work
  #   https://stackoverflow.com/questions/34731975/how-to-listen-for-more-than-one-event-expression-within-a-shiny-eventreactive-ha
  observeEvent(input$done, {
    shinyjs::js$close_window()
    stopApp(returnValue = ggcode())
  })

  # _ Cancel ====
  observeEvent(input$cancel, {
    shinyjs::js$close_window()
    stopApp(returnValue = NULL)
  })

  return(ggcode)
}

# GLOBALS ----

revList <- function(x) {
  tmp <- names(x)
  names(tmp) <- unlist(x)
  as.list(tmp)
}

resourcePath <- system.file("www", package = "serenity.viz")

plot_names_one <- list(
  "Primitive" = list(
    "geom-segment" = "Segment",
    "geom-curve" = "Curve",
    "geom-path" = "Path",
    "geom-rect" = "Rectangle",
    "geom-tile" = "Tile",
    "geom-polygon" = "Polygon",
    "geom-ribbon" = "Ribbon",
    "geom-area" = "Area Plot"
  ),
  "Discrete" = list(
    "geom-bar" = "Bar Plot"
  ),
  "Continuous" = list(
    "geom-histogram" = "Histogram",
    "geom-dotplot" = "Dot Plot",
    "geom-density" = "Density",
    "geom-freqpoly" = "Freq-Poly"
  )
)

plot_names_two <- list(
  "Continuous X, Continuous Y" = list(
    "geom-point" = "Scatter Plot",
    "geom-smooth" = "Smoother",
    "geom-rug" = "Rug Plot"
  ),
  "Discrete X, Continuous Y" = list(
    "geom-boxplot" = "Box Plot",
    "geom-violin" = "Violin Plot"
  ),
  "Continuous Function" = list(
    "geom-line" = "Line Plot"
  ),
  "Visualizing Error" = list(
  )
)

plot_names_three <- list(
  "Misc" = list(
    "geom-raster" = "Raster"
  )
)

plot_names <- unlist(c(plot_names_one, plot_names_two, plot_names_three), recursive = FALSE)
names(plot_names) <- unlist(lapply(c(plot_names_one, plot_names_two, plot_names_three), function(x) { revList(x) }), recursive = FALSE)

geoms <- names(plot_names)

stat_names <- list(
  "1D distributions" = list(
    "bin" = "Binning",
    "count" = "Count",
    "density" = "Density (x)"
  ),
  "2D distributions" = list(
    "bin_2d" = "Binning (2D Rect)",
    "bin_hex" = "Binning (2D Hex)",
    "density_2d" = "Density (2D)",
    "ellipse" = "Confidence Ellipse"
  ),
  "3 Variables" = list(
    "contour" = "Contours",
    "summary_hex" = "Summaries (2D Hex)",
    "summary_2d" = "Summaries (2D Rect)"
  ),
  "Comparisons" = list(
    "boxplot" = "Boxplot",
    "ydensity" = "Density (y)"
  ),
  "Functions" = list(
    "ecdf" = "Emperical CDF",
    "quantile" = "Quantiles",
    "smooth" = "Smooth"
  ),
  "General Purpose" = list(
    "function" = "Function",
    "identity" = "Identity",
    "qq" = "Quantile-Quantile (QQ)",
    "qq_line" = "QQ Line",
    "sum" = "Sum",
    "summary" = "Summaries",
    "summary_bin" = "Summaries (Bins)",
    "unique" = "Unique",
    "bindot" = "Binning (Dotplot)",
    "sf" = "SF",
    "sf_coordinates" = "SF Coords"
  )
)

stat_names_unlist <- unlist(stat_names, recursive = FALSE)
names(stat_names_unlist) <- unlist(lapply(stat_names, function(x) { revList(x) }), recursive = FALSE)

stat_computed_vars <- list(
  "count" = c("count", "prop"),
  "bin" = c("count", "density", "ncount", "ndensity"),
  "sum" = c("n", "prop"),
  "density" = c("density", "count", "scaled", "ndensity"),
  "smooth" = c("y", "ymin", "ymax", "se"),
  "summary" = c("y", "ymin", "ymax"),
  "boxplot" = c("x", "width", "ymin", "ymax", "lower", "middle", "upper", "notchlower", "notchupper"),
  "function" = c("x", "y"),
  "quantile" = c("quantile"),
  "qq" = c("sample", "theoretical"),
  "qq_line" = c("x", "y"),
  "ecdf" = c("x", "y"),
  "ellipse" = c("x", "y"),
  "contour" = c("level", "nlevel", "piece"),
  "ydensity" = c("density", "scaled", "count", "violinwidth", "n", "width"),
  "bindot" = c("x", "y", "binwidth", "count", "ncount"),
  "bin_2d" = c("count", "density", "ncount", "ndensity"),
  "bin_hex" = c("count", "density", "ncount", "ndensity"),
  "summary_bin" = c("y", "ymin", "ymax"),
  "summary_hex" = c("x", "y", "value"),
  "summary_2d" = c("x", "y", "value"),
  "density_2d" = c("density", "ndensity")
)

computed_word <- ifelse(packageVersion("ggplot2") < "3.3.0", "stat", "after_stat")
stat_additional_defaults <- list(
  "smooth" = c("ymin", "ymax"),
  "boxplot" = c("x", "ymin", "ymax", "lower", "middle", "upper")
)
stat_additional_defaults <- lapply(stat_additional_defaults,
       function(x) {
         lapply(as.list(x) %>% {
           names(.) <- x
           .},
           function(y) { quo(!!sym(paste0(computed_word, "(", y, ")"))) }) })

NA_defaults <- list(
  fill = "#FFFFFF",
  colour = "#333333",
  alpha = 1,
  shape = 19,
  size = 1.5,
  stroke = 0.5,
  width = 1,
  height = 1
)

help_panes <- lapply(geoms, function(x) {
  paste0("<h2>", x, "</h2>
         <div class='axis' id='xaxis'></div>
         <div class='axis' id='yaxis'></div>")
})
names(help_panes) <- stringr::str_replace(geoms, "-", "_")

makeReactiveTrigger <- function(init_val = NULL) {
  rv <- reactiveValues(a = 0)
  val <- init_val
  list(
    get = function() {
      val
    },
    set = function(new_val) {
      val <<- new_val
    },
    depend = function() {
      rv$a
      invisible()
    },
    trigger = function() {
      rv$a <- isolate(rv$a + 1)
    }
  )
}

gg_aesthetics <- reorderElements(
  list(
    "geom-blank" = c("x",
                     "y",
                     "colour",
                     "fill",
                     "alpha",
                     "size",
                     "stroke",
                     "shape",
                     "width",
                     "height",
                     "linetype",
                     "group",
                     "subgroup",
                     "weight",
                     "xmin",
                     "xmax",
                     "ymin",
                     "ymax",
                     "lower",
                     "xlower",
                     "upper",
                     "xupper",
                     "middle",
                     "xmiddle",
                     "xend",
                     "yend",
                     "sample"),
    "geom-bar" = ggplot2::GeomBar$aesthetics(),
    "geom-histogram" = ggplot2::GeomBar$aesthetics(),
    "geom-point" = ggplot2::GeomPoint$aesthetics(),
    "geom-line" = ggplot2::GeomLine$aesthetics(),
    "geom-dotplot" = ggplot2::GeomDotplot$aesthetics(),
    "geom-boxplot" = ggplot2::GeomBoxplot$aesthetics(),
    "geom-violin" = ggplot2::GeomViolin$aesthetics(),
    "geom-rug" = ggplot2::GeomRug$aesthetics(),
    "geom-smooth" = ggplot2::GeomSmooth$aesthetics(),
    "geom-segment" = ggplot2::GeomSegment$aesthetics(),
    "geom-curve" = ggplot2::GeomCurve$aesthetics(),
    "geom-path" = ggplot2::GeomPath$aesthetics(),
    "geom-rect" = ggplot2::GeomRect$aesthetics(),
    "geom-tile" = ggplot2::GeomTile$aesthetics(),
    "geom-polygon" = ggplot2::GeomPolygon$aesthetics(),
    "geom-raster" = ggplot2::GeomRaster$aesthetics(),
    "geom-ribbon" = ggplot2::GeomRibbon$aesthetics(),
    "geom-area" = ggplot2::GeomArea$aesthetics(),
    "geom-density" = ggplot2::GeomDensity$aesthetics(),
    "geom-freqpoly" = ggplot2::geom_freqpoly()$geom$aesthetics()
  )
)

terminal_dark_theme <- function() {
  mystyle <- prettycode::default_style()
  mystyle$call <- crayon::make_style("#06989A")
  mystyle$number <- crayon::make_style("#3465A4")
  mystyle$null <- crayon::make_style("#CC0000")
  mystyle$operator <- crayon::make_style("#4E9A06")
  return(mystyle)
}
