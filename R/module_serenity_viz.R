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

  miniUI::miniPage(
    style = switch(!is.null(height), paste("height:", height), height),
    shinyjs::useShinyjs(),
    shinyjs::extendShinyjs(
      script = file.path(resourcePath, "js", "shinyjs-funcs.js"),
      functions = c("close_window", "toggleClass", "addClass", "removeClass", "myshow", "myhide")
    ),
    bsplus::use_bs_tooltip(),
    tags$head(includeCSS(file.path(resourcePath, "css", "app.css"))),
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

  # Server code for layer dropzones
  dndselectr::dropZoneServer(session, "layers", layerUI)
  dndselectr::dropZoneServer(session, "base-layer", layerUI)

  # This stores returned reactives from layer modules
  layer_modules <- reactiveValues()

  # Store log for warnings
  ggplot2_log <- reactiveVal("")

  output$luminobox <- renderLuminophor({
    luminophor() %>%
      addWidget(id = session$ns("widget-geoms-and-layers"),
                ui = tagList(
                  widgetHeader(
                    div(
                      style = "display: flex; flex-direction: row; justify-content: flex-end;",
                      actionButton(
                        inputId = session$ns("add-layer-button"),
                        label = "Add Layer",
                        icon = icon("plus"),
                        style = "padding: 0; display: none;",
                        class = "add-layer"
                      ),
                      actionButton(
                        inputId = session$ns("remove-layer"),
                        label = "",
                        icon = icon("minus"),
                        style = "border: transparent; padding: 0;"
                      ),
                      prettyToggle(
                        inputId = session$ns("layer-chooser"),
                        label_on = "",
                        label_off = "",
                        status_on = "default",
                        status_off = "default",
                        outline = TRUE,
                        plain = TRUE,
                        icon_on = icon("times"),
                        icon_off = icon("plus"),
                        inline = TRUE
                      )
                    )
                  ),
                  widgetBody(
                    class = "widget-geoms-and-layers",
                    uiOutput(session$ns("widget-layers-body"))
                  )
                ),
                title = "Layers",
                icon = icon("layer-group"),
                closable = FALSE) %>%
      addWidget(id = session$ns('widget-ggplot'),
                refwidgetID = session$ns('widget-geoms-and-layers'),
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
                refwidgetID = session$ns('widget-geoms-and-layers'),
                insertmode = "tab-after",
                ui = labelsUI(session$ns("labels")),
                title = "Labels",
                icon = icon("tags"),
                closable = FALSE)
  })

  output$`widget-layers-body` <- renderUI({
    tagList(
      div(
        class = "layers-wrapper",
        div(
          class = "base-layer",
          isolate({ # See Example 22 in dndselectr
            dndselectr::dropZoneInput(
              session$ns("base_layer"),
              class = "layers",
              choices = list(
                'geom-blank' = layerUI("geom-blank")
              ),
              server = layerUI,
              presets = list(values = "geom-blank-ds-1",
                             locked = "geom-blank-ds-1",
                             freeze = "geom-blank-ds-1"),
              multivalued = TRUE,
              selectable = TRUE
            )
          })
        ),
        dndselectr::dropZoneInput(
          session$ns("layers"),
          class = "layers",
          choices = sapply(geoms, function(geom) { layerUI(geom) }, simplify = FALSE, USE.NAMES = TRUE),
          server = layerUI,
          placeholder = "Add a layer",
          multivalued = TRUE,
          selectable = TRUE,
          selectOnDrop = TRUE,
          removeOnSpill = TRUE
        )
      ),
      div(
        class = "layer-chooser-wrapper",
        style = "display: none;",
        dndselectr::dropZoneInput(session$ns("ds-layer-chooser"), choices = sapply(geoms, function(geom) { layerChoiceUI(geom) }, simplify = FALSE),
                                      class = "layer-chooser",
                                      flex = TRUE,
                                      selectable = TRUE,
                                      direction = "horizontal",
                                      presets = list(values = geoms,
                                                     locked = geoms)
        )
      )
    )
  })

  observeEvent(input$`layer-chooser`, {
    dndselectr::unselect(session, "ds-layer-chooser")
    if (input$`layer-chooser`) {
      # Toggle header views
      shinyjs::js$myhide(paste0('#', session$ns("remove-layer")))

      # Toggle body views
      shinyjs::js$myhide('.layers-wrapper')
      shinyjs::js$myshow('.layer-chooser-wrapper')
    } else {
      # Toggle header views
      shinyjs::js$myshow(paste0('#', session$ns("remove-layer")))
      shinyjs::js$myhide(paste0('#', session$ns("add-layer-button")))

      # Toggle body views
      shinyjs::js$myshow('.layers-wrapper')
      shinyjs::js$myhide('.layer-chooser-wrapper')
    }
  })

  observeEvent(input$`ds-layer-chooser_selected`, {
    if (!is.null(input$`ds-layer-chooser_selected`)) {
      shinyjs::js$myshow(paste0('#', session$ns("add-layer-button")))
    }
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

  # The next two observe events handle selection of layers
  observeEvent(input$base_layer_selected, {
    if (!is.null(input$layers_selected)) {
      dndselectr::unselect(session, "layers")
    }
  }, ignoreInit = TRUE)

  observeEvent(input$layers_selected, {
    if (!is.null(input$base_layer_selected) && !is.null(input$layers_selected)) {
      dndselectr::unselect(session, "base_layer")
    } else if (is.null(input$base_layer_selected) && is.null(input$layers_selected)) {
      dndselectr::select(session, "geom-blank-ds-1", "base_layer")
    }
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  observeEvent(input$`add-layer-button`, {
    shinyWidgets::updatePrettyToggle(session, "layer-chooser", value = FALSE)
    dndselectr::appendToDropzone(session, input$`ds-layer-chooser_selected`, "layers")
  })

  observeEvent(input$`remove-layer`, {
    if (!is.null(input$layers_selected)) {
      dndselectr::removeSelected(session, "layers")
    }
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

  # Aesthetics UI
  output$aesthetics <- renderUI({
    req(selected_layer())
    layerAestheticsUI(id = paste0(session$ns(selected_layer()),'-aesthetics'))
  })

  all_layers <- eventReactive(input$layers, {
    c(input$base_layer, input$layers)
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  selected_layer <- eventReactive(input$layers_selected, {
    input$layers_selected %||% "geom-blank-ds-1"
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  # Get the names of the visible layers
  visible_layers <- eventReactive(paste(all_layers(), input$layers_invisible), {
    setdiff(all_layers(), input$layers_invisible)
  }, ignoreInit = TRUE)

  # Handle stat changes
  observe({
    req(selected_layer())
    stat <- layer_modules[[selected_layer()]]$stat()
    dndselectr::updateDragZone(session,
                               id = paste(session$ns("computed-vars"), "computeddatazone", sep = "-"),
                               choices = dataInputChoices(stat_computed_vars[[stat]]))
    if (is.null(stat_computed_vars[[stat]])) {
      shinyjs::js$removeClass("hidden", "em.none-computed")
    } else {
      shinyjs::js$addClass("hidden", "em.none-computed")
    }
  })

  # Preps geom_blank dropzone inputs for layer modules
  geom_blank_inputs_to_reactives <- function() {
    geom_blank_inputs <- as.list(paste0('geom-blank-ds-1-aesthetics-', gg_aesthetics[["geom-blank"]], '-mapping'))
    names(geom_blank_inputs) <- gg_aesthetics[["geom-blank"]]
    return(geom_blank_inputs %>% purrr::map(~ reactive({ input[[.]] })))
  }

  # Update layer module output reactives - create only once!
  observeEvent(all_layers(), {
    # Adding new layers
    purrr::map(setdiff(all_layers(), names(layer_modules)), ~ {
      layer_modules[[.]] <- callModule(module = layerServer, id = .,
                                       selected_layer,
                                       geom_blank_inputs_to_reactives(),
                                       dataset = dataset,
                                       ggbase = switch(as.character(. != "geom-blank-ds-1"),
                                                       "TRUE" = layer_modules[["geom-blank-ds-1"]]$code,
                                                       "FALSE" = reactive({ NULL }))
      )
    })

    # Remove old layers
    purrr::map(setdiff(names(layer_modules), all_layers()), ~ { layer_modules[[.]] <- NULL })
  }, priority = 1) # Needs to happen before layer_code reactive

  # Get layer code
  layer_code <- reactive({
    req(visible_layers(),
        purrr::map(reactiveValuesToList(layer_modules)[visible_layers()], ~ .$code()))
    paste(purrr::map(reactiveValuesToList(layer_modules)[visible_layers()], ~ .$code()), collapse = "+\n")
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
    req(layer_code())
    code <- attributes(dataset)$df_name
    if (isTruthy(subsetted_data())) {
      code <- paste(code,
                    "%>%\n",
                    subsetted_data()
      )
    }

    if (isTruthy(layer_code())) {
      code <- paste(code,
                    "%>%\n",
                    layer_code()
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

resourcePath <- system.file("www", package = "serenity.viz")

geoms <- c("geom-bar", "geom-histogram", "geom-point", "geom-line", "geom-dotplot", "geom-boxplot", "geom-violin", "geom-rug", "geom-smooth")
plot_names <- list(
  "geom-bar" = "Bar Plot",
  "geom-histogram" = "Histogram",
  "geom-point" = "Scatter Plot",
  "geom-line" = "Line Plot",
  "geom-dotplot" = "Dot Plot",
  "geom-boxplot" = "Box Plot",
  "geom-violin" = "Violin Plot",
  "geom-rug" = "Rug Plot",
  "geom-smooth" = "Smoother"
)

stat_names <- list(
  "identity" = "Identity",
  "count" = "Count",
  "bin" = "Binning",
  "sum" = "Sum",
  "density" = "Density (x)",
  "smooth" = "Smooth",
  "summary" = "Summaries",
  "boxplot" = "Boxplot",
  "function" = "Function",
  "quantile" = "Quantiles",
  "qq" = "Quantile-Quantile (QQ)",
  "qq_line" = "QQ Line",
  "ecdf" = "Emperical CDF",
  "ellipse" = "Confidence Ellipse",
  "contour" = "Contours",
  "ydensity" = "Density (y)",
  "bin_2d" = "Binning (2D Rect)",
  "bin_hex" = "Binning (2D Hex)",
  "summary_bin" = "Summaries (Bins)",
  "summary_hex" = "Summaries (2D Hex)",
  "summary_2d" = "Summaries (2D Rect)",
  "density_2d" = "Density (2D)",
  "sf" = "SF",
  "sf_coordinates" = "SF Coords",
  "unique" = "Unique"
)
stats <- names(stat_names)

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
  "bin_2d" = c("count", "density", "ncount", "ndensity"),
  "bin_hex" = c("count", "density", "ncount", "ndensity"),
  "summary_bin" = c("y", "ymin", "ymax"),
  "summary_hex" = c("x", "y", "value"),
  "summary_2d" = c("x", "y", "value"),
  "density_2d" = c("density", "ndensity")
)

computed_word <- ifelse(packageVersion("ggplot2") < "3.3.0", "stat", "after_stat")
stat_additional_defaults <- list(
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
  stroke = 0.5
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
                     "linetype",
                     "group",
                     "weight",
                     "xmin",
                     "xmax",
                     "ymin",
                     "ymax",
                     "lower",
                     "upper",
                     "middle",
                     "sample"),
    "geom-bar" = ggplot2::GeomBar$aesthetics(),
    "geom-histogram" = ggplot2::GeomBar$aesthetics(),
    "geom-point" = ggplot2::GeomPoint$aesthetics(),
    "geom-line" = ggplot2::GeomLine$aesthetics(),
    "geom-dotplot" = ggplot2::GeomDotplot$aesthetics(),
    "geom-boxplot" = ggplot2::GeomBoxplot$aesthetics(),
    "geom-violin" = ggplot2::GeomViolin$aesthetics(),
    "geom-rug" = ggplot2::GeomRug$aesthetics(),
    "geom-smooth" = ggplot2::GeomSmooth$aesthetics()
  )
)

idsToGeoms <- function(id) {
  switch(!is.null(id),
         sapply(id, FUN = function(x) { paste(stringr::str_split(x, '-')[[1]][1:2], collapse="-") }, simplify = "array"),
         NULL)
}

revList <- function(x) {
  tmp <- names(x)
  names(tmp) <- unlist(x)
  as.list(tmp)
}

terminal_dark_theme <- function() {
  mystyle <- prettycode::default_style()
  mystyle$call <- crayon::make_style("#06989A")
  mystyle$number <- crayon::make_style("#3465A4")
  mystyle$null <- crayon::make_style("#CC0000")
  mystyle$operator <- crayon::make_style("#4E9A06")
  return(mystyle)
}
