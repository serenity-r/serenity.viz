#' UI for Serenity Viz module
#'
#' @param id Serenity Viz module ID
#' @param dataset Passed in dataset for visualization
#' @param titlebar Show title bar with Done and Cancel buttons?
#' @param showcode Show code for plots?
#'
#' @return UI for Serenity Viz module
#'
#' @import shiny
#' @export
#'
serenityVizUI <- function(id, dataset, titlebar = FALSE, showcode = TRUE, height = NULL) {
  ns <- NS(id)

  miniUI::miniPage(
    style = switch(!is.null(height), paste("height:", height), height),
    shinyjs::useShinyjs(),
    shinyjs::extendShinyjs(
      script = file.path(resourcePath, "js", "shinyjs-funcs.js"),
      functions = c("close_window")
    ),
    bsplus::use_bs_tooltip(),
    tags$head(includeCSS(file.path(resourcePath, "css", "app.css"))),
    switch(titlebar,
           miniUI::gadgetTitleBar("Serenity Viz",
                                  left = miniUI::miniTitleBarCancelButton(ns("cancel")),
                                  right = miniUI::miniTitleBarButton(ns("done"), "Done", primary = TRUE)),
           NULL),
    fillRow(
      flex = c(2, 3),

      # Variables
      fillRow(
        class = "vars-and-aes",
        height = NULL,
        width = "95%",
        flex = c(1, 1),
        dataUI(id = ns(attributes(dataset)$df_name)),
        uiOutput(ns("aesthetics"), inline=TRUE)
      ),

      # Layers, plot, and code
      fillCol(
        flex = c(NA, 7, NA, ifelse(showcode, 3, NA)),
        wellPanel(
          class = "plots-and-layers",
          div(
            h4("Plot Types"),
            dragulaSelectR::dragZone(ns("geoms"),
                                     class = "geoms",
                                     choices = sapply(geoms, function(geom) { div(style = "width: inherit; height: inherit;") %>% bsplus::bs_embed_tooltip(title = plot_names[[geom]]) }, simplify = FALSE, USE.NAMES = TRUE))
          ),
          div(
            h4("Plots Used"),
            uiOutput(ns("layersUI"))
          )
        ),
        miniUI::miniContentPanel(
          class = "ggplot",
          style = "padding: 19px;",
          plotOutput(ns("viz"), height = "100%"),
          shinyjs::hidden(
            absolutePanel(id = ns("help-pane"),
                          class = "help-pane",
                          top = "20px",
                          draggable = FALSE
            )
          )
        ),
        shinyWidgets::dropdownButton(
          labelsUI(id = ns("labels")),
          inputId = "label-btn",
          circle = FALSE,
          status = "primary",
          label = "Set Labels",
          icon = icon("tags"), # other options: comment, edit, i-cursor, info-circle, map-signs, marker, pen-square
          size = "sm",
          right = TRUE,
          up = TRUE),
        switch(showcode,
               miniUI::miniContentPanel(
                 tabsetPanel(
                   id = ns("verbose"),
                   type = "tabs",
                   tabPanel("Code", verbatimTextOutput(ns("code"))),
                   tabPanel("Log", verbatimTextOutput(ns("log")))
                 )
               ),
               NULL)
      )
    )
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
#' @import shiny ggplot2 dplyr forcats
#' @export
#'
serenityVizServer <- function(input, output, session, dataset, trigger=NULL) {
  if (is.null(attr(dataset, "df_name"))) {
    attr(dataset, "df_name") <- deparse(substitute(dataset))
  }

  layer_id <- paste(stringr::str_split(gsub("-$", "", session$ns('')), '-')[[1]][2:5], collapse="-")

  # This stores returned reactives from layer modules
  layer_modules <- reactiveValues()

  # Store log for warnings
  ggplot2_log <- reactiveVal("")

  # Data module
  subsetted_data <- callModule(module = dataServer,
                               id = attributes(dataset)$df_name,
                               dataset = dataset)

  output$layersUI <- renderUI({
    if (!is.null(trigger)) trigger()
    isolate({
      dragulaSelectR::dropZoneInput(session$ns("layers"),
                                    class = "layers",
                                    choices = c("geom-blank" = "",
                                                sapply(geoms, function(geom) { "" }, simplify = FALSE, USE.NAMES = TRUE)),
                                    presets = list(
                                      values = idsToGeoms(input$layers) %||% "geom-blank",
                                      selected = idsToGeoms(input$layers_selected) %||% "geom-blank",
                                      locked = "geom-blank",
                                      freeze = "geom-blank"),
                                    multivalued = TRUE,
                                    selectable = TRUE,
                                    selectOnDrop = TRUE,
                                    togglevis = TRUE,
                                    direction = "horizontal",
                                    removeOnSpill = TRUE
      )
    })
  })

  output$aesthetics <- renderUI({
    req(input$layers_selected)
    layerUI(id = session$ns(input$layers_selected))
  })

  # Get the names of the visible layers
  visible_layers <- reactive({
    setdiff(input$layers, input$layers_invisible)
  })

  # Preps geom_blank dropzone inputs for layer modules
  geom_blank_inputs_to_reactives <- function() {
    geom_blank_inputs <- as.list(paste0('geom-blank-ds-1-', gg_aesthetics[["geom-blank"]], '-dropzone'))
    names(geom_blank_inputs) <- paste0('geom-blank-ds-1-', gg_aesthetics[["geom-blank"]], '-dropzone')
    if (any(names(geom_blank_inputs) %in% names(input))) {
      return(geom_blank_inputs %>%
               purrr::map(~ reactive({ input[[.]] })))
    } else {
      return(NULL)
    }
  }

  # Update layer module output reactives - create only once!
  observeEvent(input$layers, {
    # Adding new layers
    purrr::map(setdiff(input$layers, names(layer_modules)), ~ { layer_modules[[.]] <- callModule(module = layerServer, id = .,
                                                                                                 reactive({input$layers_selected}),
                                                                                                 geom_blank_inputs_to_reactives(),
                                                                                                 dataset = dataset)} )
    # Remove old layers
    purrr::map(setdiff(names(layer_modules), input$layers), ~ { layer_modules[[.]] <- NULL })
  }, priority = 1) # Needs to happen before layer_code reactive

  # Get layer code
  layer_code <- reactive({
    req(visible_layers())
    paste(purrr::map(reactiveValuesToList(layer_modules)[visible_layers()], ~ .()), collapse = "+\n")
  })

  # _ Plot ====
  output$viz <- renderPlot({
    failure <- FALSE
    # Try to plot.  If unsuccessful, pass error message to help pane.
    # We need the print statement here or we can't capture errors
    # Note: Move to tryCatchLog package for smarter capturing
    withCallingHandlers(
      withRestarts(
        print(ggobj()),
        muffleError = function() NULL
      ),
      warning = function(w) {
        isolate(ggplot2_log(paste0("Warning: ", w$message, ggplot2_log())))
        invokeRestart("muffleWarning")
      },
      message = function(m) {
        isolate(ggplot2_log(paste0("Message: ",  m$message, ggplot2_log())))
        invokeRestart("muffleMessage")
      },
      error = function(e) {
        shinyjs::show(id = "help-pane", anim = FALSE)
        shinyjs::html(id = "help-pane", html = e$message)
        invokeRestart("muffleError")
        failure <<- TRUE
      },
      finally = {
        if (!failure) {
          shinyjs::hide(id = "help-pane", anim = FALSE)
        }
      }
    )
  })

  # _ Code ====
  output$code <- renderPrint({
    req(ggcode())
    print(ggcode())
  })

  output$log <- renderText({
    req(ggplot2_log())
    ggplot2_log()
  })

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

gg_aesthetics <- list(
  "geom-blank" = c("x",
                   "y",
                   "alpha",
                   "colour",
                   "fill",
                   "linetype",
                   "size",
                   "weight",
                   "xmin",
                   "xmax",
                   "ymin",
                   "ymax"),
  "geom-bar" = ggplot2::GeomBar$aesthetics(),
  "geom-histogram" = ggplot2::GeomBar$aesthetics(),
  "geom-point" = ggplot2::GeomPoint$aesthetics(),
  "geom-line" = ggplot2::GeomLine$aesthetics(),
  "geom-dotplot" = ggplot2::GeomDotplot$aesthetics(),
  "geom-boxplot" = c(ggplot2::GeomBoxplot$aesthetics(), "y"),
  "geom-violin" = ggplot2::GeomViolin$aesthetics(),
  "geom-rug" = ggplot2::GeomRug$aesthetics(),
  "geom-smooth" = ggplot2::GeomSmooth$aesthetics()
)
ordering <- unique(unlist(gg_aesthetics))
gg_aesthetics <- purrr::map(gg_aesthetics, ~ ordering[ordering %in% .])

idsToGeoms <- function(id) {
  switch(!is.null(id),
         sapply(id, FUN = function(x) { paste(stringr::str_split(x, '-')[[1]][1:2], collapse="-") }, simplify = "array"),
         NULL)
}
