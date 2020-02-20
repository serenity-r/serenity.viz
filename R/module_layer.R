#' UI for layer module
#'
#' @param id  Layer ID
#' @param server  Render Shiny reactive elements if TRUE
#' @param session Shiny user session
#'
#' @return UI for layer
#'
layerUI <- function(id, server=FALSE, session=getDefaultReactiveDomain()) {
  # Create a namespace function using the provided id
  ns <- NS(session$ns(id))
  geom_type <- paste(stringr::str_split(ns(''), '-')[[1]][2:3], collapse="-")
  geom_proto <- eval(parse(text=paste0(stringr::str_replace(geom_type, "-", "_"), "()")))
  default_stat <- camelToSnake(stringr::str_remove(class(geom_proto$stat)[1], "Stat"))

  if (geom_type == "geom-blank") {
    geom_name <- tagList("Base Layer")
  } else {
    geom_name <- plot_names[[geom_type]]
  }

  div(
    class = "layer-wrap layer",
    tagList(
      div(
        class = "layer-title",
        tagList(
          div(
            class = "geom-layer-title",
            switch(geom_type != "geom-blank", icon("sort", class = "ds-handle")),
            div(class = paste("geom-icon", geom_type)),
            span(class = "geom-name", geom_name)
          ),
          switch(geom_type != "geom-blank", span("Other stuff", class="hidden"), NULL)
        )
      ),
      switch(geom_type != "geom-blank",
             div(
               class = "layer-stat",
               shinyWidgets::pickerInput(
                 inputId = ns("stat"),
                 label = NULL,
                 selected = default_stat,
                 choices = revList(stat_names),
                 choicesOpt = list(
                   subtext = rep("", length(stat_names)) %>% {
                     .[which(names(stat_names) == default_stat)] <- "default"
                     .
                   }
                 ),
                 options = list(
                   size = 6,
                   `live-search` = TRUE,
                   `dropup-auto` = FALSE
                 )
               ) %>% {
                 .$attribs$class <- paste(.$attribs$class, "layer-choose-stat")
                 .
               },
               htmlOutput(ns("selected_stat"))
             )
      ),
      div(
        class = "layer-icons",
        tagList(
          switch(geom_type != "geom-blank",
                 prettyToggle(
                   inputId = ns("toggle_settings_or_params"),
                   label_on = "",
                   label_off = "",
                   status_on = "default",
                   status_off = "default",
                   outline = TRUE,
                   plain = TRUE,
                   icon_on = icon("times"),
                   icon_off = icon("cog"),
                   inline = TRUE
                 ),
                 NULL),
          switch(geom_type != "geom-blank",
                 div(class = "ds-toggle-visible", icon("eye")),
                 NULL),
          div(icon("question"))
        )
      )
    ),
    uiOutput(ns("params"))
  )
}

#' UI for layer choice
#'
#' @param geom  Layer geom
#'
#' @return UI for layer chioce
#'
layerChoiceUI <- function(geom) {
  div(
    class = "layer-wrap choice",
    tagList(
      div(
        class = "layer-title",
        tagList(
          div(class = paste("geom-icon", geom)),
          div(plot_names[geom], class = "plot-name")
        )
      ),
      div(
        class = "layer-icons",
        tagList(
          icon("question"),
          icon("check")
        )
      )
    )
  )
}

#' Server for layers module
#'
#' @param input   Shiny inputs
#' @param output  Shiny outputs
#' @param session Shiny user session
#' @param layers_selected Reactive value of currently selected layer
#' @param geom_blank_input  Need geom_blank values to check for inheritance
#' @param dataset Dataset
#' @param ggbase  All your base are belong to us...
#'
#' @importFrom magrittr %>%
#' @import shiny ggplot2
#'
layerServer <- function(input, output, session, layers_selected, geom_blank_input, dataset, ggbase = NULL) {
  ggdata <- reactiveValues(base_data = NULL)
  ns <- session$ns

  # _ Initialization and Setup ====

  # _ _ Get layer, geom, and aesthetics information ====
  layer_id <- paste(stringr::str_split(gsub("-$", "", ns('')), '-')[[1]][2:5], collapse="-")
  geom_type <- paste(stringr::str_split(layer_id, '-')[[1]][1:2], collapse="-")
  geom_proto <- eval(parse(text=paste0(stringr::str_replace(geom_type, "-", "_"), "()")))
  default_stat <- camelToSnake(stringr::str_remove(class(geom_proto$stat)[1], "Stat"))

  layer_instance <- dndselectr::multivalues(layer_id, ids=TRUE)

  # _ _ load parameters module ====

  # Ignoring subsetted data for now
  ggbaselayerobj <- reactive({
    req(ggbase(), base_layer_code())
    eval(parse(text=paste("dataset %>%", ggbase(), "+", paste0(base_layer_code(), ")"))))
  })

  observeEvent(ggbaselayerobj(), {
    failure <- FALSE
    tryCatch(
      withCallingHandlers(
        withRestarts(
          suppressMessages(print(ggbaselayerobj())),
          muffleError = function() {
            failure <<- TRUE
            NULL
          }
        ),
        error = function(e) {
          invokeRestart("muffleError")
        }),
      finally = {
        if (!failure) {
          ggdata$base_data <- suppressMessages(layer_data(ggbaselayerobj(), 1))
        } else {
          ggdata$base_data <- NULL
        }
      }
    )
  })

  output$selected_stat <- renderUI({
    tagList(
      "Calculation:",
      br(),
      em(stat_names[[input$stat]])
    )
  })

  layer_params <- list(code = reactive({ "" }))
  if (geom_type != "geom-blank") {
    layer_params <- callModule(module = layerParamsServer,
                               id = 'params',
                               base_data = reactive({ ggdata$base_data }),
                               layer_stat = reactive({ input$stat %||% default_stat })
    )
  }

  # _ _ create reactive inherit.aes for aesthetics module ====
  inherit.aes <- reactive({
    if (isTruthy(layer_params$inherit.aes) && is.logical(layer_params$inherit.aes()))
      layer_params$inherit.aes()
    else
      geom_proto$inherit.aes
  })

  # _ _ load aesthetics module ====
  layer_aesthetics <- callModule(module = layerAestheticsServer, id = 'aesthetics',
                                 layers_selected,
                                 geom_blank_input,
                                 dataset,
                                 inherit.aes,
                                 reactive({ input$stat %||% default_stat }))

  # Could be conditionalPanel, but shinyWidget switch wasn't rendering correctly
  output$params <- renderUI({
    isolate({
      wellPanel(
        class = "layer-params",
        style = switch(!(input$toggle_settings_or_params %||% FALSE), "display:none;"),
        tabsetPanel(
          type = "tabs",
          tabPanel(span(icon(name = "sliders-h"), "Parameters"),
                   layerParamsUI(ns('params'))
          ),
          tabPanel(span(icon(name = "arrows-alt"), "Position"),
                   layerPositionUI(ns('position'))
          )
        )
      )
    })
  })
  outputOptions(output, "params", suspendWhenHidden = FALSE)

  # _ _ toggle hide/show of settings or params ====
  observeEvent(input$`toggle_settings_or_params`, {
    # Toggle class for params
    if (input$`toggle_settings_or_params`) {
      shinyjs::js$myshow(paste0("#", ns("params"), " .layer-params"))
    } else {
      shinyjs::js$myhide(paste0("#", ns("params"), " .layer-params"))
    }
  })

  # Call position module
  # Only need isolated base_data for now
  position_code <- callModule(module = layerPositionServer,
                              id = 'position',
                              base_data = reactive({ ggdata$base_data }),
                              default_position = tolower(stringr::str_remove(class(geom_proto$position)[1], "Position")))

  base_layer_code <- dedupe(reactive({
    req(!is.null(layer_aesthetics()))

    processed_layer_code <- paste0(ifelse(geom_type == "geom-blank",
                                          "ggplot",
                                          stringr::str_replace(geom_type, "-", "_")), "(")

    # Add stat, if appropriate
    show_stat <- (geom_type != "geom-blank") && (input$stat != default_stat)
    if (show_stat) {
      processed_layer_code <- paste0(processed_layer_code,
                                     "stat = ", squote(input$stat))
    }

    # Layer aesthetics
    processed_layer_code <- paste0(processed_layer_code,
                                   ifelse(show_stat && nchar(layer_aesthetics()), ", ", ""),
                                   layer_aesthetics())

    # Layer parameters
    processed_layer_code <- paste0(processed_layer_code,
                                   ifelse((show_stat || nchar(layer_aesthetics())) && nchar(layer_params$code()), ",\n", ""),
                                   layer_params$code())

    return(processed_layer_code)
  }))

  layer_code <- reactive({
    req(!is.null(base_layer_code()),
        !is.null(layer_params$code()),
        !is.null(position_code()))

    # Add position arguments
    processed_layer_code <- paste0(base_layer_code(),
                                   ifelse((nchar(layer_aesthetics()) || nchar(layer_params$code())) && nchar(position_code()), ",\n", ""),
                                   position_code(),
                                   ")")

    return(processed_layer_code)
  })

  return(layer_code)
}
