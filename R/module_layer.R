#' Module UI for individual layer
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

  layer_info <- getLayerInfo(ns)
  plot_id <- layer_info$plot_id
  geom_type <- layer_info$geom
  geom_proto <- eval(parse(text=paste0(stringr::str_replace(geom_type, "-", "_"), "()")))
  default_stat <- camelToSnake(stringr::str_remove(class(geom_proto$stat)[1], "Stat"))

  if (geom_type == "geom-blank") {
    plot_id <- "geom-blank"
    plot_name <- tagList("Base Layer")
  } else {
    plot_name <- plots[plots$id == plot_id, "name"]
  }

  div(
    class = "layer-wrap layer",
    tagList(
      div(
        class = "layer-title",
        tagList(
          div(
            class = "plot-layer-title",
            switch(geom_type != "geom-blank", icon("sort", class = "ds-handle")),
            div(class = paste("plot-icon", plot_id)),
            span(class = "plot-name", plot_name)
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
                 choices = lapply(stat_names, function(x) { revList(x) }),
                 choicesOpt = list(
                   subtext = rep("", length(unlist(stat_names))) %>% {
                     .[which(names(stat_names_unlist) == default_stat)] <- "default"
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
#' @return UI for layer choice
#'
layerChoiceUI <- function(plot_id) {
  plot_ind <- which(plots$id == plot_id)
  geom <- plots[plot_ind, "geom"]
  data_dim <- plots[plot_ind, "data_dim"]
  data_types <- plots[plot_ind, "data_types"]
  geom_proto <- eval(parse(text=paste0(stringr::str_replace(geom, "-", "_"), "()")))
  default_stat <- camelToSnake(stringr::str_remove(class(geom_proto$stat)[1], "Stat"))
  div(
    class = "layer-wrap choice",
    tagList(
      div(
        class = "layer-title",
        tagList(
          div(class = paste("plot-icon", plot_id)),
          div(plots[plots$id == plot_id, "name"], class = "plot-name")
        )
      ),
      div(
        class = "layer-icons",
        tagList(
          icon("question"),
          switch(default_stat != "identity", icon("calculator"))
        )
      )
    )
  )
}

#' Server for layer module
#'
#' @param id ID of layer module
#' @param selected_layer Reactive value of currently selected layer
#' @param base_layer_aesthetics  Need base layer aesthetic values to check for inheritance
#' @param dataset Dataset
#' @param ggbase  All your base are belong to us...
#'
#' @importFrom magrittr %>%
#' @import shiny ggplot2
#'
#' @return List containing individual layer module state.
#' \describe{
#'   \item{code}{Reactive expression of layer code (string)}
#'   \item{stat}{Reactive expression of currently selected stat (string)}
#'   \item{aesthetics}{Reactive expression of layer aesthetics (vector of strings)}
#' }
layerServer <- function(id, selected_layer, base_layer_aesthetics, dataset, ggbase = NULL) {
  moduleServer(
    id,
    function(input, output, session) {
      ggdata <- reactiveValues(base_data = NULL)
      ns <- session$ns

      # _ Initialization and Setup ====

      # _ _ Get layer, geom, and aesthetics information ====
      layer_info <- getLayerInfo(ns)
      layer_id <- layer_info$layer_id
      geom_type <- layer_info$geom
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
          em(stat_names_unlist[[input$stat]])
        )
      })

      layer_params <- list(code = reactive({ "" }))
      if (geom_type != "geom-blank") {
        layer_params <- layerParamsServer(id = 'params',
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
      layer_aesthetics <- layerAestheticsServer(id = 'aesthetics',
                                                layer_id = layer_id,
                                                geom = geom_type,
                                                selected_layer,
                                                base_layer_aesthetics,
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
      position_code <- layerPositionServer(id = 'position',
                                           base_data = reactive({ ggdata$base_data }),
                                           default_position = tolower(stringr::str_remove(class(geom_proto$position)[1], "Position")))

      base_layer_code <- dedupe(reactive({
        req(!is.null(layer_aesthetics$code()))

        processed_layer_code <- paste0(ifelse(geom_type == "geom-blank",
                                              "ggplot",
                                              stringr::str_replace(geom_type, "-", "_")), "(")

        # Add stat, if appropriate
        stat <- input$stat %||% default_stat
        show_stat <- (geom_type != "geom-blank") && (stat != default_stat)
        if (show_stat) {
          processed_layer_code <- paste0(processed_layer_code,
                                         "stat = ", squote(stat))
        }

        # Layer aesthetics
        processed_layer_code <- paste0(processed_layer_code,
                                       ifelse(show_stat && nchar(layer_aesthetics$code()), ", ", ""),
                                       layer_aesthetics$code())

        # Layer parameters
        processed_layer_code <- paste0(processed_layer_code,
                                       ifelse((show_stat || nchar(layer_aesthetics$code())) && nchar(layer_params$code()), ",\n", ""),
                                       layer_params$code())

        return(processed_layer_code)
      }))

      layer_code <- reactive({
        req(!is.null(base_layer_code()),
            !is.null(layer_params$code()),
            !is.null(position_code()))

        # Add position arguments
        show_stat <- (geom_type != "geom-blank") && (isolate(input$stat) != default_stat)
        processed_layer_code <- paste0(base_layer_code(),
                                       ifelse((show_stat || nchar(layer_aesthetics$code()) || nchar(layer_params$code())) && nchar(position_code()), ",\n", ""),
                                       position_code(),
                                       ")")

        return(processed_layer_code)
      })

      return(list(
        code = layer_code,
        stat = reactive({ input$stat %||% default_stat }),
        aesthetics = layer_aesthetics$aesthetics
      ))
    }
  )
}
