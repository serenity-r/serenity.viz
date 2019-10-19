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

  if (geom_type == "geom-blank") {
    geom_name <- tagList("Base", br(), "Layer")
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
            icon("sort", class = "ds-handle"),
            div(class = paste("geom-icon", geom_type)),
            span(class = "geom-name", geom_name)
          ),
          switch(geom_type != "geom-blank", span("Other stuff", class="hidden"), NULL)
        )
      ),
      wellPanel(
        class = "layer-settings-and-params",
        switch(as.character(server),
               "TRUE" = uiOutput(ns("settings-or-params")),
               "FALSE" = "Client stuff to say")
      ),
      div(
        class = "layer-icons",
        tagList(
          switch(geom_type != "geom-blank",
                 prettyToggle(
                   inputId = ns("toggle-settings-or-params"),
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
    )
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

  ns <- session$ns

  # _ Initialization and Setup ====

  # _ _ Get layer, geom, and aesthetics information ====
  layer_id <- paste(stringr::str_split(gsub("-$", "", ns('')), '-')[[1]][2:5], collapse="-")
  geom_type <- paste(stringr::str_split(layer_id, '-')[[1]][1:2], collapse="-")
  geom_proto <- eval(parse(text=paste0(stringr::str_replace(geom_type, "-", "_"), "()")))

  layer_instance <- dragulaSelectR::multivalues(layer_id, ids=TRUE)

  # _ _ load parameters module ====

  # Ignoring subsetted data for now
  gglayercode <- reactive({
    req({
      ggbase()
      base_layer_code()
    })
    paste("dataset %>%", ggbase(), "+", paste0(base_layer_code(), ")"))
  })

  gglayerobj <- reactive({
    req(gglayercode())
    eval(parse(text=gglayercode()))
  })

  gglayerdata <- reactive({
    req(gglayerobj())
    failure <- FALSE
    tryCatch(
      withCallingHandlers(
        withRestarts(
          print(gglayerobj()),
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
          return(layer_data(gglayerobj(), 1))
        } else {
          return(NULL)
        }
      }
    )
  })

  layer_params <- NULL
  if (geom_type != "geom-blank") {
    layer_params <- callModule(module = layerParamsServer,
                               id = 'params',
                               ggdata = gglayerdata
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
                                 inherit.aes)

  # _ settings or params UI ====
  # Doesn't really need to be a renderUI at the moment but keeping for ease of reading
  output$`settings-or-params` <- renderUI({
    tagList(
      div(
        class = "layer-settings", # Settings
        verbatimTextOutput(ns("summary"), placeholder = TRUE)
      ),
      div(
        class = "layer-params", # Parameters
        style = "display: none;",
        layerParamsUI(ns('params'))
      )
    )
  })

  # _ _ toggle hide/show of settings or params ====
  observeEvent(input$`toggle-settings-or-params`, {
    # Toggle class for params
    if (input$`toggle-settings-or-params`) {
      shinyjs::js$myhide(paste0('#', ns("settings-or-params"), ' .layer-settings'))
      shinyjs::js$myshow(paste0('#', ns("settings-or-params"), ' .layer-params'))
      shinyjs::js$addClass('params', paste0('.ds-dropoption[data-value="', geom_type, '"][data-instance="', layer_instance, '"] .layer-wrap'))
    } else {
      shinyjs::js$myshow(paste0('#', ns("settings-or-params"), ' .layer-settings'))
      shinyjs::js$myhide(paste0('#', ns("settings-or-params"), ' .layer-params'))
      shinyjs::js$removeClass('params', paste0('.ds-dropoption[data-value="', geom_type, '"][data-instance="', layer_instance, '"] .layer-wrap'))
    }
  })

  # _ _ settings output ====
  output$summary <- renderPrint({
    # req(layer_aesthetics())
    # layer_aesthetics()
  })

  # _ layer code ====
  hasAesthetics <- reactive({
    isTruthy(layer_aesthetics) && nchar(layer_aesthetics())
  })

  base_layer_code <- reactive({
    processed_layer_code <- paste0(ifelse(geom_type == "geom-blank",
                                          "ggplot",
                                          stringr::str_replace(geom_type, "-", "_")), "(")

    if (hasAesthetics()) {
      processed_layer_code <- paste0(processed_layer_code,
                                     layer_aesthetics())
    }

    return(processed_layer_code)
  })

  layer_code <- reactive({
    processed_layer_code <- base_layer_code()

    if (isTruthy(layer_params) &&
        isTruthy(layer_params$code) &&
        nchar((layer_params$code()))) {
      processed_layer_code <- paste0(processed_layer_code,
                                     ifelse(hasAesthetics(), ",\n", ""),
                                     layer_params$code())
    }

    processed_layer_code <- paste0(processed_layer_code, ")")

    return(processed_layer_code)
  })

  return(layer_code)
}
