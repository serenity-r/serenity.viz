#' UI for layer module
#'
#' @param id  Layer ID
#'
#' @return UI for layer
#'
layerUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  geom_type <- paste(stringr::str_split(ns(''), '-')[[1]][2:3], collapse="-")

  tagList(
    fillCol(
      flex = c(NA, 1),
      div(
        class = "title",
        h4("Layer Aesthetics"),
        switch(geom_type != "geom-blank",
               shinyWidgets::dropdownButton(
                 layerParamsUI(ns('layer-params')),
                 inputId = ns("layer-params-btn"),
                 status = "primary",
                 icon = icon("gear"),
                 size = "sm",
                 right = TRUE,
                 tooltip = shinyWidgets::tooltipOptions(title = "Layer Parameters")),
               NULL)
      ),
      miniUI::miniContentPanel(
        id = ns("selected-aes-col"),
        class = "selected-aes-col",
        div(
          id = ns("layer-aes-wrap"),
          class = "layer-aes",
          uiOutput(ns("layer_aes"), inline = FALSE)
        )
      )
    )
  )
}

#' Server for layer module
#'
#' @param input   Shiny inputs
#' @param output  Shiny outputs
#' @param session Shiny user session
#' @param layers_selected Reactive value of currently selected layer
#' @param geom_blank_input  Need geom_blank values to check for inheritance
#' @param dataset Dataset
#'
#' @importFrom magrittr %>%
#' @import shiny ggplot2
#'
layerServer <- function(input, output, session, layers_selected, geom_blank_input, dataset) {
  # This contains the layer id
  ns <- session$ns

  # Get layer, geom, and aesthetics information
  layer_id <- paste(stringr::str_split(gsub("-$", "", ns('')), '-')[[1]][2:5], collapse="-")
  geom_type <- paste(stringr::str_split(layer_id, '-')[[1]][1:2], collapse="-")
  geom_proto <- eval(parse(text=paste0(stringr::str_replace(geom_type, "-", "_"), "()")))
  aesthetics <- gg_aesthetics[[geom_type]]

  # Create trigger for this layers update
  triggerAesUpdate <- makeReactiveTrigger()
  observeEvent(layers_selected(), {
    if (layers_selected() == layer_id) {
      triggerAesUpdate$trigger()
    }
  })

  # MAIN ----

  # _ Aesthetic divs ====
  output$layer_aes <- renderUI({
    triggerAesUpdate$depend()

    ns <- session$ns
    bsa <- bsplus::bs_accordion(id = ns("aes")) %>%
      bsplus::bs_set_opts(panel_type = "success", use_heading_link = TRUE)
    lapply(aesthetics, function(aes) {
      bsa <<- layerAesUI(id = ns(aes), bsa)
    })
    bsa
  })

  # _ load parameters module ====
  layer_params <- NULL
  if (geom_type != "geom-blank") {
    layer_params <- callModule(module = layerParamsServer, id = 'layer-params', reactive({ triggerAesUpdate$depend() }))
  }

  # _ load variable subset modules ====
  layer_args <- purrr::map(aesthetics, ~ callModule(module = layerAesServer, id = .,
                                                    reactive({ triggerAesUpdate$depend() }),
                                                    geom_blank_input,
                                                    inherit.aes = geom_proto$inherit.aes,
                                                    default_aes = geom_proto$geom$default_aes[[.]],
                                                    dataset = dataset))

  # _ process subset arguments ====
  layer_code <- reactive({
    # Evaluate reactives
    args <- purrr::map(layer_args, ~ .())

    # Pull out the filter and mutate elements
    mapping_args <- unlist(purrr::map(args, "mappings"))
    value_args <- unlist(purrr::map(args, "values"))

    processed_layer_code <- paste0(ifelse(geom_type == "geom-blank",
                                          "ggplot",
                                          stringr::str_replace(geom_type, "-", "_")), "(")

    # Build aes code
    if (length(mapping_args)) {
      processed_layer_code <- paste0(processed_layer_code,
                                     "aes(",
                                     paste(mapping_args, collapse = ", "),
                                     ")")
    }

    # Build values code
    if (length(value_args)) {
      processed_layer_code <- paste0(processed_layer_code,
                                     ifelse(length(mapping_args), ",\n", ""),
                                     paste(value_args, collapse = ", "))
    }

    # Build parameter code
    if (isTruthy(layer_params) &&
        isTruthy(layer_params()) &&
        length(layer_params())) {
      processed_layer_code <- paste0(processed_layer_code,
                                     ifelse(length(mapping_args)+length(value_args), ",\n", ""),
                                     paste(layer_params(), collapse = ", "))
    }

    processed_layer_code <- paste0(processed_layer_code, ")")

    return(processed_layer_code)
  })

  return(layer_code)
}
