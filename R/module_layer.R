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
            div(class = paste("geom-icon", geom_type)),
            span(class = "geom-name", geom_name)
          ),
          switch(geom_type != "geom-blank", span("Other stuff", class="hidden"), NULL)
        )
      ),
      wellPanel(
        class = "layer-settings",
        switch(as.character(server),
               "TRUE" = verbatimTextOutput(ns("setts"), placeholder = TRUE),
               "FALSE" = "Client stuff to say")
      ),
      div(
        class = "layer-icons",
        tagList(
          switch(geom_type != "geom-blank",
                 div(icon("cog")),
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
#'
#' @importFrom magrittr %>%
#' @import shiny ggplot2
#'
layerServer <- function(input, output, session, layers_selected, geom_blank_input, dataset) {

  ns <- session$ns

  # Get layer, geom, and aesthetics information
  layer_id <- paste(stringr::str_split(gsub("-$", "", ns('')), '-')[[1]][2:5], collapse="-")
  geom_type <- paste(stringr::str_split(layer_id, '-')[[1]][1:2], collapse="-")
  geom_proto <- eval(parse(text=paste0(stringr::str_replace(geom_type, "-", "_"), "()")))

  # _ load parameters module ====
  layer_params <- NULL
  if (geom_type != "geom-blank") {
    layer_params <- callModule(module = layerParamsServer, id = ns('params'), layers_selected)
  }

  inherit.aes <- reactive({
    if (isTruthy(layer_params$inherit.aes) && is.logical(layer_params$inherit.aes()))
      layer_params$inherit.aes()
    else
      geom_proto$inherit.aes
  })

  # _ load aesthetics module ====
  layer_aesthetics <- callModule(module = layerAestheticsServer, id = 'aesthetics',
                                 layers_selected,
                                 geom_blank_input,
                                 dataset,
                                 inherit.aes)

  output$setts <- renderPrint({
    req(layer_aesthetics())
    layer_aesthetics()
  })

  layer_code <- reactive({
    processed_layer_code <- paste0(ifelse(geom_type == "geom-blank",
                                          "ggplot",
                                          stringr::str_replace(geom_type, "-", "_")), "(")

    hasAesthetics <- isTruthy(layer_aesthetics) && nchar(layer_aesthetics())
    if (hasAesthetics) {
      processed_layer_code <- paste0(processed_layer_code,
                                     layer_aesthetics())
    }

    if (isTruthy(layer_params) &&
        isTruthy(layer_params$code) &&
        nchar((layer_params$code()))) {
      processed_layer_code <- paste0(processed_layer_code,
                                     ifelse(hasAesthetics, ",\n", ""),
                                     layer_params$code())
    }

    processed_layer_code <- paste0(processed_layer_code, ")")

    return(processed_layer_code)
  })

  return(layer_code)
}
