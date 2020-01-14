#' UI for layer aesthetics module
#'
#' @param id  Layer ID
#'
#' @return UI for layer
#'
layerAestheticsUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  geom_type <- paste(stringr::str_split(ns(''), '-')[[1]][2:3], collapse="-")

  tagList(
      widgetHeader(
        span(em(paste("Layer:", ifelse(geom_type == "geom-blank", "Base Layer", plot_names[[geom_type]]))),
             style="float:right;margin-right:10px")
      ),
      widgetBody(
        uiOutput(ns("layer_aesthetics"),
                 class = "layer-aesthetics")
      )
    )
}

#' Server for layer aesthetics module
#'
#' @param input   Shiny inputs
#' @param output  Shiny outputs
#' @param session Shiny user session
#' @param layers_selected Reactive value of currently selected layer
#' @param geom_blank_input  Need geom_blank values to check for inheritance
#' @param dataset Dataset
#' @param inherit.aes Inherit aesthetics from base layer? (reactive)
#'
#' @importFrom magrittr %>%
#' @import shiny ggplot2
#'
layerAestheticsServer <- function(input, output, session, layers_selected, geom_blank_input, dataset, inherit.aes) {
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
  }, ignoreInit = TRUE)

  # MAIN ----

  # _ Aesthetic divs ====
  output$layer_aesthetics <- renderUI({
    triggerAesUpdate$depend()

    tagList(
      lapply(aesthetics, function(aes) {
        layerAesUI(id = session$ns(aes))
      })
    )
  })

  # _ load variable subset modules ====
  aes_args <- purrr::map(aesthetics, ~ callModule(module = layerAesServer, id = .,
                                                  reactive({ triggerAesUpdate$depend() }),
                                                  geom_blank_input,
                                                  inherit.aes = inherit.aes,
                                                  default_aes = geom_proto$geom$default_aes[[.]],
                                                  dataset = dataset,
                                                  renderNum = renderNumSource()))

  # _ process subset arguments ====
  aes_code <- eventReactive(paste(purrr::map(aes_args, ~ .())), {
    # Evaluate reactives
    args <- purrr::map(aes_args, ~ .())

    # Pull out the filter and mutate elements
    mapping_args <- unlist(purrr::map(args, "mappings"))
    value_args <- unlist(purrr::map(args, "values"))

    processed_aes_code <- ''
    # Build aes code
    if (length(mapping_args)) {
      processed_aes_code <- paste0(processed_aes_code,
                                   "aes(",
                                   paste(mapping_args, collapse = ", "),
                                   ")")
    }

    # Build values code
    if (length(value_args)) {
      processed_aes_code <- paste0(processed_aes_code,
                                   ifelse(length(mapping_args), ",\n", ""),
                                   paste(value_args, collapse = ", "))
    }

    return(processed_aes_code)
  }, ignoreInit = TRUE)

  return(aes_code)
}

renderNumSource <- function() {
  nextRenderNum <- 0
  list(nextNum = function() {
    r <- nextRenderNum
    nextRenderNum <<- nextRenderNum + 1
    r
  },
  getNum = function() {
    nextRenderNum
  })
}
