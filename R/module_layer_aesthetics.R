#' UI for layer aesthetics module
#'
#' @param id  Layer ID
#'
#' @return UI for layer
#'
layerAestheticsUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  layer_info <- getLayerInfo(ns)
  geom_type <- layer_info$geom
  plot_name <- plots[plots$id == layer_info$plot_id, "name"]

  tagList(
      widgetHeader(
        span(em(paste("Layer:", ifelse(geom_type == "geom-blank", "Base Layer", plot_name))),
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
#' @param layer_selected Reactive value of currently selected layer
#' @param base_layer_mappings  Need base layer aesthetic mappings for inheritance
#' @param dataset Dataset
#' @param inherit.aes Inherit aesthetics from base layer? (reactive)
#' @param layer_stat Reactive value of currently selected layer stat
#'
#' @importFrom magrittr %>%
#' @import shiny ggplot2
#'
layerAestheticsServer <- function(input, output, session, layer_selected, base_layer_mappings, dataset, inherit.aes, layer_stat) {
  # This contains the layer id
  ns <- session$ns

  # Get layer, geom, and aesthetics information
  layer_info <- getLayerInfo(ns)
  layer_id <- layer_info$layer_id
  geom_type <- layer_info$geom
  geom_proto <- eval(parse(text=paste0(stringr::str_replace(geom_type, "-", "_"), "()")))
  stat_proto <- reactive({ get(paste0("Stat", snakeToCamel(layer_stat(), capFirst = TRUE))) })
  geom_aesthetics <- gg_aesthetics[[geom_type]]
  geom_required_aesthetics <- (stringr::str_split(geom_proto$geom$required_aes, "[|]", simplify=TRUE) %T||% NULL)[,1]

  # Create trigger for this layers update
  triggerAesUpdate <- makeReactiveTrigger()
  observeEvent(layer_selected(), {
    if (layer_selected() == layer_id) {
      triggerAesUpdate$trigger()
    }
  }, ignoreInit = TRUE)

  # MAIN ----

  stat_aesthetics <- reactive({
    setdiff(
      stat_proto()$aesthetics(),
      geom_aesthetics
    )
  })

  # Refactor: This will become more involved when implementing true ggplot2 3.0.0
  #   support of axis choice
  stat_required_aesthetics <- reactive({
    (stringr::str_split(stat_proto()$required_aes, "[|]", simplify=TRUE) %T||% NULL)[,1]
  })

  # Possible refactor:  Probably more efficient to just use insertUI and removeUI for
  #  each aesthetic, rather than rerendering all aesthetics on every stat change.
  #  Also, the "side-effect" in this reactive is making me twitch.
  aesthetics <- reactive({
    triggerAesUpdate$trigger() # Make sure individual aesthetics update as well (probably bad form as side effect)
    reorderElements(c(geom_aesthetics, stat_aesthetics()),
                    orderBy = unique(c(
                      reorderElements(c(geom_required_aesthetics,
                                        stat_required_aesthetics(),
                                        names(stat_additional_defaults[[stringr::str_split(geom_type, "-")[[1]][2]]])),
                                      orderBy = unique(unlist(gg_aesthetics))),
                      unlist(gg_aesthetics))
                    ))
  })

  # _ Aesthetic divs ====
  output$layer_aesthetics <- renderUI({
    req(aesthetics())
    triggerAesUpdate$depend()

    tagList(
      lapply(aesthetics(), function(aes) {
        layerAesUI(id = session$ns(aes))
      })
    )
  })

  # _ load variable subset modules ====
  geom_aes_args <- purrr::map(geom_aesthetics, ~ {
    callModule(module = layerAesServer, id = .,
               reactive({ triggerAesUpdate$depend() }),
               base_layer_mappings[[.]],
               inherit.aes = inherit.aes,
               default_geom_aes = geom_proto$geom$default_aes[[.]],
               default_stat_aes = reactive({ stat_proto()$default_aes[[.]] %||% stat_additional_defaults[[layer_stat()]][[.]] }),
               required = reactive({ . %in% c(stat_required_aesthetics(), geom_required_aesthetics) }),
               dataset = dataset,
               computed_vars = reactive({ stat_computed_vars[[layer_stat()]] }))})

  stat_aes_args <- list()
  observe({
    req(stat_aesthetics())
    stat_aes_args <<- purrr::map(stat_aesthetics(), ~ callModule(module = layerAesServer, id = .,
                                                                 reactive({ triggerAesUpdate$depend() }),
                                                                 base_layer_mappings[[.]],
                                                                 inherit.aes = inherit.aes,
                                                                 default_geom_aes = geom_proto$geom$default_aes[[.]],
                                                                 default_stat_aes = reactive({ stat_proto()$default_aes[[.]] %||% stat_additional_defaults[[layer_stat()]][[.]] }),
                                                                 required = reactive({ . %in% c(stat_required_aesthetics(), geom_required_aesthetics) }),
                                                                 dataset = dataset,
                                                                 computed_vars = reactive({ stat_computed_vars[[layer_stat()]] })))
  })

  # _ process subset arguments ====
  aes_code <- eventReactive(paste(purrr::map(c(geom_aes_args, stat_aes_args), ~ .())), {
    # Evaluate reactives
    args <- purrr::map(c(geom_aes_args, stat_aes_args), ~ .())

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

  return(
    list(
      code = aes_code,
      aesthetics = aesthetics
    )
  )
}
