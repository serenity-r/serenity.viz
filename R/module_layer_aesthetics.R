#' UI for layer aesthetics module
#'
#' @param id  Layer ID
#' @param name Layer name
#'
#' @return UI for layer
#'
#' @export
layerAestheticsUI <- function(id, name) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tagList(
      widgetHeader(
        span(em(paste("Layer:", name)), # ifelse(geom == "geom-blank", "Base Layer", plot_name))),
             style="float:right; margin-right:10px")
      ),
      widgetBody(
        uiOutput(ns("layer_aesthetics"),
                 class = "layer-aesthetics")
      )
    )
}

#' Server for layer aesthetics module
#'
#' @param id ID of layer aesthetics submodule
#' @param layer_id Layer ID
#' @param geom Geom
#' @param layer_selected Reactive value of ID of currently selected layer
#' @param base_layer_stages  Need base layer aesthetic mappings for inheritance
#' @param dataset Dataset
#' @param inherit_aes Reactive: Is this aesthetic inheritable?
#' @param layer_stat Reactive value of currently selected layer stat
#'
#' @importFrom magrittr %>%
#' @import shiny ggplot2
#'
#' @export
layerAestheticsServer <- function(id, layer_id, geom, layer_selected,
                                  base_layer_stages, dataset, inherit_aes,
                                  layer_stat) {
  moduleServer(
    id,
    function(input, output, session) {
      geom_proto <- eval(parse(text=paste0(stringr::str_replace(geom, "-", "_"), "()")))
      stat_proto <- reactive({ get(paste0("Stat", snakeToCamel(layer_stat(), capFirst = TRUE))) })
      geom_aesthetics <- gg_aesthetics[[geom]]
      geom_required_aesthetics <- (stringr::str_split(geom_proto$geom$required_aes, "[|]", simplify=TRUE) %T||% NULL)[,1]

      # Create trigger for this layers update
      triggerAesUpdate <- makeReactiveTrigger()
      observeEvent(layer_selected(), {
        if (layer_selected() == layer_id) {
          triggerAesUpdate$trigger()
        }
      }, ignoreInit = TRUE)

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
        cat("(Re)creating aesthetics\n")
        reorderElements(c(geom_aesthetics, stat_aesthetics()),
                        orderBy = unique(c(
                          reorderElements(c(geom_required_aesthetics,
                                            stat_required_aesthetics(),
                                            names(stat_additional_defaults[[stringr::str_split(geom, "-")[[1]][2]]])),
                                          orderBy = unique(unlist(gg_aesthetics))),
                          unlist(gg_aesthetics))
                        ))
      })

      # _ Aesthetic divs ====
      output$layer_aesthetics <- renderUI({
        req(aesthetics())
        cat("Rendering aesthetic UIs...")
        triggerAesUpdate$depend()

        stuff <- tagList(
          lapply(aesthetics(), function(aes) {
            layerAesUI(id = session$ns(aes))
          })
        )
        cat("done!\n")
        stuff
      })

      # MAIN ----

      # _ load variable subset modules ====
      withProgress(message = 'Rendering geom aesthetics', value = 0, {
        geom_aes_args <- purrr::map(geom_aesthetics, ~ {
          # aesUpdateDependency = reactive({ triggerAesUpdate$depend() })
          incProgress(1/length(geom_aesthetics), detail = .)
          layerAesServer(
            id = .,
            geom = geom,
            aesthetic = .,
            base_layer_stages = switch(geom != "geom-blank", base_layer_stages[[.]]),
            inherit_aes = inherit_aes,
            default_geom_aes = geom_proto$geom$default_aes[[.]],
            default_stat_aes = reactive({ stat_proto()$default_aes[[.]] %||% stat_additional_defaults[[layer_stat()]][[.]] }),
            required = reactive({ . %in% c(stat_required_aesthetics(), geom_required_aesthetics) }),
            dataset = dataset,
            computed_vars = reactive({ stat_computed_vars[[layer_stat()]] }),
            aesthetics = aesthetics
          )
        })
      })

      stat_aes_args <- list()
      observe({
        req(stat_aesthetics())
        cat("Loading stat aesthetic servers...\n")
        stat_aes_args <<- purrr::map(stat_aesthetics(), ~ {
          # aesUpdateDependency = reactive({ triggerAesUpdate$depend() })
          cat(paste(., "\n"))
          layerAesServer(
            id = .,
            geom = geom,
            aesthetic = .,
            base_layer_stages = switch(geom != "geom-blank", base_layer_stages[[.]]),
            inherit_aes = inherit_aes,
            default_geom_aes = geom_proto$geom$default_aes[[.]],
            default_stat_aes = reactive({ stat_proto()$default_aes[[.]] %||% stat_additional_defaults[[layer_stat()]][[.]] }),
            required = reactive({ . %in% c(stat_required_aesthetics(), geom_required_aesthetics) }),
            dataset = dataset,
            computed_vars = reactive({ stat_computed_vars[[layer_stat()]] }),
            aesthetics = aesthetics
          )
        })
      })

      # _ process subset arguments ====
      # Refactor: Why is this an eventReactive??
      aes_code <- eventReactive(paste(purrr::map(c(geom_aes_args,
                                                   stat_aes_args), ~ .$code())), {
        cat("Rendering code...\n")
        # Evaluate reactives
        args <- purrr::map(c(geom_aes_args, stat_aes_args), ~ .$code())

        # Pull out the filter and mutate elements
        mapping_args <- unlist(purrr::map(args, "mapping"))
        value_args <- unlist(purrr::map(args, "value"))

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
  )
}

