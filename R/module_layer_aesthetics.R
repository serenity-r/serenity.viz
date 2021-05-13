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
        uiOutput(ns("layer_aesthetics_ui"),
                 class = "layer-aesthetics")
      )
    )
}

#' Server for layer aesthetics module
#'
#' @param id ID of layer aesthetics submodule
#' @param layer_id Layer ID
#' @param geom Geom
#' @param selected_layer Reactive value of ID of currently selected layer
#' @param base_layer_aesthetics  Need base layer aesthetic mappings for inheritance
#' @param dataset Dataset
#' @param inherit_aes Reactive: Is this aesthetic inheritable?
#' @param layer_stat Reactive value of currently selected layer stat
#'
#' @importFrom magrittr %>%
#' @import shiny ggplot2
#'
#' @export
layerAestheticsServer <- function(id, layer_id, geom, selected_layer,
                                  base_layer_aesthetics, dataset, inherit_aes,
                                  layer_stat) {
  moduleServer(
    id,
    function(input, output, session) {
      geom_proto <- eval(parse(text=paste0(stringr::str_replace(geom, "-", "_"), "()")))
      stat_proto <- reactive({ get(paste0("Stat", snakeToCamel(layer_stat(), capFirst = TRUE))) })
      geom_aesthetics <- gg_aesthetics[[geom]]
      geom_required_aesthetics <- (stringr::str_split(geom_proto$geom$required_aes, "[|]", simplify=TRUE) %T||% NULL)[,1]
      if (length(geom_required_aesthetics) == 0) {
        geom_required_aesthetics <- c("x", "y", "group")
      }

      stat_aesthetics <- reactive({
        setdiff(
          stat_proto()$aesthetics(),
          geom_aesthetics
        )
      })

      # Refactor: This will become more involved when implementing true ggplot2 3.0.0
      #   support of axis choice
      required_aesthetics <- reactive({
        unique(
          c(geom_required_aesthetics,
            (stringr::str_split(stat_proto()$required_aes, "[|]", simplify=TRUE) %T||% NULL)[,1])
        )
      })

      # Create trigger for this layers update
      triggerAesUpdate <- makeReactiveTrigger()

      aesthetics <- reactive({
        # triggerAesUpdate$trigger() # Make sure individual aesthetics update as well (probably bad form as side effect)
        reorderElements(c(geom_aesthetics, stat_aesthetics()),
                        orderBy = unique(c(
                          reorderElements(c(required_aesthetics(),
                                            names(stat_additional_defaults[[stringr::str_split(geom, "-")[[1]][2]]])),
                                          orderBy = unique(unlist(gg_aesthetics))),
                          unlist(gg_aesthetics))
                        ))
      })

      layer_aesthetics <- reactiveValues()

      # _ Aesthetic divs ====
      output$layer_aesthetics_ui <- renderUI({
        # Problems if this req(...) is triggerAesUpdate$depend()
        req(selected_layer() == layer_id)

        isolate({
          tagList(
            shinyWidgets::pickerInput(
              inputId = session$ns('layer_aesthetics_chooser'),
              label = "Aesthetics",
              choices = keepTruthy(purrr::map(all_aesthetics, ~ { reorderElements(intersect(aesthetics(), .), unique(unlist(all_aesthetics))) })),
              multiple = TRUE,
              selected = input$layer_aesthetics_chooser %||% required_aesthetics(),
              options = list(
                size = 6,
                `live-search` = TRUE,
                `dropup-auto` = FALSE
              )
            ),
            div(class = "aesthetics",
                tagList(
                  purrr::map(
                    intersect( # Only show pre-rendered aesthetics
                      input$layer_aesthetics_chooser %||% required_aesthetics(),
                      names(layer_aesthetics)
                    ), ~ {
                      layerAesUI(session$ns(.), aesthetic = .)
                    })
                )
            )
          )
        })
      })

      # MAIN ----

      observeEvent(selected_layer(), {
        if (selected_layer() == layer_id) {
          triggerAesUpdate$trigger()
        }
      }, ignoreInit = TRUE)

      observeEvent(input$layer_aesthetics_chooser, {
        # Adding new layers
        # Note: Uh, for loop doesn't work here - duplicates y???
        purrr::walk(setdiff(input$layer_aesthetics_chooser, names(layer_aesthetics)), ~ {
          # Update layer module output reactives - create only once!
          # Where to insert?
          # ind <- which(. == input$layer_aesthetics_chooser) - 1
          # selector <- paste0('#', session$ns('layer_aesthetics'),
          #                    ifelse())
          insertUI(
            selector = paste0('#', session$ns('layer_aesthetics_ui'), ' > .aesthetics'),
            where = "beforeEnd",
            ui = layerAesUI(session$ns(.), aesthetic = .)
          )
          layer_aesthetics[[.]] <- layerAesServer(
            id = .,
            geom = geom,
            aesthetic = .,
            base_layer_stages = base_layer_aesthetics[[.]],
            inherit_aes = inherit_aes,
            default_geom_aes = geom_proto$geom$default_aes[[.]],
            default_stat_aes = reactive({ stat_proto()$default_aes[[.]] %||% stat_additional_defaults[[layer_stat()]][[.]] }),
            required = reactive({ . %in% c(stat_required_aesthetics(), geom_required_aesthetics) }),
            dataset = dataset,
            computed_vars = reactive({ stat_computed_vars[[layer_stat()]] }),
            aesthetics = aesthetics,
            aesUpdateDependency = triggerAesUpdate$depend
          )
        })

        purrr::walk(setdiff(names(layer_aesthetics), input$layer_aesthetics_chooser), ~ {
          layer_aesthetics[[.]] <- NULL
          removeUI(paste0('#', session$ns('layer_aesthetics_ui'), ' .aesthetics > div[data-aesthetic="', ., '"]'))
        })
      }, ignoreInit = TRUE)

      # _ process subset arguments ====
      aes_code <- reactive({
        # Evaluate reactives
        args <- purrr::map(reactiveValuesToList(layer_aesthetics), ~ { .$code() })

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
      })

      return(
        list(
          code = aes_code,
          aesthetics = aesthetics
        )
      )
    }
  )
}

