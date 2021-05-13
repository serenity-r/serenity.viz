#' Module UI for all layers
#'
#' @param id  Layers tab ID
#' @param session Shiny user session
#'
#' @return UI for layer
#'
layersUI <- function(id, session = getDefaultReactiveDomain()) {
  ns <- NS(id)

  tagList(
    widgetHeader(
      div(
        style = "display: flex; flex-direction: row; justify-content: flex-end;",
        actionButton(
          inputId = ns("add-layer-button"),
          label = "Add Layer",
          icon = icon("plus"),
          style = "padding: 0; display: none;",
          class = "add-layer"
        ),
        actionButton(
          inputId = ns("remove-layer"),
          label = "",
          icon = icon("minus"),
          style = "border: transparent; padding: 0;"
        ),
        prettyToggle(
          inputId = ns("layer-chooser"),
          label_on = "",
          label_off = "",
          status_on = "default",
          status_off = "default",
          outline = TRUE,
          plain = TRUE,
          icon_on = icon("times"),
          icon_off = icon("plus"),
          inline = TRUE
        )
      )
    ),
    widgetBody(
      class = "widget-geoms-and-layers",
      uiOutput(ns("widget-layers-body"))
    )
  )
}

#' Server for all layers module
#'
#' @param id ID of layers module
#' @param dataset Dataset
#'
#' @importFrom magrittr %>%
#' @import shiny
#'
layersServer <- function(id, dataset) {
  moduleServer(
    id,
    function(input, output, session) {
      # Server code for layer dropzones
      dndselectr::dropZoneServer(session, "layers", layerUI)
      dndselectr::dropZoneServer(session, "base-layer", layerUI)

      # This stores returned reactives from layer modules
      layer_modules <- reactiveValues()

      output$`widget-layers-body` <- renderUI({
        tagList(
          div(
            class = "layers-wrapper",
            div(
              class = "base-layer",
              dndselectr::dropZoneInput(
                session$ns("base_layer"),
                class = "layers",
                choices = list(
                  'geom-blank' = layerUI("geom-blank")
                ),
                server = layerUI,
                presets = list(values = "geom-blank-ds-1",
                               locked = "geom-blank-ds-1",
                               freeze = "geom-blank-ds-1"),
                multivalued = TRUE,
                selectable = TRUE
              )
            ),
            dndselectr::dropZoneInput(
              session$ns("layers"),
              class = "layers",
              choices = sapply(setdiff(plots$id, "geom-blank"), function(plot_id) { layerUI(plot_id) }, simplify = FALSE, USE.NAMES = TRUE),
              server = layerUI,
              placeholder = "Add a layer",
              multivalued = TRUE,
              selectable = TRUE,
              selectOnDrop = TRUE,
              removeOnSpill = TRUE
            )
          ),
          div(
            class = "layer-chooser-wrapper",
            style = "display: none;",
            tagList(
              selectInput(session$ns("plot_type"),
                          label = "Plots:",
                          choices = c("All" = "all",
                                      "One variable" = "one",
                                      "Two variable" = "two",
                                      "Primitives" = "primitives"),
                          selected = "all"),
              dndselectr::dropZoneInput(session$ns("ds-layer-chooser"),
                                        choices = sapply(setdiff(plots$id, "geom-blank"), function(plot) { layerChoiceUI(plot) }, simplify = FALSE),
                                        class = "layer-chooser",
                                        flex = TRUE,
                                        selectable = TRUE,
                                        direction = "horizontal",
                                        presets = list(values = setdiff(plots$id, "geom-blank"),
                                                       locked = setdiff(plots$id, "geom-blank"))
              )
            )
          )
        )
      })

      observeEvent(input$plot_type, {
        if (input$plot_type == "all") {
          filtered_plots <- plots$id
        } else {
          filtered_plots <- filter(plots,
                                   data_dim == case_when(
                                     input$plot_type == "one" ~ 1,
                                     input$plot_type == "two" ~ 2,
                                     input$plot_type == "primitives" ~ 0
                                   ))$id
        }
        filtered_plots <- setdiff(filtered_plots, "geom-blank")
        dndselectr::updateDropZoneInput(session, "ds-layer-chooser",
                                        presets = list(values = filtered_plots,
                                                       locked = filtered_plots)
        )
      })

      observeEvent(input$`layer-chooser`, {
        dndselectr::unselect(session, "ds-layer-chooser")
        if (input$`layer-chooser`) {
          # Toggle header views
          shinyjs::js$myhide(paste0('#', session$ns("remove-layer")))

          # Toggle body views
          shinyjs::js$myhide('.layers-wrapper')
          shinyjs::js$myshow('.layer-chooser-wrapper')
        } else {
          # Toggle header views
          shinyjs::js$myshow(paste0('#', session$ns("remove-layer")))
          shinyjs::js$myhide(paste0('#', session$ns("add-layer-button")))

          # Toggle body views
          shinyjs::js$myshow('.layers-wrapper')
          shinyjs::js$myhide('.layer-chooser-wrapper')
        }
      })

      observeEvent(input$`ds-layer-chooser_selected`, {
        if (!is.null(input$`ds-layer-chooser_selected`)) {
          shinyjs::js$myshow(paste0('#', session$ns("add-layer-button")))
        }
      })

      # The next two observe events handle selection of layers
      observeEvent(input$base_layer_selected, {
        if (!is.null(input$layers_selected)) {
          dndselectr::unselect(session, "layers")
        }
      }, ignoreInit = TRUE)

      observeEvent(input$layers_selected, {
        if (!is.null(input$base_layer_selected) && !is.null(input$layers_selected)) {
          dndselectr::unselect(session, "base_layer")
        } else if (is.null(input$base_layer_selected) && is.null(input$layers_selected)) {
          dndselectr::select(session, "geom-blank-ds-1", "base_layer")
        }
      }, ignoreNULL = FALSE, ignoreInit = TRUE)

      observeEvent(input$`add-layer-button`, {
        shinyWidgets::updatePrettyToggle(session, "layer-chooser", value = FALSE)
        dndselectr::appendToDropzone(session, input$`ds-layer-chooser_selected`, "layers")
      })

      observeEvent(input$`remove-layer`, {
        if (!is.null(input$layers_selected)) {
          dndselectr::removeSelected(session, "layers")
        }
      })

      all_layers <- eventReactive(input$layers, {
        c(input$base_layer, input$layers)
      }, ignoreNULL = FALSE, ignoreInit = TRUE)

      selected_layer <- eventReactive(input$layers_selected, {
        input$layers_selected %||% "geom-blank-ds-1"
      }, ignoreNULL = FALSE, ignoreInit = TRUE)

      # Get the names of the visible layers
      visible_layers <- eventReactive(paste(all_layers(), input$layers_invisible), {
        setdiff(all_layers(), input$layers_invisible)
      }, ignoreInit = TRUE)

      # Creates list of inputs for all base layer aesthetic mapping stages
      # Needed for layer inheritance
      get_base_layer_aesthetics <- function(prefix = 'geom-blank-ds-1-aesthetics') {
        geom_blank_inputs <- as.list(paste0(prefix, '-', gg_aesthetics[["geom-blank"]]))
        names(geom_blank_inputs) <- gg_aesthetics[["geom-blank"]]
        return(
          geom_blank_inputs %>%
            purrr::map(~ list(
              start = list(
                mapping = reactive({ input[[paste0(.,'-mapping-start-mapping')]] }),
                custom_mapping = reactive({ input[[paste0(.,'-mapping-start-custom_mapping-custom_text')]] %||% '' }),
                custom_toggle = reactive({ input[[paste0(.,'-mapping-start-custom_toggle')]] %||% FALSE })
              ),
              after_scale = list(
                mapping = reactive({ input[[paste0(.,'-mapping-after_scale-mapping')]] }),
                custom_mapping = reactive({ input[[paste0(.,'-mapping-after_scale-custom_mapping-custom_text')]] %||% '' }),
                custom_toggle = reactive({ input[[paste0(.,'-mapping-after_scale-custom_toggle')]] %||% FALSE })
              )
            )))
      }

      # Update layer module output reactives - create only once!
      observeEvent(all_layers(), {
        # Adding new layers
        purrr::map(setdiff(all_layers(), names(layer_modules)), ~ {
          layer_modules[[.]] <- layerServer(id = .,
                                            selected_layer,
                                            get_base_layer_aesthetics(),
                                            dataset = dataset,
                                            ggbase = switch(as.character(. != "geom-blank-ds-1"),
                                                            "TRUE" = layer_modules[["geom-blank-ds-1"]]$code,
                                                            "FALSE" = reactive({ NULL }))
          )
        })

        # Remove old layers
        purrr::map(setdiff(names(layer_modules), all_layers()), ~ { layer_modules[[.]] <- NULL })
      }, priority = 1) # Needs to happen before layer_code reactive

      # Get layer code
      layer_code <- reactive({
        req(visible_layers(),
            purrr::map(reactiveValuesToList(layer_modules)[visible_layers()], ~ .$code()))
        paste(purrr::map(reactiveValuesToList(layer_modules)[visible_layers()], ~ .$code()), collapse = "+\n")
      })

      return(
        list(
          code = layer_code,
          selected_layer = selected_layer,
          selected_stat = reactive({ layer_modules[[selected_layer()]]$stat() }),
          aesthetics = reactive({ layer_modules[[selected_layer()]]$aesthetics() })
        )
      )
    }
  )
}
