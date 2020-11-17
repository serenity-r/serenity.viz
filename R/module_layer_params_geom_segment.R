layerParamsGeomSegmentUI <- function(id) {
  ns <- NS(id)

  uiOutput(ns("params"))
}

# Refactor: Unnecessary plotting occurs sometimes on base_data change
layerParamsGeomSegmentServer <- function(input, output, session, base_data) {
  default_args <- list("arrow" = NULL)

  layer_data <- reactiveValues(arrow = NULL)

  reactive_inputs <- reactive({
    paste(
      layer_data$arrow
    )
  })

  output$params <- renderUI({
    isolate({
      tagList(
        arrowUI(session$ns("myarrow"))
      )
    })
  })
  outputOptions(output, "params", suspendWhenHidden = FALSE)

  arrow <- callModule(arrowServer, "myarrow")

  # Update arrow input
  observeEvent(arrow(), {
    layer_data$arrow <- arrow()
  })

  geom_params_code <- reactive({
    reactive_inputs()
    isolate({
      processed_geom_params_code <- process_args(default_args,
                                                 c(reactiveValuesToList(input),
                                                   reactiveValuesToList(layer_data)),
                                                 NULL)
    })

    return(processed_geom_params_code)
  })

  return(geom_params_code)
}
