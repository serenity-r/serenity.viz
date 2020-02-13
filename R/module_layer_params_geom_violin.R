layerParamsGeomViolinUI <- function(id) {
  ns <- NS(id)

  uiOutput(ns("params"))
}

layerParamsGeomViolinServer <- function(input, output, session, base_data) {
  default_args <- list("draw_quantiles" = NA)

  layer_data <- reactiveValues(draw_quantiles = NULL)

  refreshDT <- makeReactiveTrigger()

  output$params <- renderUI({
    isolate({
      bs_accordion(session$ns('quantile_params')) %>%
        bsplus::bs_append(tagList("Quantiles", icon("")),
                          content = wellPanel(
                            class = "violin_quantiles_panel",
                            editableTableUI(session$ns('quantiles'),
                                            refreshIcon = "trash")
                          )
        )
    })
  })
  outputOptions(output, "params", suspendWhenHidden = FALSE)

  quantiles <- callModule(module = editableTableServer,
                          id = "quantiles",
                          refreshDT = refreshDT,
                          unique_values = TRUE,
                          default_from = reactive({ 0.25 }),
                          default_to = reactive({ 0.75 }),
                          default_num = 3,
                          session = session)

  observeEvent(quantiles(), {
    layer_data$draw_quantiles <- data.frame(
      draw_quantiles = quantiles()$values
    )
  })

  geom_params_code <- reactive({
    layer_data$draw_quantiles
    isolate({
      args <- default_args[setdiff(names(default_args),
                                   switch(is.null(layer_data$draw_quantiles) || (nrow(layer_data$draw_quantiles) == 0),
                                          "draw_quantiles"))]

      processed_geom_params_code <- process_args(args,
                                                 c(reactiveValuesToList(input),
                                                   reactiveValuesToList(layer_data)),
                                                 base_data)
    })

    return(processed_geom_params_code)
  })

  return(geom_params_code)
}
