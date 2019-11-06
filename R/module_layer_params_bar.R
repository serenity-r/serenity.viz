layerParamsGeomBarUI <- function(id) {
  ns <- NS(id)

  uiOutput(ns("params"))
}

layerParamsGeomBarServer <- function(input, output, session, ggdata) {
  default_args <- list("width" = 0.9)

  output$params <- renderUI({
    if (isTruthy(ggdata())) {
      isolate({
        sliderInput(session$ns("width"),
                    label = "Bar Width:",
                    value = input[["width"]] %||% default_args[["width"]],
                    min = 0,
                    max = 1,
                    step = 0.05)
      })
    }
  })

  # _ Make sure params always update ====
  outputOptions(output, "params", suspendWhenHidden = FALSE)

  geom_params_code <- reactive({
    processed_geom_params_code <- process_args(default_args, input, ggdata, modify_geom_bar_args)

    return(processed_geom_params_code)
  })

  return(geom_params_code)
}

modify_geom_bar_args <- function(param, value, data) {
  return(
    switch(param,
           "width" = value*resolution(data$x, zero = FALSE),
           value
    )
  )
}
