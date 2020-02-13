layerParamsStatBoxplotUI <- function(id) {
  ns <- NS(id)

  tagList(
    uiOutput(ns("params"))
  )
}

layerParamsStatBoxplotServer <- function(input, output, session, base_data) {
  default_args <- list("coef" = 1.5)       # Whisker length as multiple of IQR

  output$params <- renderUI({
    isolate({
      numericInput(session$ns('coef'),
                   label = "Whisker length (x IQR)",
                   value = input[['coef']] %||% default_args[['coef']],
                   min = 0,
                   max = Inf
      )
    })
  })
  outputOptions(output, "params", suspendWhenHidden = FALSE)

  stat_params_code <- reactive({
    processed_stat_params_code <- process_args(default_args, input, NULL)

    return(processed_stat_params_code)
  })

  return(stat_params_code)
}
