layerParamsGeomDotplotUI <- function(id) {
  ns <- NS(id)

  uiOutput(ns("params"))
}

layerParamsGeomDotplotServer <- function(input, output, session, ggdata) {
  default_args <- list("method" = "dotdensity")

  output$params <- renderUI({
    isolate({
      selectInput(session$ns('method'),
                  label = 'Binning method',
                  choices = c("Dot-density" = "dotdensity",
                              "Fixed bin widths" = "histodot"),
                  selected = input[['method']] %||% default_args[['method']])
    })
  })

  # _ Make sure params always update ====
  outputOptions(output, "params", suspendWhenHidden = FALSE)

  geom_params_code <- reactive({
    processed_geom_params_code <- process_args(default_args, input, ggdata)

    return(processed_geom_params_code)
  })

  return(geom_params_code)
}
