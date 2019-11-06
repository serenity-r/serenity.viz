layerParamsGeomSmoothUI <- function(id) {
  ns <- NS(id)

  uiOutput(ns("params"))
}

layerParamsGeomSmoothServer <- function(input, output, session, ggdata) {
  default_args <- list("method" = "auto",   # loess or gam
                       "se" = TRUE,         # Show confidence bands
                       "level" = 0.95,      # Confidence level
                       "fullrange" = FALSE) # Span full range of plot?

  output$params <- renderUI({
    isolate({
      tagList(
        selectInput(session$ns('method'),
                    label = 'Regression type',
                    choices = c("Auto" = "auto",
                                "Linear regression" = "lm",
                                "Generalized linear model" = "glm",
                                "Generalized additive model" = "gam",
                                "LOESS" = "loess"),
                    selected = input[['method']] %||% default_args[['method']]),
        div(
          class = "switch-numeric-input",
          div(
            class = "SNI-switch",
            shinyWidgets::switchInput(session$ns('se'),
                                      label = 'Confidence bands?',
                                      value = input[['se']] %||% default_args[['se']],
                                      onLabel = "Yes",
                                      offLabel = "No"
            )
          ),
          div(
            class = paste0("SNI-numeric", ifelse(default_args[['se']], "", " hidden")),
            numericInput(session$ns('level'),
                         label = 'Level',
                         value = input[['level']] %||% default_args[['level']],
                         min = 0,
                         max = 1)
          )
        ),
        checkboxInput(session$ns('fullrange'),
                      label = 'Span full range of plot?',
                      value = input[['fullrange']] %||% default_args[['fullrange']])
      )
    })
  })

  # _ Make sure params always update ====
  outputOptions(output, "params", suspendWhenHidden = FALSE)

  observeEvent(input$se, {
    if (input$se) {
      shinyjs::js$removeClass("hidden", paste0('#', session$ns("params"), ' .SNI-numeric'))
    } else {
      shinyjs::js$addClass("hidden", paste0('#', session$ns("params"), ' .SNI-numeric'))
    }
  })

  geom_params_code <- reactive({
    processed_geom_params_code <- process_args(default_args, input, ggdata)

    return(processed_geom_params_code)
  })

  return(geom_params_code)
}
