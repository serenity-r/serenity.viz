layerParamsGeomHistogramUI <- function(id) {
  ns <- NS(id)

  uiOutput(ns("params"))
}

layerParamsGeomHistogramServer <- function(input, output, session, ggdata) {
  default_args <- list("bins_width" = TRUE, # Number of bins (TRUE) or binwidth (FALSE)
                       "bins" = 30,         # Default number of bins
                       "binwidth" = NA,     # Width of bins
                       "closed" = TRUE)     # Bins closed on left (TRUE) or right (FALSE)

  # Need a reactive trigger to fix a shinyWidgets bug
  refreshWidget <- makeReactiveTrigger()

  # Gets range data for bins
  rng <- reactive({
    req(ggdata())
    ggdata()$scales$x$range$range
  })

  output$params <- renderUI({
    refreshWidget$depend()
    if (isTruthy(ggdata())) {
      isolate({
        tagList(
          div(
            class = "switch-numeric-input",
            div(
              class = "SNI-switch",
              shinyWidgets::switchInput(session$ns('bins_width'),
                                        label = 'Bins',
                                        value = input[['bins_width']] %||% default_args[['bins_width']],
                                        onLabel = "Number",
                                        offLabel = "Width",
                                        offStatus = "primary"
              )
            ),
            div(
              class = paste0("SNI-numeric bins", ifelse(default_args[['bins_width']], "", " hidden")),
              numericInput(session$ns('bins'),
                           label = '',
                           value = input[['bins']] %||% default_args[['bins']],
                           min = 1,
                           max = Inf)
            ),
            div(
              class = paste0("SNI-numeric binwidth", ifelse(default_args[['bins_width']], " hidden", "")),
              numericInput(session$ns('binwidth'),
                           label = '',
                           value = input[['binwidth']] %||% default_args[['binwidth']],
                           min = 0,
                           max = diff(rng())) # Might cause problems since isolated - wait and see
            )
          ),
          div(
            class = "inline-switch-no-label",
            tagList(
              span("Bins are"),
              shinyWidgets::switchInput(session$ns('closed'),
                                        value = input[['closed']] %||% default_args[['closed']],
                                        onLabel = "left",
                                        offLabel = "right",
                                        offStatus = "primary",
                                        labelWidth = "0",
                                        size = "mini",
                                        inline = TRUE),
              span("closed.")
            )
          )
        )
      })
    } else {
      span("Please fix layer error before continuing.")
    }
  })

  # _ Make sure params always update ====
  outputOptions(output, "params", suspendWhenHidden = FALSE)

  observeEvent(input$bins_width, {
    if (input$bins_width && isTruthy(input$binwidth)) {
      shinyjs::js$removeClass("hidden", paste0('#', session$ns("params"), ' .SNI-numeric.bins'))
      shinyjs::js$addClass("hidden", paste0('#', session$ns("params"), ' .SNI-numeric.binwidth'))
      updateNumericInput(session, 'bins',
                         value = round(diff(rng())/input$binwidth))
    } else
      if (!input$bins_width && isTruthy(input$bins)) {
      shinyjs::js$addClass("hidden", paste0('#', session$ns("params"), ' .SNI-numeric.bins'))
      shinyjs::js$removeClass("hidden", paste0('#', session$ns("params"), ' .SNI-numeric.binwidth'))
      updateNumericInput(session, 'binwidth',
                         value = diff(rng())/input$bins,
                         max = diff(rng()))
    }
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  # Fire one event to refresh shinyWidget (current bug in shinyWidgets)
  observeEvent(input$bins_width, {
    refreshWidget$trigger()
  }, once = TRUE)

  geom_params_code <- reactive({
    req(!is.null(input$bins_width))
    args <- default_args[setdiff(names(default_args),
                                 c("bins_width",
                                   switch(input$bins_width, "binwidth", "bins")))]

    processed_geom_params_code <- process_args(args, input, ggdata)

    return(processed_geom_params_code)
  })

  return(geom_params_code)
}
