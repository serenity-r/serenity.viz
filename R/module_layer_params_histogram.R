layerParamsGeomHistogramUI <- function(id) {
  ns <- NS(id)

  uiOutput(ns("params"))
}

layerParamsGeomHistogramServer <- function(input, output, session, base_data) {
  default_args <- list("bins_width" = TRUE,  # Number of bins (TRUE) or binwidth (FALSE)
                       "bins" = 30,          # Default number of bins
                       "binwidth" = NA,      # Width of bins
                       "breaks" = NA,        # Breaks (if not using bins or binwidth)
                       "closed" = TRUE,      # Bins closed on left (TRUE) or right (FALSE)
                       "use_breaks" = FALSE) # Don't use breaks by default

  # Not a built-in input (handled via datatable)
  layer_data <- reactiveValues(breaks = NULL,
                               bin_width_base_data = NULL)

  refreshDT <- makeReactiveTrigger()

  # Only these inputs should trigger an update to the layer code
  reactive_inputs <- reactive({
    paste(input$bins_width,
          input$bins,
          input$binwidth,
          layer_data$breaks,
          input$closed,
          input$use_breaks)
  })

  # Gets range data for bins
  rng <- reactive({
    c(base_data()[1, "xmin"], base_data()[nrow(base_data()), "xmax"])
  })

  output$params <- renderUI({
    isolate({
      if (isTruthy(base_data())) {
        # Set default binwidth
        if (is.na(default_args$binwidth)) {
          default_args$binwidth <- diff(rng())/default_args$bins
        }

        # UI
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
          ),
          checkboxInput(session$ns('use_breaks'),
                        label = "Specify breaks?",
                        value = input[['use_breaks']] %||% default_args[['use_breaks']]),
          wellPanel(
            class = paste(c("histogram_breaks_panel",
                            switch(!(input[['use_breaks']] %||% default_args[['use_breaks']]),
                                   "hidden")), collapse = " "),
            editableTableUI(session$ns('breaks'))
          )
        )
      } else {
        span("Please fix layer error before continuing.")
      }
    })
  })

  # _ Make sure params always update ====
  outputOptions(output, "params", suspendWhenHidden = FALSE)

  breaks <- callModule(module = editableTableServer,
                       id = "breaks",
                       init = reactive({ data.frame(
                         values = c(layer_data$bin_width_base_data$xmin,
                                    layer_data$bin_width_base_data[nrow(layer_data$bin_width_base_data), "xmax"])
                       ) }),
                       refreshDT = refreshDT,
                       unique_values = TRUE,
                       default_from = reactive({ layer_data$bin_width_base_data[1, "xmin"] }),
                       default_to = reactive({ layer_data$bin_width_base_data[nrow(layer_data$bin_width_base_data), "xmax"] }),
                       session = session)

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
  })

  observeEvent(input$use_breaks, {
    if (input$use_breaks) {
      shinyjs::disable("bins")
      shinyjs::disable("binwidth")
      shinyjs::js$addClass("disabled", paste0("#", session$ns("params"), " .SNI-switch .bootstrap-switch"))
      shinyjs::js$removeClass("hidden", '.histogram_breaks_panel')
      layer_data$bin_width_base_data <<- base_data()
      refreshDT$trigger() # Need the trigger for some reason
    } else {
      shinyjs::enable("bins")
      shinyjs::enable("binwidth")
      shinyjs::js$removeClass("disabled", paste0("#", session$ns("params"), " .SNI-switch .bootstrap-switch"))
      shinyjs::js$addClass("hidden", '.histogram_breaks_panel')
    }
  })

  observeEvent(breaks(), {
    layer_data$breaks <- data.frame(
      breaks = breaks()$values
    )
  })

  # This reactive needs to be isolated since we have inputs that don't need
  #   to rerun this code (e.g. buttons in breaks panel).
  geom_params_code <- reactive({
    reactive_inputs()
    isolate({
      processed_geom_params_code <- ''
      if (!is.null(input$bins_width)) {
        args <- default_args[setdiff(names(default_args),
                                     c("bins_width",
                                       "use_breaks",
                                       switch(as.character(input$use_breaks),
                                              "TRUE" = c("binwidth", "bins"),
                                              "FALSE" = c("breaks", ifelse(input$bins_width, "binwidth", "bins")))))]

        processed_geom_params_code <- process_args(args,
                                                   c(reactiveValuesToList(input),
                                                     reactiveValuesToList(layer_data)),
                                                   base_data)
      }
    })

    return(processed_geom_params_code)
  })

  return(geom_params_code)
}
