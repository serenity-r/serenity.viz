layerParamsStatBinUI <- function(id) {
  ns <- NS(id)

  uiOutput(ns("params"))
}

# Refactor: Duplicate plotting when changing between bins and binwidth
layerParamsStatBinServer <- function(input, output, session, base_data) {
  default_args <- list("bins_width" = TRUE,  # Number of bins (TRUE) or binwidth (FALSE)
                       "bins" = 30,          # Default number of bins
                       "binwidth" = NA,      # Width of bins
                       "breaks" = NA,        # Breaks (if not using bins or binwidth)
                       "closed" = TRUE,      # Bins closed on left (TRUE) or right (FALSE)
                       "pad" = FALSE,
                       "use_breaks" = FALSE) # Don't use breaks by default

  # Not a built-in input (handled via datatable)
  layer_data <- reactiveValues(breaks = NULL,
                               bin_width_base_data = NULL)

  refreshDT <- makeReactiveTrigger()
  initBreaks <- makeReactiveTrigger()

  # Only these inputs should trigger an update to the layer code
  # Refactor: Probably don't need to do this anymore now that the
  #  editable table is a module
  reactive_inputs <- reactive({
    paste(input$bins_width,
          input$bins,
          input$binwidth,
          layer_data$breaks,
          input$closed,
          input$pad,
          input$use_breaks)
  })

  # Gets range data for bins
  rng <- reactive({
    c(base_data()[1, "xmin"], base_data()[nrow(base_data()), "xmax"])
  })

  # Avoid NA warning
  observeEvent(rng(), {
    default_args$binwidth <- diff(rng())/default_args$bins
    if (!isTruthy(input$binwidth)) {
      updateNumericInput(session, 'binwidth',
                         value = default_args$binwidth,
                         max = diff(rng()))
    }
  }, once = TRUE)

  output$params <- renderUI({
    isolate({
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
                         max = switch(as.character(isTruthy(rng())),
                                      "TRUE" = diff(rng()),
                                      "FALSE" = NA))
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
        div(
          class = "inline-switch-no-label",
          tagList(
            span("Pad with zero bins:"),
            shinyWidgets::switchInput(session$ns('pad'),
                                      value = input[['pad']] %||% default_args[['pad']],
                                      onLabel = "yes",
                                      offLabel = "no",
                                      labelWidth = "0",
                                      size = "mini",
                                      inline = TRUE)
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
    })
  })
  outputOptions(output, "params", suspendWhenHidden = FALSE)

  # Need to make sure bin_width_base_data is initialized before calling module
  breaks <- reactive({ NULL })
  observeEvent(layer_data$bin_width_base_data, {
    breaks <<- callModule(module = editableTableServer,
                          id = "breaks",
                          init = reactive({
                            c(layer_data$bin_width_base_data$xmin,
                              layer_data$bin_width_base_data[nrow(layer_data$bin_width_base_data), "xmax"])
                          }),
                          refreshDT = refreshDT,
                          unique_values = TRUE,
                          default_from = reactive({ layer_data$bin_width_base_data[1, "xmin"] }),
                          default_to = reactive({ layer_data$bin_width_base_data[nrow(layer_data$bin_width_base_data), "xmax"] }),
                          session = session)
    initBreaks$trigger() # Since updating breaks reactive, need to trigger
  }, ignoreNULL = TRUE, once = TRUE)

  # Initialize and update breaks
  observe({
    initBreaks$depend()
    layer_data$breaks <- data.frame(
      breaks = switch(length(breaks()$values) != 0, breaks()$values) # Editable table module returns numeric(0)
    )
  })

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
  }, ignoreInit = TRUE)

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

  # This reactive needs to be isolated since we have inputs that don't need
  #   to rerun this code (e.g. buttons in breaks panel).
  stat_params_code <- reactive({
    reactive_inputs()
    isolate({
      processed_stat_params_code <- ''
      if (!is.null(input$bins_width)) {
        args <- default_args[setdiff(names(default_args),
                                     c("bins_width",
                                       "use_breaks",
                                       switch(as.character(input$use_breaks),
                                              "TRUE" = c("binwidth", "bins"),
                                              "FALSE" = c("breaks", ifelse(input$bins_width, "binwidth", "bins")))))]

        processed_stat_params_code <- process_args(args,
                                                   c(reactiveValuesToList(input),
                                                     reactiveValuesToList(layer_data)),
                                                   base_data)
      }
    })

    return(processed_stat_params_code)
  })

  return(stat_params_code)
}
