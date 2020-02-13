layerParamsStatYdensityUI <- function(id) {
  ns <- NS(id)

  uiOutput(ns("params"))
}

layerParamsStatYdensityServer <- function(input, output, session, base_data) {
  default_args <- list("trim" = TRUE,            # Trim the tails of the violins to the range of the data
                       "scale" = "area",         # Scaling algorithm for violins
                       "kernel" = "gaussian",    # Kernel
                       "adjust" = 1.0,           # A multiplicate bandwidth adjustment
                       "bw_algorithm" = "nrd0",  # The smoothing bandwidth to be used
                       "bw_numeric" = NULL,
                       "bw" = "nrd0",
                       "bw_override" = FALSE)

  layer_data <- reactiveValues(bw = NULL)

  refreshDT <- makeReactiveTrigger()

  reactive_inputs <- reactive({
    paste(input$trim,
          input$scale,
          input$kernel,
          input$adjust,
          input$bw_algorithm,
          input$bs_numeric,
          layer_data$bw
    )
  })

  output$params <- renderUI({
    isolate({
      # UI
      tagList(
        checkboxInput(session$ns('trim'),
                      label = "Trim tails to range of data",
                      value = input[['trim']] %||% default_args[['trim']]),
        selectInput(session$ns('scale'),
                    label = 'Scale',
                    choices = c(
                      "Equal areas" = "area",
                      "Proportional" = "count",
                      "Equal width" = "width"
                    ),
                    selected = input[['scale']] %||% default_args[['scale']]),
        bs_accordion(session$ns('kernel_params')) %>%
          bsplus::bs_append(tagList("Kernel Parameters", icon("")),
                            content = tagList(
                              selectInput(session$ns('kernel'),
                                          label = 'Kernel',
                                          choices = c("Gaussian" = "gaussian",
                                                      "Epanechnikov" = "epanechnikov",
                                                      "Rectangular" = "rectangular",
                                                      "Triangular" = "triangular",
                                                      "Biweight" = "biweight",
                                                      "Cosine" = "cosine",
                                                      "Optcosine" = "optcosine"),
                                          selected = input[['kernel']] %||% default_args[['kernel']]
                              ),
                              selectInput(session$ns('bw_algorithm'),
                                          label = 'Bandwidth',
                                          choices = c("Rule-of-Thumb" = "nrd0",
                                                      "Scott (1992)" = "nrd",
                                                      "Unbiased cross-validation" = "ucv",
                                                      "Biased cross-validation" = "bcv",
                                                      "Sheather & Jones (STE)" = "sj-ste",
                                                      "Sheather & Jones (DPI)" = "sj-dpi"),
                                          selected = input[['bw_algorithm']] %||% default_args[['bw_algorithm']]
                              ),
                              h5('Override bandwidth?'),
                              div(
                                class = "switch-numeric-input",
                                div(
                                  class = "SNI-switch",
                                  shinyWidgets::switchInput(session$ns('bw_override'),
                                                            label = '',
                                                            value = input[['bw_override']] %||% default_args[['bw_override']],
                                                            onLabel = "Yes",
                                                            offLabel = "No"
                                  )
                                ),
                                div(
                                  class = "SNI-numeric",
                                  numericInput(session$ns('bw_numeric'),
                                               label = '',
                                               value = input[['bw_numeric']] %||% default_args[['bw_numeric']],
                                  )
                                )
                              ),
                              sliderInput(session$ns('adjust'),
                                          label = 'Adjustment',
                                          min = 0,
                                          max = 1,
                                          value = input[['adjust']] %||% default_args[['adjust']]
                              )
                            )
          )
      )
    })
  })
  outputOptions(output, "params", suspendWhenHidden = FALSE)

  observeEvent(input$bw_override, {
    if (input$bw_override) {
      shinyjs::disable("bw_algorithm")
      shinyjs::show("bw_numeric")
    } else {
      shinyjs::enable("bw_algorithm")
      shinyjs::hide("bw_numeric")
    }
  })

  # Refactor:  Should just be an observe, yes?
  observeEvent({
    paste(input$bw_override, input$bw_algorithm, input$bw_numeric)
  }, {
    layer_data$bw <- switch(as.character(is.null(input$bw_override) || !input$bw_override),
                            "TRUE" = input$bw_algorithm,
                            "FALSE" = input$bw_numeric)
  })

  observe({
    req(base_data(), !is.null(input$bw_override))
    # Only update numeric bw if not currently overriding
    if (!input$bw_override && nrow(base_data()) > 0) {
      xs <- split(base_data()$y, base_data()$group)
      bws <- switch(input$bw_algorithm,
                    "sj-ste" = ,
                    "sj-dpi" = vapply(xs, "bw.SJ", numeric(1), method = strsplit(input$bw_algorithm, "-")[[1]][2]),
                    vapply(xs, paste0("bw.", input$bw_algorithm), numeric(1)))
      isolate(updateNumericInput(session, "bw_numeric", value = signif(mean(bws), 3)))
    }
  })

  stat_params_code <- reactive({
    reactive_inputs()
    isolate({
      args <- default_args[setdiff(names(default_args), c("bw_override", "bw_algorithm", "bw_numeric"))]

      processed_stat_params_code <- process_args(args,
                                                 c(reactiveValuesToList(input),
                                                   reactiveValuesToList(layer_data)),
                                                 base_data)
    })

    return(processed_stat_params_code)
  })

  return(stat_params_code)
}
