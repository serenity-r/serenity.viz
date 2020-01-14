layerParamsGeomDotplotUI <- function(id) {
  ns <- NS(id)

  uiOutput(ns("params"))
}

layerParamsGeomDotplotServer <- function(input, output, session, base_data) {
  # Ignoring width, origin, and drop (haven't found examples yet of what these really do)
  default_args <- list("method" = "dotdensity",
                       "bw_override" = FALSE,
                       "binwidth" = NA,
                       "binaxis" = "x",
                       "stackdir" = "up",
                       "binpositions" = "bygroup",
                       "stackgroups" = FALSE,
                       "stackratio" = 1.0,
                       "dotsize" = 1.0)

  reactive_inputs <- reactive({
    paste(input$method,
          input$bw_override,
          input$binwidth,
          input$binaxis,
          input$stackdir,
          input$binpositions,
          input$stackgroups,
          input$stackratio,
          input$dotsize
    )
  })

  output$params <- renderUI({
    isolate({
      tagList(
        sliderInput(session$ns('dotsize'),
                    label = 'Relative dot size',
                    min = 0.05,
                    max = 2.0,
                    step = 0.05,
                    value = input[['dotsize']] %||% default_args[['dotsize']]),
        serenity.viz::bs_accordion(session$ns('accordion_params')) %>%
          bsplus::bs_append(tagList("Binning Parameters", icon("")),
                            content = tagList(
                              selectInput(session$ns('method'),
                                          label = 'Binning method',
                                          choices = c("Dot-density" = "dotdensity",
                                                      "Fixed bin widths" = "histodot"),
                                          selected = input[['method']] %||% default_args[['method']]),
                              h5('Specify binwidth?'),
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
                                  numericInput(session$ns('binwidth'),
                                               label = '',
                                               value = input[['binwidth']] %||% default_args[['binwidth']],
                                  )
                                )
                              ),
                              radioButtons(session$ns('binaxis'),
                                           label = "Bin along axis:",
                                           choices = c("x", "y"),
                                           selected = input[['binaxis']] %||% default_args[['binaxis']],
                                           inline = TRUE)
                            )
          ) %>%
          bsplus::bs_append(tagList("Stacking Parameters", icon("")),
                            content = tagList(
                              selectInput(session$ns('stackdir'),
                                          label = 'Stacking direction',
                                          choices = c("Up" = "up",
                                                      "Down" = "down",
                                                      "Center" = "center",
                                                      "Center & Align" = "centerwhole"),
                                          selected = input[['stackdir']] %||% default_args[['stackdir']]),
                              sliderInput(session$ns('stackratio'),
                                          label = 'Stacking ratio',
                                          min = 0.05,
                                          max = 2.0,
                                          step = 0.05,
                                          value = input[['stackratio']] %||% default_args[['stackratio']])
                            )
          ) %>%
          bsplus::bs_append(tagList("Grouping Adjustments", icon("")),
                            content = div(
                              id = session$ns("grouping_adjustments"),
                              div(
                                class = switch(!isTruthy(show_grouping_params()), "hidden"),
                                radioButtons(session$ns('binpositions'),
                                             label = "Group Alignment",
                                             choices = c("Within" = "bygroup",
                                                         "Across" = "all"),
                                             selected = input[['binpositions']] %||% default_args[['binpositions']],
                                             inline = TRUE),
                                div(
                                  class = "inline-switch-with-label",
                                  h5("Group Stacking"),
                                  shinyWidgets::switchInput(session$ns('stackgroups'),
                                                            value = input[['stackgroups']] %||% default_args[['stackgroups']],
                                                            size = "mini",
                                                            inline = TRUE)
                                )
                              ),
                              span(
                                "No grouping present.",
                                class = switch(isTruthy(show_grouping_params()),"hidden")
                              )
                            )
          )
      )
    })
  })
  # _ Make sure params always update ====
  outputOptions(output, "params", suspendWhenHidden = FALSE)

  show_grouping_params <- eventReactive(base_data(), {
    !is.null(base_data()) && (length(unique(base_data()$group)) > 1)
  }, ignoreNULL = FALSE)

  observe({
    if (isTruthy(show_grouping_params())) {
      shinyjs::js$removeClass("hidden", paste0('#', session$ns("grouping_adjustments"), ' > div'))
      shinyjs::js$addClass("hidden", paste0('#', session$ns("grouping_adjustments"), ' > span'))
    } else {
      shinyjs::js$addClass("hidden", paste0('#', session$ns("grouping_adjustments"), ' > div'))
      shinyjs::js$removeClass("hidden", paste0('#', session$ns("grouping_adjustments"), ' > span'))
    }
  })

  observeEvent(input$bw_override, {
    if (input$bw_override) {
      shinyjs::enable("binwidth")
    } else {
      shinyjs::disable("binwidth")
    }
  })

  observe({
    if (!isTruthy(input$bw_override)) {
      updateNumericInput(session, "binwidth", value = max(base_data()$binwidth))
    }
  })

  geom_params_code <- reactive({
    reactive_inputs()
    isolate({
      args <- default_args[setdiff(names(default_args),
                                   c("bw_override",
                                     switch(!isTruthy(input$bw_override), "binwidth")))]

      processed_geom_params_code <- process_args(args, input, base_data)
    })

    return(processed_geom_params_code)
  })

  return(geom_params_code)
}
