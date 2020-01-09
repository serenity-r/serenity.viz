layerParamsGeomRugUI <- function(id) {
  ns <- NS(id)

  uiOutput(ns("params"))
}

layerParamsGeomRugServer <- function(input, output, session, base_data) {
  # Reactive since sides defaults changes based on data
  default_args <- reactiveValues("sides" = c("b", "l"))

  # Need to hijack sides input as shinyWidgets returns NULL instead
  #   of character(0) when nothing is selected
  layer_data <- reactiveValues(sides = character(0))

  reactive_inputs <- reactive({
    paste(
      layer_data$sides,
      default_args$sides
    )
  })

  output$params <- renderUI({
    if (isTruthy(base_data())) {
      isolate({
        shinyWidgets::checkboxGroupButtons(
          session$ns('sides_original'),
          label = "Which sides?",
          choices = c(switch(!is.null(base_data()$x),
                             c("Bottom" = "b",
                               "Top" = "t")),
                      switch(!is.null(base_data()$y),
                             c("Left" = "l",
                               "Right" = "r"))
          ),
          selected = input[['sides_original']] %||% default_args[['sides']],
          status = "default",
          size = "xs",
          checkIcon = list(
            yes = icon("ok", lib = "glyphicon"),
            no = icon("remove", lib = "glyphicon")
          )
        )
      })
    }
  })

  # _ Make sure params always update ====
  outputOptions(output, "params", suspendWhenHidden = FALSE)

  # Update sides input (NULL -> character(0))
  observeEvent(input$sides_original, {
    layer_data$sides <- input$sides_original %||% character(0)
  }, ignoreNULL = FALSE)

  # Update sides choices based on data
  observeEvent(base_data(), {
    default_args$sides <<- c(switch(!is.null(base_data()$x),
                                    c("b")),
                             switch(!is.null(base_data()$y),
                                    c("l")))
    shinyWidgets::updateCheckboxGroupButtons(session, 'sides_original',
                                             choices = c(switch(!is.null(base_data()$x),
                                                                c("Bottom" = "b",
                                                                  "Top" = "t")),
                                                         switch(!is.null(base_data()$y),
                                                                c("Left" = "l",
                                                                  "Right" = "r"))
                                             ),
                                             selected = input[['sides_original']] %||% default_args[['sides']],
                                             status = "default",
                                             size = "xs",
                                             checkIcon = list(
                                               yes = icon("ok", lib = "glyphicon"),
                                               no = icon("remove", lib = "glyphicon")
                                             )
    )
  }, ignoreNULL = FALSE)

  geom_params_code <- reactive({
    reactive_inputs()
    isolate({
      processed_geom_params_code <- process_args(reactiveValuesToList(default_args),
                                                 c(reactiveValuesToList(input),
                                                   reactiveValuesToList(layer_data)),
                                                 NULL)
    })

    return(processed_geom_params_code)
  })

  return(geom_params_code)
}
