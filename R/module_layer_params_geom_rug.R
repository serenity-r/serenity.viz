layerParamsGeomRugUI <- function(id) {
  ns <- NS(id)

  uiOutput(ns("params"))
}

# Refactor: Unnecessary plotting occurs sometimes on base_data change
layerParamsGeomRugServer <- function(input, output, session, base_data) {
  # Reactive since sides defaults changes based on data
  default_args <- reactiveValues("sides" = c("b", "l"),
                                 "length" = unit(0.03, 'npc'),
                                 "outside" = FALSE)

  # Need to hijack sides input as shinyWidgets returns NULL instead
  #   of character(0) when nothing is selected
  layer_data <- reactiveValues(sides = character(0),
                               length = unit(0.03, 'npc')) # Should set to default in observeEvent - being lazy here

  reactive_inputs <- reactive({
    paste(
      layer_data$sides,
      default_args$sides,
      layer_data$length,
      input$outside
    )
  })

  output$params <- renderUI({
    isolate({
      tagList(
        shinyWidgets::checkboxGroupButtons(
          session$ns('sides_original'),
          label = "Which sides?",
          choices = c("Bottom" = "b",
                      "Top" = "t",
                      "Left" = "l",
                      "Right" = "r"),
          selected = input[['sides_original']] %||% default_args[['sides']],
          status = "default",
          size = "xs",
          checkIcon = list(
            yes = icon("ok", lib = "glyphicon"),
            no = icon("remove", lib = "glyphicon")
          )
        ),
        unitChooserUI(session$ns('length'), "Rug length"),
        div(
          class = "inline-switch-with-label",
          h5("Outside plot"),
          shinyWidgets::switchInput(session$ns('outside'),
                                    value = input[['outside']] %||% default_args[['outside']],
                                    size = "mini",
                                    inline = TRUE)
        )
      )
    })
  })
  outputOptions(output, "params", suspendWhenHidden = FALSE)

  rug_length <- callModule(unitChooserServer, "length")

  # Update length input
  observeEvent(rug_length(), {
    layer_data$length <- rug_length()
  })

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
  })

  geom_params_code <- reactive({
    reactive_inputs()
    isolate({
      processed_geom_params_code <- process_args(reactiveValuesToList(default_args),
                                                 c(reactiveValuesToList(input),
                                                   reactiveValuesToList(layer_data)),
                                                 NULL,
                                                 modify_geom_rug_args)
    })

    return(processed_geom_params_code)
  })

  return(geom_params_code)
}

modify_geom_rug_args <- function(param, value, base_data) {
  return(
    switch(param,
           "length" = paste0("unit(", as.numeric(value), ", '", grid::unitType(value), "')"),
           value
    )
  )
}
