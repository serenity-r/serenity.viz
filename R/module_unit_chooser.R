unitChooserUI <- function(id, title = "Value") {
  ns <- NS(id)

  # Set title as option to module for server implementation
  op <- options()
  op.serenity.viz.unit_chooser <- list(
    serenity.viz.unit_chooser.title = title
  )
  options(op.serenity.viz.unit_chooser)

  uiOutput(ns('unit_chooser'))
}

unitChooserServer <- function(input, output, session = getDefaultReactiveDomain(),
                              default_value = 0.03,
                              default_unit = "npc") {
  previous_unit <- NULL

  output$unit_chooser <- renderUI({
    isolate({
      div(
        id = session$ns('unit_chooser'),
        tags$head(includeCSS(file.path(resourcePath, "css", "unit_chooser.css"))),
        class = "unit-chooser",
        numericInput(session$ns('value'),
                     getOption("serenity.viz.unit_chooser.title"),
                     value = input[['value']] %||% default_value,
                     min = 0,
                     max = Inf,
                     width = "150px"),
        selectInput(session$ns('unit'),
                    HTML('&nbsp;'),
                    choices = c(
                      "Normalised Parent Coordinates" = "npc",
                      "Centimetres" = "cm",
                      "Millimetres" = "mm",
                      "Inches" = "inches",
                      "Points" = "points",
                      "Picas" = "picas",
                      "Big Points" = "bigpts"
                    ),
                    selected = input[['unit']] %||% default_unit
        )
      )
    })
  })
  outputOptions(output, "unit_chooser", suspendWhenHidden = FALSE)

  observeEvent(input$unit, {
    updateNumericInput(session, 'value', value = grid::convertUnit(previous_unit, input$unit, valueOnly = TRUE))
  }, ignoreInit = TRUE)

  observeEvent(input$value, {
    previous_unit <<- unit(input$value, input$unit)
  }, priority = 1)

  return(
    reactive({
      req(input$value)

      isolate({
        unit(input$value, input$unit)
      })
    })
  )
}

`==.unit` <- function (x, y) {
  abs(as.numeric(x) - as.numeric(grid::convertUnit(y, attr(x, "unit")))) < .Machine$double.eps
}

`!=.unit` <- function (x, y) {
  abs(as.numeric(x) - as.numeric(grid::convertUnit(y, attr(x, "unit")))) >= .Machine$double.eps
}

