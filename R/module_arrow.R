arrowUI <- function(id, title = "Value") {
  ns <- NS(id)

  # Set title as option to module for server implementation
  op <- options()
  op.serenity.viz.arrow <- list(
    serenity.viz.arrow.title = title
  )
  options(op.serenity.viz.arrow)

  uiOutput(ns('arrow'))
}

arrowServer <- function(input, output, session = getDefaultReactiveDomain(),
                        default_angle = 30,
                        default_length = grid::unit(0.25, "inches"),
                        default_ends = "none",
                        default_type = "open") {

  output$arrow <- renderUI({
    isolate({
      div(
        id = session$ns('arrow'),
        tags$head(includeCSS(file.path(resourcePath, "css", "arrow.css"))),
        class = "arrow",
        div(
          class = "endpoints",
          tags$label("Endpoints:", class = "control-label"),
          shinyWidgets::pickerInput(
            session$ns('first'),
            label = NULL,
            selected = input[['first']] %||% ifelse(default_ends %in% c("first", "both"), "yes", "no"),
            choices = list("none" = "no", " " = "yes"),
            choicesOpt = list(
              icon = c("", ifelse((input[['type']] %||% default_type) == "open", "fa fa-arrow-left", "glyphicon glyphicon-arrow-left"))
            ),
            options = list(
              `icon-base` = "")
          ),
          shinyWidgets::pickerInput(
            session$ns('last'),
            label = NULL,
            selected = input[['last']] %||% ifelse(default_ends %in% c("last", "both"), "yes", "no"),
            choices = list("none" = "no", " " = "yes"),
            choicesOpt = list(
              icon = c("", ifelse((input[['type']] %||% default_type) == "open", "fa fa-arrow-right", "glyphicon glyphicon-arrow-right"))
            ),
            options = list(
              `icon-base` = "")
          )
        ),
        shinyWidgets::radioGroupButtons(
          session$ns("type"),
          label = "Arrow type:",
          selected = input[['type']] %||% default_type,
          choices = c(`<i class='fa fa-arrow-right'></i>` = "open", `<i class='glyphicon glyphicon-arrow-right'></i>` = "closed")
        ),
        shinyWidgets::knobInput(
          session$ns("angle"),
          label = "Angle:",
          value = input[['angle']] %||% default_angle,
          min = 1, max = 179,
          angleOffset = -90,
          angleArc = 180
        ),
        div(
          style="position: relative;",
          unitChooserUI(session$ns('length'), "Length") %>% {
            .$attribs$class <- paste(.$attribs$class, "arrow-length")
            .
          }
        )
      )
    })
  })
  outputOptions(output, "arrow", suspendWhenHidden = FALSE)

  arrow_length <- callModule(unitChooserServer,
                             "length",
                             default_value = 0.25,
                             default_unit = "inches")

  observeEvent(input$type, {
    shinyWidgets::updatePickerInput(session, 'first',
                                    selected = input[['first']],
                                    choices = list("none" = "no", " " = "yes"),
                                    choicesOpt = list(
                                      icon = c("", ifelse(input[['type']] == "open", "fa fa-arrow-left", "glyphicon glyphicon-arrow-left"))
                                    ))
    shinyWidgets::updatePickerInput(session, 'last',
                                    selected = input[['last']],
                                    choices = list("none" = "no", " " = "yes"),
                                    choicesOpt = list(
                                      icon = c("", ifelse(input[['type']] == "open", "fa fa-arrow-right", "glyphicon glyphicon-arrow-right"))
                                    ))
  })

  return(
    reactive({
      req(input$first)

      if ((input$first == "no") && (input$last == "no")) {
        return(NULL)
      } else {
        return(grid::arrow(angle = input$angle,
                           length = arrow_length(),
                           ends = ifelse(input$first == "yes",
                                         ifelse(input$last == "yes",
                                                "both",
                                                "first"),
                                         "last"),
                           type = input$type)
        )
      }
    })
  )
}

`==.arrow` <- function (x, y) {
  identical(x, y)
}

`!=.arrow` <- function (x, y) {
  !identical(x, y)
}

as.character.arrow <- function (x, ...) {
  # Defaults for grid::arrow
  default_args <- list("angle" = 30,
                       "length" = grid::unit(0.25, "inches"),
                       "ends" = "last",
                       "type" = "open")

  y <- x
  y$ends <- switch(y$ends, "1" = "first", "2" = "last", "3" = "both")
  y$type <- switch(y$type, "1" = "open", "2" = "closed")
  paste0("arrow(", process_args(default_args, y, NULL), ")")
}
