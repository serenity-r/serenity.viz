#' UI for labels module
#'
#' @param id  labels ID
#'
#' @return UI for labels module
#'
labelsUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  # If using an update button:
  # The update button should be fixed - may need Javascript for this
  # https://stackoverflow.com/a/11833892/8663034
  # http://tether.io/overview/why_you_should_use_tether/
  div(
    id = ns("wrap"),
    class = "labels-wrap",
    uiOutput(ns("ui"))
  )
}

#' Server for labels module
#'
#' @param input   Shiny inputs
#' @param output  Shiny outputs
#' @param session Shiny user session
#' @param xlabel  x-label reactive to replace placeholder
#' @param ylabel  y-label reactive to replace placeholder
#'
#' @import shiny
#'
labelsServer <- function(input, output, session, xlabel, ylabel) {
  output$ui <- renderUI({
    ns <- session$ns

    isolate({
      # NOTE: If in the future you want to make these go through a keypress of focus
      # change before plotting, check this out:
      #  https://gist.github.com/xiaodaigh/7150112
      #  https://groups.google.com/forum/#!msg/shiny-discuss/BFUgjICEQlc/DSz5Itl_oGMJ
      tagList(
        textInput(ns("x"), "x", input$x),
        textInput(ns("y"), "y", input$y),
        textInput(ns("title"), "Title", input$title),
        textInput(ns("subtitle"), "Subtitle", input$subtitle),
        textAreaInput(ns("caption"), "Caption", input$caption, resize="vertical")
      )
    })
  })

  processed_labels <- reactive({
    label_names <- c("x", "y", "title", "subtitle", "caption")
    labels <- vector(mode="list", length=length(label_names))
    names(labels) <- label_names
    labels <- dropNulls(purrr::map2(labels, label_names, ~ input[[.y]] %T||% NULL))

    paste(
      purrr::map2(names(labels),
                      labels,
                      ~ paste0(.x, ' = "', .y, '"')),
      collapse = ", ") %>% {
        ifelse(isTruthy(.), paste0("labs(", ., ")"), .)
      }
  })

  # Note: I find it interesting that the ID in this case doesn't need
  #   to be explicitly namespaced. I guess it makes sense as we are
  #   passing in the session as an argument. <shrug>
  observe({
    updateTextInput(session, "x", placeholder = xlabel())
  })

  observe({
    updateTextInput(session, "y", placeholder = ylabel())
  })

  return(processed_labels)
}
