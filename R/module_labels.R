#' UI for labels module
#'
#' @param id  labels ID
#'
#' @return UI for labels module
#'
labelsUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

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

    # TODO: Have these text inputs activate only on keypress
    # - https://stackoverflow.com/questions/31415301/shiny-responds-to-enter
    # - https://stackoverflow.com/questions/24973549/r-shiny-key-input-binding
    tagList(
      textInput(ns("x"), "x", input$x),
      textInput(ns("y"), "y", input$y),
      textInput(ns("title"), "Title", input$title),
      textInput(ns("subtitle"), "Subtitle", input$subtitle),
      textAreaInput(ns("caption"), "Caption", input$caption, resize="vertical")
    )
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

# UTILS ----

# Given a vector or list, drop all the NULL items in it
dropNulls <- function(x) {
  x[!vapply(x, is.null, FUN.VALUE=logical(1))]
}
