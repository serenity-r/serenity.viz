#' UI for computed data module
#'
#' @param id  Data ID
#'
#' @return UI for data
#'
computedDataUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tagList(
    widgetHeader(),
    widgetBody(
      uiOutput(ns("dataset_computed_vars"),
               class = "dataset-vars")
    )
  )
}

#' Server for computed data module
#'
#' @param input   Shiny inputs
#' @param output  Shiny outputs
#' @param session Shiny user session
#' @param layer_stat Reactive value of currently selected layer stat
#'
#' @import shiny
#'
computedDataServer <- function(input, output, session, layer_stat) {

  computed_var_names <- reactive({
    stat_computed_vars[[layer_stat()]]
  })

  output$dataset_vars <- renderUI({
    dragulaSelectR::dragZone(
      id = session$ns('computeddatazone'),
      choices = sapply(computed_var_names(), function(var_name) {
        div(
          class = "varzone computed",
          icon("calculator"),
          span(class = "varname", var_name)
        )
      }, simplify = FALSE, USE.NAMES = TRUE)
    )
  })
}
