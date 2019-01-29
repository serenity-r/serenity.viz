#' UI for data variable submodule
#'
#' @param id  ID of data variable
#' @param var Variable from data frame
#' @param default Some kind of class
#'
#' @return UI for data variable
#'
dataVarUI <- function(id, var, default='') {
  # Create a namespace function using the provided id
  ns <- NS(id)
  inputId <- ns("filter")

  init <- init_vals(var)

  tagList(
    tagList(
      switch(class(var),
           'integer' = ,
           'numeric' = sliderInput(inputId = inputId,
                                   label = "",
                                   min = init$min,
                                   max = init$max,
                                   step = (init$max - init$min)/100,
                                   value = c(init$min, init$max)),
           'factor' = selectizeInput(inputId = inputId,
                                     label = "",
                                     choices = init$levels,
                                     selected = init$levels,
                                     multiple = TRUE,
                                     options = list(
                                       'plugins' = list('remove_button',
                                                        'drag_drop'),
                                       'create' = TRUE,
                                       'persist' = FALSE
                                     )),
           '')
    ) %>%
      var_wrap(id, default)
  )
}

#' Server for data variable submodule
#'
#' @param input   Shiny inputs
#' @param output  Shiny outputs
#' @param session Shiny user session
#' @param var Variable from data frame
#'
dataVarServer <- function(input, output, session, var) {

  varToCode <- reactive({
    init <- init_vals(var)
    arg <- list(filter = c(), mutate = c())
    if (!is.null(input$filter)) {
      ns <- session$ns
      var_name <- stringr::str_split(ns(''), '-')[[1]][3]
      if (class(input$filter[1]) %in% c('integer', 'numeric')) {
        if (init$min < input$filter[1]) {
          arg$filter <- paste(input$filter[1], "<", var_name)
        }

        if (input$filter[2] < init$max) {
          arg$filter <- c(arg$filter,
                          paste(var_name, "<", input$filter[2]))
        }
      } else {
        # First, drop levels
        dropme <- setdiff(init$levels, input$filter)
        if (length(dropme) > 0) {
          arg$filter <- paste0("!(",
                               var_name,
                               " %in% c(",
                               paste0("\"", dropme, "\"", collapse = ", "),
                               "))")
          arg$mutate <- paste0(var_name,
                               " = fct_drop(",
                               var_name,
                               ", only = c(",
                               paste0("\"", dropme, "\"", collapse = ", "),
                               "))")
        }

        # Check if reordered
        if (!all(intersect(init$levels, input$filter) ==
                 intersect(input$filter, init$levels))) {
          arg$mutate <- c(arg$mutate,
                          paste0(var_name,
                                 " = fct_relevel(",
                                 var_name,
                                 ", c(",
                                 paste0("\"", input$filter, "\"", collapse = ", "),
                                 "))")
          )
        }
      }
    }
    arg
  })

  return(varToCode)
}

# UTILS ----

# Surrounding div for buttons and labels
var_wrap <- function(content, id, default='') {
  tagList(
    div(
      id = paste0(id, '-wrap'),
      class = paste0('var-wrap ', default),
      content
    )
  )
}

init_vals <- function(var) {
  init <- list()

  if (class(var) %in% c("numeric","integer")) {
    init$min <- min(var, na.rm = TRUE)
    init$max <- max(var, na.rm = TRUE)
  } else if (class(var) %in% c("factor")) {
    init$levels <- levels(var)
  }

  return(init)
}
