# UI ----

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

# Module UI function
dataVarInput <- function(id, var, default='') {
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
           ''),
      verbatimTextOutput(ns("placeholder"))
    ) %>%
      var_wrap(id, default)
  )
}

# SERVER ----
dataVar <- function(input, output, session, var) {

  output$placeholder <- renderPrint({ input$filter })

  varToCode <- reactive({
    init <- init_vals(var)
    arg <- c()
    if (!is.null(input$filter)) {
      ns <- session$ns
      var_name <- stringr::str_split(ns(''), '-')[[1]][2]
      if (class(input$filter[1]) %in% c('integer', 'numeric')) {
        if (init$min < input$filter[1]) {
          arg <- paste(input$filter[1], "<", var_name)
        }

        if (input$filter[2] < init$max) {
          arg <- c(arg,
                   paste(var_name, "<", input$filter[2]))
        }

        if (length(arg) > 0) {
          arg <- paste(arg, collapse = ", ")
        }
      } else {
      }
    }
    arg
  })

  return(varToCode)
}
