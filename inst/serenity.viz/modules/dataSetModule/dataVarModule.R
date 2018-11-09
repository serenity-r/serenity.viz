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

# Module UI function
dataVarInput <- function(id, var, default='') {
  # Create a namespace function using the provided id
  ns <- NS(id)
  inputId <- ns("filter")

  # Prep
  if (class(var) %in% c("numeric","integer")) {
    min_var <- min(var, na.rm = TRUE)
    max_var <- max(var, na.rm = TRUE)
  }

  tagList(
    switch(class(var),
           'integer' = ,
           'numeric' = sliderInput(inputId = inputId,
                                   label = "",
                                   min = min_var,
                                   max = max_var,
                                   step = (max_var - min_var)/100,
                                   value = c(min_var, max_var)),
           'factor' = selectizeInput(inputId = inputId,
                                     label = "",
                                     choices = levels(var),
                                     selected = levels(var),
                                     multiple = TRUE,
                                     options = list(
                                       'plugins' = list('remove_button',
                                                        'drag_drop'),
                                       'create' = TRUE,
                                       'persist' = FALSE
                                     )
           ),
           ''
    ) %>%
      var_wrap(id, default)
  )
}

# SERVER ----
dataVar <- function(input, output, session) {
  return(
    reactive({
      browser()
      if (!is.null(input$filter)) {
        # Get name
        ns <- session$ns
        return(str_split_fixed(ns(''), '-', n=3)[2])
        # if (class(input$filter[1]) %in% c('integer', 'numeric')) {
        # } else if (class(input$filter[1]) == 'factor') {
        # }
      }
      return(NULL)
    })
  )
}
