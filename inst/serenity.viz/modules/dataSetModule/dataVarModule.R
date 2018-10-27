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

  # Prep
  if (class(var) %in% c("numeric","integer")) {
    min_var <- min(var, na.rm = TRUE)
    max_var <- max(var, na.rm = TRUE)
  }

  tagList(
    switch(class(var),
           'integer' = ,
           'numeric' = sliderInput(inputId = ns("slider"),
                                   label = "",
                                   min = min_var,
                                   max = max_var,
                                   step = (max_var - min_var)/100,
                                   value = c(min_var, max_var)),
           'factor' = selectizeInput(inputId = ns("selectize"),
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
# Needs the values$gg plot to adjust variables on changes
dataVar <- function(input, output, session, gg) {
  observeEvent(input$slider, {
    ns <- session$ns
    var <- gsub('-', '', ns(""))
    showNotification(var)
  })
}
