layerPositionUI <- function(id) {
  ns <- NS(id)

  tagList(
    uiOutput(ns('position_chooser')),
    uiOutput(ns('position_options'))
  )
}

layerPositionServer <- function(input, output, session) {
  # Needed for mapping of ui inputs to ggplot2 arguments
  #  This creates the default list of reactives that just pass on the value of inputs
  # position_vals <- reactive({
  #   req(input[["position"]])
  #
  #   do.call(reactiveValues, purrr::imap(formals(paste0("position_", input[["position"]])), ~ eval(substitute(reactive({ input[[.y]] }), list(.y = .y)))))
  # })

  output$position_chooser <- renderUI({
    ns <- session$ns

    isolate({
      selectizeInput(ns('position'),
                     label = NULL,
                     choices = list("Identity" = "identity",
                                    "Jitter" = "jitter",
                                    "Dodge" = "dodge",
                                    "Jitter-Dodge" = "jitterdodge",
                                    "Nudge" = "nudge",
                                    "Stack" = "stack"),
                     options = list(render = I(
                       "{
                        option: function(item, escape) {
                        return '<div class = \"position\"><span data-value = \"' + escape(item.value) + '\"></span>' + escape(item.label) + '</div>'
                       }
    }")),
                     selected = input[["position"]] %||% "identity"
      )
    })
  })

  output$position_options <- renderUI({
    switch(isTruthy(input[["position"]]),
           purrr::imap(formals(paste0("position_", input[["position"]])), ~ tryCatch(
             do.call(paste0(input[["position"]], '_', .y,'_ui'),
                     list(value = .x, input = input, session = session)),
             error = function(e) {
               tryCatch(
                 do.call(paste0(.y,'_ui'),
                         list(value = .x, input = input, session = session)),
                 error = function(e) NULL
               )
             })
           ),
           NULL)
  })

  updateSelectizeInput(
    session, 'position', server = TRUE,
    choices = c("Identity", "Jitter", "Dodge", "Jitter-Dodge", "Nudge", "Stack", "Fill")
  )

  position_code <- reactive({
    if (isTruthy(input$position) &&
        input$position != "identity") {
      processed_position_code <- paste0("position = position_", input$position, "(")

      # Process arguments
      processed_position_code <- paste0(processed_position_code,
                                        purrr::imap(position_args(input$position), ~ filter_out_defaults(.y, .x, input[[.y]])) %>%
                                          dropNulls() %>%
                                          purrr::imap(~ paste(stringr::str_split(.y, "_")[[1]][2], "=", .x)) %>%
                                          paste(., collapse = ", ")
      )

      # Close it up
      processed_position_code <- paste0(processed_position_code, ")")
    } else {
      processed_position_code <- NULL
    }

    return(processed_position_code)
  })

  return(position_code)
}

# Option inputs  ----
jitter_width_ui <- function(value, input, session) {
  if (is.null(value)) value = 0.4

  sliderInput(session$ns('jitter_width'),
              label = 'Width:',
              min = 0,
              max = 1,
              value = input[['jitter_width']] %||% value,
              step = 0.01)
}

jitter_height_ui <- function(value, input, session) {
  if (is.null(value)) value = 0.4

  sliderInput(session$ns('jitter_height'),
              label = 'Height:',
              min = 0,
              max = 1,
              value = input[['jitter_height']] %||% value,
              step = 0.01)
}

jitter_seed_ui <- function(value, input, session) {
  if (!isTruthy(value)) value = sample.int(.Machine$integer.max, 1L)

  tagList(
    numericInput(session$ns('jitter_seed'),
                 label = 'Seed:',
                 value = input[['jitter_seed']] %||% value,
                 min = 0,
                 max = .Machine$integer.max,
                 step = 1
    ),
    actionButton(session$ns('jitter_seed_refresh'),
                 label = '',
                 icon = icon('redo'))
  )
}

# Utils ----

# Get argument list for position function and set defaults
position_args <- function(position) {
  pargs <- formals(paste0("position_", position))
  names(pargs) <- paste0(position, "_", names(pargs))
  pargs %>%
    purrr::modify_at(c("jitter_width", "jitter_height"), ~ 0.4) %>%
    purrr::modify_at(c("dodge_width", "dodge2_width"), ~ 0)
}

modify_args <- function(param, value, position="identity") {
  return(
    switch(paste0(position, '_', param),
           "jitter_width" = value,
           value
    )
  )
}
