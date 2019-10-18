layerPositionUI <- function(id) {
  ns <- NS(id)

  tagList(
    uiOutput(ns('position_chooser')),
    uiOutput(ns('position_options'))
  )
}

layerPositionServer <- function(input, output, session, ggdata) {
  # Needed for mapping of ui inputs to ggplot2 arguments
  #  This creates the default list of reactives that just pass on the value of inputs
  # position_vals <- reactive({
  #   req(input[["position"]])
  #
  #   do.call(reactiveValues, purrr::imap(formals(paste0("position_", input[["position"]])), ~ eval(substitute(reactive({ input[[.y]] }), list(.y = .y)))))
  # })

  observeEvent(input[['jitter_seed_refresh']], {
    updateNumericInput(session, 'jitter_seed', value = sample.int(.Machine$integer.max, 1L))
  })

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
                     list(value = .x, input = input, session = session, data = isolate(ggdata()))),
             error = function(e) {
               tryCatch(
                 do.call(paste0(.y,'_ui'),
                         list(value = .x, input = input, session = session, data = isolate(ggdata()))),
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
                                          # purrr::imap(~ modify_args(.y, .x, isolate(ggdata()))) %>%
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
jitter_width_ui <- function(value, input, session, data = NULL) {
  if (is.null(value)) value = 0.4*resolution(data$x, zero = FALSE)

  numericInput(session$ns('jitter_width'),
              label = 'Width:',
              value = input[['jitter_width']] %||% value,
              min = 0,
              max = Inf,
              step = 0.1)
}

jitter_height_ui <- function(value, input, session, data = NULL) {
  if (is.null(value)) value = 0.4*resolution(data$y, zero = FALSE)

  numericInput(session$ns('jitter_height'),
               label = 'Height:',
               value = input[['jitter_height']] %||% value,
               min = 0,
               max = Inf,
               step = 0.1)
}

jitter_seed_ui <- function(value, input, session, data = NULL) {
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

modify_args <- function(param, value, data) {
  return(
    switch(param,
           "jitter_width" = value*resolution(data$x, zero = FALSE),
           value
    )
  )
}
