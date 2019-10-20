layerPositionUI <- function(id) {
  ns <- NS(id)

  tagList(
    uiOutput(ns('position_chooser')),
    uiOutput(ns('position_options'))
  )
}

layerPositionServer <- function(input, output, session, ggdata, default_position) {
  # Consider doing something similar to ui with server code
  #  (e.g. create jitter_seed_server function analogous to
  #   jitter_seed_ui function)
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
                     selected = input[["position"]] %||% default_position
      )
    })
  })

  # Change this from depends on ggdata() to reactive trigger
  #  in module_layer, create failure <- makeReactiveTrigger(FALSE)
  # This should only redraw when plot failure switches logical value
  output$position_options <- renderUI({
    if (isTruthy(ggdata())) {
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
    } else {
      span("Please fix layer error before continuing.")
    }
  })

  updateSelectizeInput(
    session, 'position', server = TRUE,
    choices = c("Identity", "Jitter", "Dodge", "Jitter-Dodge", "Nudge", "Stack", "Fill")
  )

  position_code <- reactive({
    processed_position_code <- NULL
    if (isTruthy(input$position)) {
      # Process arguments
      args <- purrr::imap(position_args(input$position), ~ filter_out_defaults(.y, .x, input[[.y]])) %>%
        dropNulls() %>%
        purrr::imap(~ modify_args(.y, .x, isolate(ggdata()))) %>%
        purrr::imap(~ paste(stringr::str_split(.y, "_")[[1]][2], "=", .x)) %>%
        paste(., collapse = ", ")

      if ((input$position != default_position) || isTruthy(args)) {
        processed_position_code <- paste0("position = position_", input$position, "(")
        processed_position_code <- paste0(processed_position_code, args)
        processed_position_code <- paste0(processed_position_code, ")")
      }
    }

    return(processed_position_code)
  })

  return(position_code)
}

# Option inputs  ----
jitter_width_ui <- function(value, input, session, data = NULL) {
  if (is.null(value)) value = 0.4

  sliderInput(session$ns('jitter_width'),
              label = 'Width:',
              value = input[['jitter_width']] %||% value,
              min = 0,
              max = 1,
              step = 0.05)
}

jitter_height_ui <- function(value, input, session, data = NULL) {
  if (is.null(value)) value = 0.4

  sliderInput(session$ns('jitter_height'),
               label = 'Height:',
               value = input[['jitter_height']] %||% value,
               min = 0,
               max = 1,
               step = 0.05)
}

jitter_seed_ui <- function(value, input, session, data = NULL) {
  if (!isTruthy(value)) value = sample.int(.Machine$integer.max, 1L)

  div(
    class = "jitter-seed-ui",
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
  if (!is.null(pargs)) {
    names(pargs) <- paste0(position, "_", names(pargs))
    pargs %>%
      purrr::modify_at(c("jitter_width", "jitter_height"), ~ 0.4) %>%
      purrr::modify_at(c("dodge_width", "dodge2_width"), ~ 0)
  } else {
    NULL
  }
}

modify_args <- function(param, value, data) {
  return(
    switch(param,
           "jitter_width" = value*resolution(data$x, zero = FALSE),
           "jitter_height" = value*resolution(data$y, zero = FALSE),
           value
    )
  )
}
