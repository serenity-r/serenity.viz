layerPositionUI <- function(id) {
  ns <- NS(id)

  tagList(
    uiOutput(ns('position_chooser')),
    uiOutput(ns('position_options')),
    uiOutput(ns('position_sub_options'))
  )
}

#' Server function for position arguments
#'
#' @param id ID of layer position module
#' @param base_data Pre-isolated reactive to base layer data
#' @param default_position  Default layer position
#'
#' @importFrom magrittr %>%
#' @import shiny ggplot2
#'
#' @return Reactive expression for layer position code (string)
#'
layerPositionServer <- function(id, base_data, default_position) {
  moduleServer(
    id,
    function(input, output, session) {
      positions <- c("identity", "jitter", "dodge", "dodge2", "jitterdodge", "nudge", "stack", "fill")

      # Need a reactive trigger to fix a shinyWidgets bug
      refreshWidget <- makeReactiveTrigger()

      # Sub-positions that share settings
      pos_to_sub <- list(
        "dodge" = "dodge2",
        "stack" = "fill"
      )
      sub_to_pos <- as.list(names(pos_to_sub))
      names(sub_to_pos) <- purrr::flatten_chr(pos_to_sub)

      # Load position server code (if present)
      # Note that for now all _server function need the additional argument refreshWidget
      #   Can remove once bug in shinyWidgets is addressed
      purrr::map(positions, ~ tryCatch(
        do.call(paste0(.,'_server'),
                list(session = session, refreshWidget = refreshWidget)),
        error = function(e) NULL)
      )

      output$position_chooser <- renderUI({
        init_position <- input$position %||% sub_to_pos[[default_position]] %||% default_position
        isolate({
          tagList(
            selectizeInput(session$ns('position'),
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
                           selected = init_position
            ),
            switch(init_position %in% names(pos_to_sub),
                   checkboxInput(session$ns('position_sub'),
                                 label = switch(pos_to_sub[[init_position]],
                                                "fill" = "Normalize heights?",
                                                "dodge2" = "Variable widths?"),
                                 value = input[["position_sub"]] %||% (default_position %in% names(sub_to_pos))),
                   NULL)
          )
        })
      })
      # _ Make sure position chooser always update ====
      outputOptions(output, "position_chooser", suspendWhenHidden = FALSE)

      position_sub <- reactive({
        req(input$position)
        pos_to_sub[[ifelse(is.logical(input$position_sub) %||% NULL,
                           ifelse(input$position_sub,
                                  input$position,
                                  NA),
                           NA)]]
      })

      position <- reactive({
        req(input$position)
        position_sub() %||% input$position
      })

      additional_args <- reactive({
        req(input$position)
        pos_args <- formals(paste0("position_", input$position))
        sub_args <- formals(paste0("position_", position_sub()))
        sub_args[!(names(sub_args) %in% names(pos_args))]
      })

      output$position_options <- renderUI({
        req(input$position, base_data())
        refreshWidget$depend()

        isolate({
          if (isTruthy(base_data())) {
            tagList(
              # Main options
              purrr::imap(formals(paste0("position_", input$position)), ~ {
                tryCatch(
                  do.call(paste0(input$position, '_', .y, '_ui'),
                          list(value = .x, input = input, session = session, base_data = base_data)),
                  error = function(e) {
                    tryCatch(
                      do.call(paste0(.y,'_ui'),
                              list(value = .x, input = input, session = session, base_data = base_data)),
                      error = function(e) NULL
                    )
                  })
              }
              )
            )
          } else {
            span("Please fix layer error before continuing.")
          }
        })
      })
      # _ Make sure position chooser always update ====
      outputOptions(output, "position_options", suspendWhenHidden = FALSE)

      output$position_sub_options <- renderUI({
        req(position_sub())

        # Sub options
        isolate({
          tagList(
            purrr::imap(additional_args(), ~ {
              tryCatch(
                do.call(paste0(position_sub(), '_', .y, '_ui'),
                        list(value = .x, input = input, session = session, base_data = base_data)),
                error = function(e) {
                  tryCatch(
                    do.call(paste0(.y,'_ui'),
                            list(value = .x, input = input, session = session, base_data = base_data)),
                    error = function(e) NULL
                  )
                })
            })
          )
        })
      })
      # _ Make sure position chooser always update ====
      outputOptions(output, "position_sub_options", suspendWhenHidden = FALSE)

      updateSelectizeInput(
        session, 'position', server = TRUE,
        choices = list("Identity" = "identity",
                       "Jitter" = "jitter",
                       "Dodge" = "dodge",
                       "Jitter-Dodge" = "jitterdodge",
                       "Nudge" = "nudge",
                       "Stack" = "stack")
      )

      position_code <- dedupe(reactive({
        processed_position_code <- ''
        if (isTruthy(input$position)) {
          # Process arguments
          args <- process_position_args(input$position, input, base_data)
          subargs <- process_position_args(position_sub(), input, base_data)

          args <- paste(c(switch(isTruthy(args), args), switch(isTruthy(subargs), subargs)), collapse = ", ")

          if ((position() != default_position) || isTruthy(args)) {
            processed_position_code <- paste0("position = position_", position(), "(")
            processed_position_code <- paste0(processed_position_code, args)
            processed_position_code <- paste0(processed_position_code, ")")
          }
        }

        return(processed_position_code)
      }))

      return(position_code)
    }
  )
}

# Option inputs  ----

# > reusable ui ----

reverse_ui <- function(position) {
  id <- paste0(position, "_reverse")
  function(value, input, session, base_data = NULL) {
    if (is.null(value)) value = FALSE

    checkboxInput(session$ns(id),
                  label = "Reverse?",
                  value = input[[id]] %||% value
    )
  }
}

width_ui <- function(position, type=NULL, default=0.4) {
  id <- paste0(c(position, ifelse(is.null(type), "width", type)), collapse = "_")
  function(value, input, session, base_data = NULL) {
    if (is.null(value)) value = default

    sliderInput(session$ns(id),
                label = paste(c(switch(!is.null(type), .firstCap(type)), "Width:"), collapse = " "),
                value = input[[id]] %||% value,
                min = 0,
                max = 1,
                step = 0.05)
  }
}

height_ui <- function(position, type=NULL, default=0.4) {
  id <- paste0(c(position, ifelse(is.null(type), "height", type)), collapse = "_")
  function(value, input, session, base_data = NULL) {
    if (is.null(value)) value = default

    sliderInput(session$ns(id),
                label = paste(c(switch(!is.null(type), .firstCap(type)), "Height:"), collapse = " "),
                value = input[[id]] %||% value,
                min = 0,
                max = 1,
                step = 0.05)
  }
}

seed_ui <- function(position) {
  id <- paste0(position, "_seed")
  function(value, input, session, base_data = NULL) {
    if (!isTruthy(value)) value = sample.int(.Machine$integer.max, 1L)

    div(
      class = "seed-ui",
      numericInput(session$ns(id),
                   label = 'Seed:',
                   value = input[[id]] %||% value,
                   min = 0,
                   max = .Machine$integer.max,
                   step = 1
      ),
      actionButton(session$ns(paste0(id, '_refresh')),
                   label = '',
                   icon = icon('redo'))
    )
  }
}

# > jitter ----

jitter_width_ui <- width_ui("jitter")
jitter_height_ui <- height_ui("jitter")
jitter_seed_ui <- seed_ui("jitter")

jitter_server <- function(session, refreshWidget = NULL) {
  return({
    observeEvent(session$input[['jitter_seed_refresh']], {
      updateNumericInput(session, 'jitter_seed', value = sample.int(.Machine$integer.max, 1L))
    })
  })
}

# > dodge ----

dodge_width_ui <- function(value, input, session, base_data = NULL) {
  value <- input[["dodge_width"]] %||% value

  div(
    class = "position-dodge-width",
    div(
      class = "dodge-width-switch",
      shinyWidgets::switchInput(
        session$ns("dodge_set_width"),
        label = "Width",
        value = isTruthy(value)
      )
    ),
    div(
      class = paste0("dodge-width-input", ifelse(isTruthy(value), "", " hidden")),
      numericInput(session$ns("dodge_width"),
                   label = "",
                   value = value,
                   min = 0,
                   max = Inf
      )
    )
  )
}

dodge_preserve_ui <- function(value, input, session, base_data = NULL) {
  if (is.null(value) || (length(value) > 1)) value = "total"

  radioButtons(session$ns("dodge_preserve"),
               label = "Preserve:",
               choices = c("total", "single"),
               selected = input[["dodge_preserve"]] %||% value,
               inline = TRUE)
}

dodge_server <- function(session, refreshWidget = NULL) {
  return({
    observeEvent(session$input$dodge_set_width, {
      if (isTruthy(session$input$dodge_set_width)) {
        shinyjs::js$toggleClass("hidden", paste0('#', session$ns("position_options"), ' .dodge-width-input'))
        updateNumericInput(session, session$ns("dodge_width"), value = 0)
      } else {
        shinyjs::js$toggleClass("hidden", paste0('#', session$ns("position_options"), ' .dodge-width-input'))
        session$sendCustomMessage(type = "nullify", message = session$ns("dodge_width"))
      }
    })

    # Fire one event to refresh shinyWidget (current bug in shinyWidgets)
    observeEvent(session$input$dodge_set_width, {
      refreshWidget$trigger()
    }, once = TRUE)
  })
}

# > dodge2 ----

dodge2_padding_ui <- function(value, input, session, base_data = NULL) {
  if (is.null(value)) value = 0.1

  sliderInput(session$ns("dodge2_padding"),
              label = "Padding:",
              min = 0,
              max = 1,
              value = input[["dodge2_padding"]] %||% value,
              step = 0.05)
}

dodge2_reverse_ui <- reverse_ui("dodge2")

# > stack/fill ----

stack_vjust_ui <- function(value, input, session, base_data = NULL) {
  if (is.null(value)) value = 1

  sliderInput(session$ns("stack_vjust"),
              label = "Vertical adjustment:",
              min = 0,
              max = 1,
              value = input[["stack_vjust"]] %||% value,
              step = 0.05
  )
}

stack_reverse_ui <- reverse_ui("stack")

# > nudge ----

nudge_x_ui <- function(value, input, session, base_data = NULL) {
  if (is.null(value)) value = 0

  sliderInput(session$ns("nudge_x"),
              label = "Horizontal Adjustment:",
              min = -1,
              max = 1,
              value = input[["nudge_x"]] %||% value,
              step = 0.02)
}

nudge_y_ui <- function(value, input, session, base_data = NULL) {
  if (is.null(value)) value = 0

  sliderInput(session$ns("nudge_y"),
              label = "Vertical Adjustment:",
              min = -1,
              max = 1,
              value = input[["nudge_y"]] %||% value,
              step = 0.02)
}

# > jitterdodge ----

jitterdodge_jitter.width_ui <- width_ui("jitterdodge", "jitter.width")
jitterdodge_jitter.height_ui <- height_ui("jitterdodge", "jitter.height", default = 0)
jitterdodge_seed_ui <- seed_ui("jitterdodge")
jitterdodge_dodge.width_ui <- width_ui("jitterdodge", "dodge.width", 0.75)

# Consider combining code with jitter_server
jitterdodge_server <- function(session, refreshWidget = NULL) {
  return({
    observeEvent(session$input[['jitterdodge_seed_refresh']], {
      updateNumericInput(session, 'jitterdodge_seed', value = sample.int(.Machine$integer.max, 1L))
    })
  })
}

# Utils ----

# Get argument list for position function and set defaults
position_args <- function(position) {
  pargs <- formals(paste0("position_", position))
  if (!is.null(pargs)) {
    names(pargs) <- paste0(position, "_", names(pargs))
    pargs %>%
      purrr::modify_at(c("jitter_width",
                         "jitterdodge_jitter.width",
                         "jitter_height"), ~ 0.4) %>%
      purrr::modify_at(c("dodge_width", "dodge2_width"), ~ -1) %>%
      purrr::modify_at(c("dodge_preserve", "dodge2_preserve"), ~ "total")
  } else {
    NULL
  }
}

process_position_args <- function(position, input, base_data) {
  if (is.null(position)) return(NULL)

  purrr::imap(position_args(position), ~ filter_out_defaults(.y, .x, input[[.y]])) %>%
    dropNulls() %>%
    purrr::imap(~ modify_position_args(.y, .x, base_data)) %>%
    purrr::imap(~ paste(stringr::str_split(.y, "_")[[1]][2], "=", .x)) %>%
    paste(., collapse = ", ")
}

modify_position_args <- function(param, value, base_data) {
  return(
    switch(param,
           "jitter_width" =,
           "jitterdodge_jitter.width" = value*resolution(base_data()$x, zero = FALSE),
           "jitter_height" =,
           "jitterdodge_jitter.height" = value*resolution(base_data()$y, zero = FALSE),
           "nudge_x" = value*(max(base_data()$x) - min(base_data()$x)),
           "nudge_y" = value*(max(base_data()$y) - min(base_data()$y)),
           value
    )
  )
}

.firstCap <- function(x) {
  s <- strsplit(x, "[.]")[[1]][1]
  paste0(toupper(substring(s, 1, 1)), substring(s, 2))
}
