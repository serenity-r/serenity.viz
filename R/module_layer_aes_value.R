#' UI for layer aesthetic value submodule
#'
#' @param id ID of layer aesthetic value submodule
#'
#' @return UI for layer aesthetic value submodule
#' @export
layerAesValueUI <- function(id) {
  ns <- NS(id)

  uiOutput(ns('value_ui'))
}

#' Server for layer aesthetic submodule
#'
#' @param id ID of layer aesthetic value submodule
#' @param aesthetic Input will be for this aesthetic (determines widget)
#' @param initial Input initial value
#' @param show_initial Reactive: Show initial value?
#' @param aesUpdateDependency Trigger update on layer change
#'
#' @return String "<aesthetic> = <value>"
#' @export
layerAesValueServer <- function(id, aesthetic, initial, show_initial = reactive({ FALSE }),
                                aesUpdateDependency = reactive({ NULL })) {
  moduleServer(
    id,
    function(input, output, session) {
      # Input checking for initial ----

      if (!is.null(initial)) {
        initial <- ifelse(!is.na(initial), initial, NA_defaults[[aesthetic]])
      } else {
        return(reactive({ NULL }))
      }

      # Convert default colour values to hex (if applicable)
      if ((aesthetic %in% c('colour', 'fill')) && isTruthy(initial)) {
        initial <- colour_to_hex(initial)
      }

      # Convert default linetype values to string (if applicable)
      if ((aesthetic %in% c('linetype')) && isTruthy(initial)) {
        initial <- linetype_to_string(initial)
      }

      output$value_ui <- renderUI({
        aesUpdateDependency()

        isolate({
          init_value <- input$value %T||% initial

          icons <- div(
            class = "aes-content-icons",
            prettyToggle(
              inputId = session$ns("custom_toggle"),
              value = input$custom_toggle %||% FALSE,
              label_on = "",
              label_off = "",
              status_on = "default",
              status_off = "default",
              outline = TRUE,
              plain = TRUE,
              icon_on = icon("times"),
              icon_off = icon("pencil-alt"),
              inline = TRUE
            ) %>% {
              .$attribs$class <- paste(c(.$attribs$class, "custom-toggle"), collapse = " ")
              .
            }
          )

          content <- tagList(
            create_aes_input(session$ns('value'),
                             aesthetic,
                             init_value),
            actionButton(
              session$ns("reset_value"),
              label = "",
              icon = icon("undo"),
              class = switch(init_value == initial, "disabled")
            )
          )

          tags$section(
            class = "value-section",
            icons,
            conditionalPanel(condition = "input.custom_toggle == null || input.custom_toggle == false",
                             ns = session$ns,
                             content,
                             class = "aes-content"),
            conditionalPanel(condition = "input.custom_toggle == true",
                             ns = session$ns,
                             layerAesCustomUI(session$ns("value")))
          )
        })
      })
      outputOptions(output, "value_ui", suspendWhenHidden = FALSE)

      # Call custom module
      custom_value <- layerAesCustomServer(
        "value",
        custom_for = reactive({ input$value }),
        aesUpdateDependency = aesUpdateDependency
      )

      # Show or hide aesthetic value reset button
      observeEvent(input$value, {
        req(!is.null(input$value))

        if (input$value != initial) {
          shinyjs::enable("reset_value")
        } else {
          shinyjs::disable("reset_value")
        }
      }, ignoreInit = TRUE)

      # Reset aesthetic value to initial value
      observeEvent(input$reset_value, {
        update_aes_input(session, 'value', aesthetic, initial)
      }, ignoreInit = TRUE)

      value_to_code <- reactive({
        req(!is.null(input$custom_toggle),
            !is.null(input$value),
            !is.null(custom_value()))

        value <- NULL
        if (input$custom_toggle) {
          if (show_initial() || (custom_value() != as.character(initial))) {
            # Custom override
            value <- switch(aesthetic,
                            "colour" = ,
                            "linetype" = ,
                            "fill" = paste0('"', custom_value(), '"'),
                            custom_value())
          }
        } else if (show_initial() || (input$value != initial)) {
          # Set value (non-null)
          value <- switch(aesthetic,
                          "colour" = ,
                          "linetype" = ,
                          "fill" = paste0('"', input$value, '"'),
                          as.character(input$value))
        }
        value
      })

      return(value_to_code)
    }
  )
}

aes_wrap <- function(content, class=NULL) {
  tagList(
    div(
      class = paste(c('aes-wrap', class), collapse = " "),
      content
    )
  )
}

# Importing .data from rlang
# https://community.rstudio.com/t/how-to-solve-no-visible-binding-for-global-variable-note/28887

#' Colour translator
#'
#' @param col R color specification
#'
#' @importFrom rlang .data
colour_to_hex <- function(col) {
  if (!grepl("^#[0-9a-fA-F]{6}", col)) {
    return(farver::encode_colour(farver::decode_colour(col)))
  } else {
    return(col)
  }
}

# Linetype translator
#   Linetype goes from [0..6] to linetype name
linetype_choices <- c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash")
linetype_to_string <- function(linetype) {
  if (is.numeric(linetype) &&
      (linetype >= 0) && (linetype <= 6)) {
    return(linetype_choices[linetype+1])
  } else {
    return(linetype)
  }
}

# Create aesthetic input control
# aes_val is assumed to be truthy

#' Create aes inputs
#'
#' @param inputId Id of Shiny input
#' @param aes Name of aesthetic
#' @param aes_val Value of aesthetic
#' @param class Class to add to aesthetic wrapper div
#'
#' @importFrom magrittr %>%
create_aes_input <- function(inputId, aes, aes_val, class=NULL) {
  tagList(
    switch(aes,
           'shape' = sliderInput(inputId = inputId,
                                 label = "",
                                 min = 0,
                                 max = 25,
                                 step = 1,
                                 value = aes_val),
           'colour' = ,
           'fill' = colourpicker::colourInput(inputId = inputId,
                                              label = "",
                                              value = colour_to_hex(aes_val)),
           'weight' = ,
           'size' = ,
           'stroke' = ,
           'width' = ,
           'height' = sliderInput(inputId = inputId,
                                  label = "",
                                  min = 0.1,
                                  max = 10,
                                  step = 0.1,
                                  value = aes_val),
           'alpha' = sliderInput(inputId = inputId,
                                 label = "",
                                 min = 0,
                                 max = 1,
                                 value = aes_val),
           'linetype' = selectInput(inputId = inputId,
                                    label = "",
                                    choices = linetype_choices,
                                    selected = linetype_to_string(aes_val)),
           ''
    ) %>%
      aes_wrap(class)
  )
}

#' Update aes inputs
#'
#' @param session The session object passed to function given to shinyServer.
#' @param inputId Id of input object
#' @param aes Name of aesthetic
#' @param aes_val Value of aesthetic
#'
update_aes_input <- function(session, inputId, aes, aes_val) {
  switch(aes,
         'colour' = ,
         'fill' = colourpicker::updateColourInput(session, inputId, value = colour_to_hex(aes_val)),
         'alpha' = ,
         'shape' = ,
         'weight' = ,
         'size' = ,
         'stroke' = ,
         'width' = ,
         'height' = updateSliderInput(session, inputId, value = aes_val),
         'linetype' = updateSelectInput(session, inputId, selected = linetype_to_string(aes_val)),
         ''
  )
}


