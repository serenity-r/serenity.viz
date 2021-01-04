#' UI for layer aesthetic custom submodule
#'
#' @param id ID of layer aesthetic custom submodule
#'
#' @return UI for layer aesthetic custom submodule
#' @export
layerAesCustomUI <- function(id) {
  ns <- NS(id)

  uiOutput(ns('custom_ui'))
}

#' Server for layer aesthetic submodule
#'
#' @param id ID of layer aesthetic custom submodule
#' @param custom_for Reactive character vector to customize
#' @param custom_value Reactive character vector to initialize and override
#' @param waitforit If custom_for input is within renderUI, set to 1 to make
#'   sure custom_for does not override custom_value.  REFACTOR: This is REALLY
#'   cludgy. Couldn't figure out a more elegant way around this issue.
#'
#' @return
#' @export
layerAesCustomServer <- function(id, custom_for, custom_value = reactive({ NULL }), waitforit = 0) {
  moduleServer(
    id,
    function(input, output, session) {
      # Initialize ----

      ## Storing custom value state to utilize change on button press only
      customized <- reactiveVal(NULL)

      ## Convert inputs to characters
      custom_for_text <- reactive({ as.character(custom_for()) })
      custom_value_text <- reactive({ as.character(custom_value()) })

      # Custom UI ----
      ## _ renderUI ----
      output$custom_ui <- renderUI({
        isolate({
          req(!is.null(custom_for_text()), !is.null(custom_value_text()))
          init_value <- input$custom_text %T||% custom_value_text() %T||% custom_for_text()
          customized(init_value)
          div(
            class = "aes-custom-content",
            tagList(
              textInput(
                session$ns("custom_text"),
                label = "",
                value = init_value
              ),
              actionButton(
                session$ns("custom_ready"),
                label = "",
                icon = icon("check"),
                class = "custom disabled"
              ),
              actionButton(
                session$ns("reset_value"),
                label = "",
                icon = icon("undo"),
                class = switch(identical(init_value, custom_for_text()), "disabled")
              )
            )
          )
        })
      })
      outputOptions(output, "custom_ui", suspendWhenHidden = FALSE)

      # Update custom_text on module input change (custom_value_text()) ----
      ## _ observeEvent ----
      observeEvent(custom_value_text(), {
        updateTextInput(session, "custom_text", value = custom_value_text())
        customized(custom_value_text())
      }, ignoreInit = TRUE)

      # Copy input to custom_text if custom_for_text() changes ----
      #   Using waitforit in case custom_for input is within renderUI in
      #   calling app.
      ## _ observeEvent ----
      observeEvent(custom_for_text(), {
        if (!waitforit) {
          updateTextInput(session, "custom_text", value = custom_for_text())
          customized(custom_for_text())
        } else {
          waitforit <<- 0
        }
      }, ignoreInit = TRUE)

      # Custom ready button events ----
      ## _ observeEvent: Enable/disable custom ready button ----
      # Adding a minor debounce to avoid double call
      observeEvent(debounce(reactive({ c(input$custom_text, customized()) }), 100)(), {
        req(input$custom_text)
        if (!identical(input$custom_text, customized())) {
          shinyjs::enable("custom_ready")
        } else {
          shinyjs::disable("custom_ready")
        }
      }, ignoreInit = TRUE)

      ## _ observeEvent: Equalize input on custom ready button press ----
      observeEvent(input$custom_ready, {
        customized(input$custom_text)
      }, ignoreInit = TRUE)

      ## _ onevent: Enter key in custom_text input presses custom_ready button ----
      shinyjs::onevent("keypress", "custom_text",
                       function(event) {
                         if (event$key == "Enter") {
                           shinyjs::click(paste0("custom_ready"))
                         }
                       }
      )

      # Reset button events ----
      ## _ observeEvent: Show or hide custom text reset button ----
      observeEvent(c(customized(), custom_for_text()), {
        req(!is.null(customized()))

        if (!identical(customized(), custom_for_text())) {
          shinyjs::enable("reset_value")
        } else {
          shinyjs::disable("reset_value")
        }
      }, ignoreInit = TRUE)

      ## _ observeEvent: Copy input to custom_text if reset clicked ----
      #   Not sure why need to duplicate. eventExpr = c(custom_for_text(), input$reset_value)
      #   with ignoreInit = TRUE caused handlerExpr to be run on init.
      observeEvent(input$reset_value, {
        updateTextInput(session, "custom_text", value = custom_for_text())
        customized(custom_for_text())
      }, ignoreInit = TRUE)

      return(customized)
    }
  )
}
