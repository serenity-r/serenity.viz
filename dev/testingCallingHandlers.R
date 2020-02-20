library(ggplot2)
library(magrittr)

p <- iris %>% ggplot() + geom_point()

failure <- FALSE
tryCatch(
  withCallingHandlers(
    withRestarts(
      print(p),
      muffleError = function() {
        failure <<- TRUE
        NULL
      }
    ),
    error = function(e) {
      invokeRestart("muffleError")
    },
    warning = function(w) {
      print("Warning!")
      invokeRestart("muffleWarning")
    },
    message = function(m) {
      print("Message!")
      invokeRestart("muffleMessage")
    }
  ),
  finally = {
    if (!failure) print("Finally!")
  }
)

failure <- FALSE
withCallingHandlers(
  withRestarts({
    print(p)},
    muffleError = function() {
      failure <<- TRUE
      NULL
    }
  ),
  warning = function(w) {
    invokeRestart("muffleWarning")
  },
  message = function(m) {
    invokeRestart("muffleMessage")
  },
  error = function(e) {
    invokeRestart("muffleError")
  },
  finally = {
    if (!failure) print("Finally!")
  }
)

withCallingHandlers(withRestarts({
  steve <<- TRUE
  print(p)
  },
  muffleMessage = function() { return() },
  muffleError = function() { return() }),
  error = function(e) {
    steve <<- "error"
    invokeRestart("muffleError")
  },
  warning = function(w) {
    steve <<- "warning"
  },
  message = function(m) {
    steve <<- "message"
    invokeRestart("muffleMessage")
  }
)

switch(geom_type != "geom-blank",
       shinyWidgets::dropdownButton(
         layerParamsUI(ns('layer-params')),
         inputId = ns("layer-params-btn"),
         status = "header-icon",
         icon = icon("gear"),
         size = "xs",
         right = TRUE,
         tooltip = shinyWidgets::tooltipOptions(title = "Layer Parameters")),
       NULL)

rval <- reactiveVal(NULL)
observeEvent(rval(), {
  ns <- session$ns
  if (!isTRUE(all.equal(ifelse(is.null(rval()), "", rval()),
                        ifelse(is.null(input$mapping), "", input$mapping)))) {
    dndselectr::updateDropZoneInput(session, 'mapping', presets = rval() %||% NA)
  }
  if (!isTRUE(all.equal(ifelse(is.null(input$`aes-choose-data`), "", input$`aes-choose-data`),
                        ifelse(is.null(rval()), "", rval())))) {
    shinyWidgets::updatePickerInput(session, "aes-choose-data", selected = rval() %||% "")
  }
}, ignoreNULL = FALSE)
observeEvent(input$mapping, {
  rval(input$mapping)
}, ignoreNULL = FALSE)
observeEvent(input$`aes-choose-data`, {
  rval(input$`aes-choose-data`)
}, ignoreNULL = FALSE)
