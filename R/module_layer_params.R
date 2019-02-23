#' UI for layer parameters submodule
#'
#' @param id  ID of layer aesthetic
#'
#' @return UI for layer parameters
#'
layerParamsUI <- function(id) {
  ns <- NS(id)

  tagList(
    h3("Layer Parameters"),
    uiOutput(ns('params'))
  )
}

#' Server for layer parameters submodule
#'
#' @param input   Shiny inputs
#' @param output  Shiny outputs
#' @param session Shiny user session
#'
#' @importFrom magrittr %>%
#' @import shiny ggplot2
#'
layerParamsServer <- function(input, output, session) {
  ns <- session$ns

  geom_fun <- paste(stringr::str_split(ns(''), '-')[[1]][2:3], collapse="_")
  # Could of used a switch statement, but I was feeling the obfuscation bug...
  output$params <- renderUI({
    isolate({
      tagList(
        purrr::imap(pars(geom_fun), ~ tryCatch(do.call(paste0(.y,'_ui'), list(value = .x, input = input, session = session)), error = function(e) NULL))
      )
    })
  })

  # paramsToCode <- reactive({
  #   purrr::imap(pars(geom_fun), ~ filter_out_defaults(.y, .x, input[[.y]])) %>%
  #     dropNulls()
  # })

  return(paramsToCode)
}

# Parameter UI ----

inherit.aes_ui <- function(value, input, session) {
  checkboxInput(session$ns('inherit.aes'),
                label = 'Inherit aesthetics?',
                value = input[['inherit.aes']] %||% value)
}

na.rm_ui <- function(value, input, session) {
  checkboxInput(session$ns('na.rm'),
                label = 'Remove NA?',
                value = input[['na.rm']] %||% value)
}

show.legend_ui <- function(value, input, session) {
  if (is.na(value)) value = "auto"
  if (value == TRUE) value = "yes"
  if (value == FALSE) value = "no"
  radioButtons(session$ns('show.legend'),
               label = 'Show legend?',
               choices = c("auto", "yes", "no"),
               selected = input[['show.legend']] %||% value,
               inline = TRUE)
}

method_ui <- function(value, input, session) {
  selectInput(session$ns('method'),
              label = 'Regression type',
              choices = c("Auto" = "auto",
                          "Linear regression" = "lm",
                          "Generalized linear model" = "glm",
                          "Generalized additive model" = "gam",
                          "LOESS" = "loess"),
              selected = input[['method']] %||% value)
}

se_ui <- function(value, input, session) {
  checkboxInput(session$ns('se'),
                label = 'Show confidence bands?',
                value = input[['se']] %||% value)
}

# Utils ----
pars <- function(x) {
  formals(x) %>%
  {
    c(
      .[c("na.rm", "show.legend", "inherit.aes")],
      .[setdiff(names(.),
                c("mapping", "data", "...", "na.rm", "show.legend", "inherit.aes", "stat", "position"))],
      .["position"]
    )
  }
}

filter_out_defaults <- function(param, default, value) {
  if (is.null(value)) {
    return(NULL)
  }

  filtered <- switch(param,
                     "show.legend" = ifelse((value == "auto" && !is.na(default)) ||
                                              (value == "yes" && !default) ||
                                              (value == "no" && default),
                                            value,
                                            NULL),
                     ifelse(default != value, value, NULL)
  )

  if (is.string(filtered)) {
    return(quote(filtered))
  }

  return(filtered)
}

quote <- function(x) {
  paste0('"', x, '"')
}

is.string <- function(x) { is.character(x) && length(x) == 1 }
