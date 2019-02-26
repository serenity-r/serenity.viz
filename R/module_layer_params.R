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
layerParamsServer <- function(input, output, session, triggerAesUpdate) {
  ns <- session$ns

  geom_fun <- paste(stringr::str_split(ns(''), '-')[[1]][2:3], collapse="_")
  # Could of used a switch statement, but I was feeling the obfuscation bug...
  output$params <- renderUI({
    triggerAesUpdate()
    isolate({
      tagList(
        purrr::imap(pars(geom_fun), ~ tryCatch(do.call(paste0(.y,'_ui'), list(value = .x, input = input, session = session)), error = function(e) NULL))
      )
    })
  })

  # _ Make sure params always update ====
  outputOptions(output, "params", suspendWhenHidden = FALSE)

  paramsToCode <- reactive({
    parList <- purrr::imap(pars(geom_fun), ~ filter_out_defaults(.y, .x, input[[.y]])) %>%
      dropNulls() %>%
      purrr::imap(~ paste(.y, "=", .x))
  })

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

  show.legend.key <- list("auto" = NA, "yes" = TRUE, "no" = FALSE)
  filtered <- switch(param,
                     "show.legend" = switch((value == "auto" && !is.na(default)) ||
                                              (value == "yes" && (!default || is.na(default))) ||
                                              (value == "no" && (default || is.na(default))),
                                            show.legend.key[[value]],
                                            NULL),
                     switch(default != value, value, NULL)
  )

  if (is.string(filtered)) {
    return(squote(filtered))
  }

  return(filtered)
}

squote <- function(x) {
  paste0('"', x, '"')
}

is.string <- function(x) { is.character(x) && length(x) == 1 }
