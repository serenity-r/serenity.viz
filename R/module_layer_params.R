#' UI for layer parameters submodule
#'
#' @param id  ID of layer aesthetic
#'
#' @return UI for layer parameters
#'
layerParamsUI <- function(id) {
  ns <- NS(id)

  tagList(
    tabsetPanel(
      type = "tabs",
      tabPanel(span(icon(name = "sliders-h"), "Parameters"),
               uiOutput(ns('params'))
      ),
      tabPanel(span(icon(name = "arrows-alt"), "Position"),
               layerPositionUI(ns('position'))
      )
    )
  )
}

#' Server for layer parameters submodule
#'
#' @param input   Shiny inputs
#' @param output  Shiny outputs
#' @param session Shiny user session
#' @param ggdata Ggplot plot object
#' @param default_position  Default layer position
#'
#' @importFrom magrittr %>%
#' @import shiny ggplot2
#'
layerParamsServer <- function(input, output, session, ggdata, default_position) {
  ns <- session$ns

  geom_fun <- paste(stringr::str_split(ns(''), '-')[[1]][2:3], collapse="_")

  # Call position module
  position_code <- callModule(module = layerPositionServer,
                              id = 'position',
                              ggdata = reactive({ ggdata() }),
                              default_position = default_position)

  # Change this from depends on ggdata() to reactive trigger
  #  in module_layer, create failure <- makeReactiveTrigger(FALSE)
  # This should only redraw when plot failure switches logical value.
  # See similar code in module_layer_position
  output$params <- renderUI({
    if (isTruthy(ggdata())) {
      isolate({
        tagList(
          purrr::imap(pars(geom_fun), ~ tryCatch(
            do.call(paste0(geom_fun, '_', .y,'_ui'),
                    list(value = .x, input = input, session = session, data = ggdata())),
            error = function(e) {
              tryCatch(
                tagList(
                  switch(.y == "na.rm", hr(), NULL),
                  do.call(paste0(.y,'_ui'),
                          list(value = .x, input = input, session = session, data = ggdata()))
                ),
                error = function(e) NULL
              )
            })
          )
        )
      })
    } else {
      span("Please fix layer error before continuing.")
    }
  })

  # _ Make sure params always update ====
  outputOptions(output, "params", suspendWhenHidden = FALSE)

  params_code <- reactive({
    processed_params_code <- purrr::imap(pars(geom_fun), ~ filter_out_defaults(.y, .x, input[[.y]])) %>%
      dropNulls() %>%
      purrr::imap(~ paste(.y, "=", .x)) %>%
      paste(., collapse = ", ")

    if (!is.null(position_code())) {
      processed_params_code <- paste0(processed_params_code,
                                      ifelse(nchar(processed_params_code) > 0, ",\n", ""),
                                      position_code())
    }

    return(processed_params_code)
  })

  return(
    list(
      inherit.aes = reactive({ input[['inherit.aes']] }),
      code = params_code
    )
  )
}

# Parameter UI ----

# > General ----

inherit.aes_ui <- function(value, input, session, data=NULL) {
  checkboxInput(session$ns('inherit.aes'),
                label = 'Inherit aesthetics?',
                value = input[['inherit.aes']] %||% value)
}

na.rm_ui <- function(value, input, session, data=NULL) {
  checkboxInput(session$ns('na.rm'),
                label = 'Remove NA?',
                value = input[['na.rm']] %||% value)
}

show.legend_ui <- function(value, input, session, data=NULL) {
  if (is.na(value)) value = "auto"
  if (value == TRUE) value = "yes"
  if (value == FALSE) value = "no"
  radioButtons(session$ns('show.legend'),
               label = 'Show legend?',
               choices = c("auto", "yes", "no"),
               selected = input[['show.legend']] %||% value,
               inline = TRUE)
}

bins_ui <- function(value, input, session, data=NULL) {
  numericInput(session$ns('bins'),
               label = 'Number of bins:',
               value = input[['bins']] %||% 30)
}

binwidth_ui <- function(value, input, session, data=NULL) {
  if (is.null(value)) {
    return(NULL)
  }

  numericInput(session$ns('binwidth'),
               label = 'Width of bins:',
               value = input[['binwidth']] %||% value)
}

se_ui <- function(value, input, session, data=NULL) {
  checkboxInput(session$ns('se'),
                label = 'Show confidence bands?',
                value = input[['se']] %||% value)
}

# > geom_bar ----

geom_bar_width_ui <- function(value, input, session, data=NULL) {
  if (is.null(value)) value = 0.9

  sliderInput(session$ns("width"),
              label = "Width:",
              min = 0,
              max = 1,
              value = input[["width"]] %||% value,
              step = 0.05)
}

# > geom_smooth ----

geom_smooth_method_ui <- function(value, input, session, data=NULL) {
  selectInput(session$ns('method'),
              label = 'Regression type',
              choices = c("Auto" = "auto",
                          "Linear regression" = "lm",
                          "Generalized linear model" = "glm",
                          "Generalized additive model" = "gam",
                          "LOESS" = "loess"),
              selected = input[['method']] %||% value)
}

# > geom_dotplot ----

geom_dotplot_method_ui <- function(value, input, session, data=NULL) {
  selectInput(session$ns('method'),
              label = 'Binning method',
              choices = c("Dot-density" = "dotdensity",
                          "Fixed bin widths" = "histodot"),
              selected = input[['method']] %||% value)
}

# > geom_boxplot ----

geom_boxplot_varwidth_ui <- function(value, input, session, data=NULL) {
  checkboxInput(session$ns('varwidth'),
                label = 'Use variable width boxes?',
                value = input[['varwidth']] %||% value)
}

# Utils ----
pars <- function(x) {
  formals(x) %>%
  {
    c(
      .[setdiff(names(.),
                c("mapping", "data", "...", "na.rm", "show.legend", "inherit.aes", "stat", "position"))],
      .[c("na.rm", "show.legend", "inherit.aes")]
    )
  } %>%
    purrr::modify_at("bins", ~ 30) %>%
    purrr::modify_at("width", ~ 0.9) # Default bins is 30
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
                     switch(is.na(default) || (default != value), value, NULL)
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
