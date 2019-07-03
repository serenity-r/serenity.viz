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
               uiOutput(ns('position'))
      )
    )
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
        purrr::imap(pars(geom_fun), ~ tryCatch(
          do.call(paste0(geom_fun, '_', .y,'_ui'),
                  list(value = .x, input = input, session = session)),
          error = function(e) {
            tryCatch(
              do.call(paste0(.y,'_ui'),
                      list(value = .x, input = input, session = session)),
              error = function(e) NULL
              )
            })
          )
      )
    })
  })

  output$position <- renderUI({
    triggerAesUpdate()
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

  updateSelectizeInput(
    session, 'position', server = TRUE,
    choices = c("Identity", "Jitter", "Dodge", "Jitter-Dodge", "Nudge", "Stack", "Fill")
    )

  # _ Make sure params always update ====
  outputOptions(output, "params", suspendWhenHidden = FALSE)

  paramsToCode <- reactive({
    parList <- purrr::imap(pars(geom_fun), ~ filter_out_defaults(.y, .x, input[[.y]])) %>%
      dropNulls() %>%
      purrr::imap(~ paste(.y, "=", .x))
  })

  return(
    list(
      inherit.aes = reactive({ input[['inherit.aes']] }),
      code = paramsToCode
    )
  )
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

bins_ui <- function(value, input, session) {
  numericInput(session$ns('bins'),
               label = 'Number of bins:',
               value = input[['bins']] %||% 30)
}

binwidth_ui <- function(value, input, session) {
  if (is.null(value)) {
    return(NULL)
  }

  numericInput(session$ns('binwidth'),
               label = 'Width of bins:',
               value = input[['binwidth']] %||% value)
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

geom_smooth_method_ui <- function(value, input, session) {
  selectInput(session$ns('method'),
              label = 'Regression type',
              choices = c("Auto" = "auto",
                          "Linear regression" = "lm",
                          "Generalized linear model" = "glm",
                          "Generalized additive model" = "gam",
                          "LOESS" = "loess"),
              selected = input[['method']] %||% value)
}

geom_dotplot_method_ui <- function(value, input, session) {
  selectInput(session$ns('method'),
              label = 'Binning method',
              choices = c("Dot-density" = "dotdensity",
                          "Fixed bin widths" = "histodot"),
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
  } %>%
    purrr::modify_at("bins", ~ 30) # Default bins is 30
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
