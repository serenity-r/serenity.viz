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

  # Call geom param module (if exists)
  geom_params_code <- NULL
  geom_params_ui <- paste0("layerParams", snakeToCamel(geom_fun, capFirst = TRUE), "UI")
  geom_params_module <- paste0("layerParams", snakeToCamel(geom_fun, capFirst = TRUE), "Server")
  if (exists(geom_params_module)) {
    geom_params_code <- callModule(module = get(geom_params_module),
                                   id = geom_fun,
                                   ggdata = reactive({ ggdata() }))
  }

  # Call position module
  position_code <- callModule(module = layerPositionServer,
                              id = 'position',
                              ggdata = reactive({ ggdata() }),
                              default_position = default_position)

  output$params <- renderUI({
    isolate({
      tagList(
        switch(exists(geom_params_ui), tagList(get(geom_params_ui)(session$ns(geom_fun)), hr()), NULL),
        common_params_ui(input, session)
      )
    })
  })

  # _ Make sure params always update ====
  outputOptions(output, "params", suspendWhenHidden = FALSE)

  params_code <- reactive({
    # Get specific geom params
    processed_params_code <- ifelse(isTruthy(geom_params_code), geom_params_code(), "")

    # Get common layer params
    common_layer_code <- process_args(formals(geom_fun)[c("na.rm", "show.legend", "inherit.aes")], input, ggdata)
    processed_params_code <- paste0(processed_params_code,
                                    ifelse(nchar(processed_params_code) > 0 && nchar(common_layer_code) > 0, ",\n", ""),
                                    common_layer_code)

    # Get position arguments
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

common_params_ui <- function(input, session) {
  tagList(
    checkboxInput(session$ns('na.rm'),
                  label = 'Remove NA?',
                  value = input[['na.rm']] %||% FALSE),
    checkboxInput(session$ns('inherit.aes'),
                  label = 'Inherit aesthetics?',
                  value = input[['inherit.aes']] %||% TRUE),
    radioButtons(session$ns('show.legend'),
                 label = 'Show legend?',
                 choices = c("auto", "yes", "no"),
                 selected = input[['show.legend']] %||% "auto",
                 inline = TRUE)
  )
}

# Utils ----
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

#' Process arguments
#'
#' @param default_args List of default arguments
#' @param input Shiny inputs
#' @param ggdata Reactive of computed layer data from ggplot
#' @param modify_args Function that modifies arguments (if supplied - default NULL)
#'
#' @return Comma separated string of function arguments, with defaults removed
#'   and modified if necessary.
process_args <- function(default_args, input, ggdata, modify_args = NULL) {
  purrr::imap(default_args, ~ filter_out_defaults(.y, .x, input[[.y]])) %>%
    dropNulls() %>%
    purrr::imap(~ ifelse(!is.null(modify_args),
                         do.call(modify_args, list(param = .y, value = .x, data = isolate(ggdata()))),
                         .x)) %>%
    purrr::imap(~ paste(.y, "=", .x)) %>%
    paste(., collapse = ", ")
}

squote <- function(x) {
  paste0('"', x, '"')
}

is.string <- function(x) { is.character(x) && length(x) == 1 }

snakeToCamel <- function(x, capFirst = FALSE) {
  paste(unlist(
    purrr::imap(
      strsplit(x, "[_]")[[1]],
      ~ paste0(ifelse(.y > 1 || capFirst,
                      toupper(substring(.x, 1, 1)),
                      substring(.x, 1, 1)),
               substring(.x, 2))
    )),
    collapse = ""
  )
}
