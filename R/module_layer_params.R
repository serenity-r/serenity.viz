#' UI for layer parameters submodule
#'
#' @param id  ID of layer aesthetic
#'
#' @return UI for layer parameters
#'
layerParamsUI <- function(id) {
  ns <- NS(id)

  uiOutput(ns('params'))
}

#' Server for layer parameters submodule
#'
#' @param input   Shiny inputs
#' @param output  Shiny outputs
#' @param session Shiny user session
#' @param base_data Reactive values of ggplot plot object states (data and scales)
#' @param layer_stat Reactive value of currently selected layer stat
#'
#' @importFrom magrittr %>%
#' @import shiny ggplot2
#'
layerParamsServer <- function(input, output, session, base_data, layer_stat) {
  ns <- session$ns

  ns_levels <- stringr::str_split(ns(''), '-')[[1]]
  geom_ns_ind <- which(ns_levels == "geom")
  geom_fun <- paste(ns_levels[geom_ns_ind:(geom_ns_ind+1)], collapse="_")

  # This stores returned reactives from stat modules
  stat_modules <- reactiveValues()

  # Call geom param module (if exists)
  geom_params_code <- reactive({ "" })
  geom_params_ui <- paste0("layerParams", snakeToCamel(geom_fun, capFirst = TRUE), "UI")
  geom_params_module <- paste0("layerParams", snakeToCamel(geom_fun, capFirst = TRUE), "Server")
  if (exists(geom_params_module)) {
    geom_params_code <- callModule(module = get(geom_params_module),
                                   id = geom_fun,
                                   base_data = base_data,
                                   session = session)
  }

  # Update stat module output reactives - create only once!
  observeEvent(layer_stat(), {
    stat_params_module <- paste0("layerParamsStat", snakeToCamel(layer_stat(), capFirst = TRUE), "Server")
    if (!(layer_stat() %in% names(stat_modules))) {
      stat_modules[[layer_stat()]] <- switch(as.character(exists(stat_params_module)),
                                             "TRUE" = callModule(module = get(stat_params_module),
                                                                 id = paste0("stat_", layer_stat()),
                                                                 base_data = base_data,
                                                                 session = session),
                                             "FALSE" = reactive({ "" })
      )
    }
  }, priority = 1)

  output$params <- renderUI({
    isolate({
      tagList(
        switch(exists(geom_params_ui), tagList(get(geom_params_ui)(session$ns(geom_fun)), hr())),
        uiOutput(session$ns('stat_ui')),
        common_params_ui(input, session)
      )
    })
  })
  outputOptions(output, "params", suspendWhenHidden = FALSE)

  output$stat_ui <- renderUI({
    req(layer_stat())
    stat_params_ui <- paste0("layerParamsStat", snakeToCamel(layer_stat(), capFirst = TRUE), "UI")
    switch(exists(stat_params_ui), tagList(get(stat_params_ui)(session$ns(paste0("stat_", layer_stat()))), hr()))
  })
  outputOptions(output, "stat_ui", suspendWhenHidden = FALSE)

  params_code <- dedupe(reactive({
    req(!is.null(geom_params_code()),
        isTruthy(stat_modules[[layer_stat()]]) && !is.null(stat_modules[[layer_stat()]]()))

    # Get specific geom params
    processed_params_code <- geom_params_code()

    # Get specific stat params
    processed_params_code <- paste0(processed_params_code,
                                    ifelse(nchar(processed_params_code) > 0 && nchar(stat_modules[[layer_stat()]]()) > 0, ", ", ""),
                                    stat_modules[[layer_stat()]]())

    # Get common layer params
    common_layer_code <- process_args(formals(geom_fun)[c("na.rm", "show.legend", "inherit.aes")], input, base_data)
    processed_params_code <- paste0(processed_params_code,
                                    ifelse(nchar(processed_params_code) > 0 && nchar(common_layer_code) > 0, ",\n", ""),
                                    common_layer_code)

    return(processed_params_code)
  }))

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
                     "closed" = switch(value != default, "right", NULL),
                     "sides" = switch(as.character(length(value)),
                                      "0" = "",
                                      switch((length(value) != length(default)) || any(!(value %in% default)),
                                             paste(sort(value), collapse = ""), NULL)
                     ),
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
#' @param base_data Computed layer and scales data from ggplot
#' @param modify_args Function that modifies arguments (if supplied - default NULL)
#' @param allowNULL Arguments where NULL means something, so don't drop.  This
#'   must be followed by a modify_args function that properly deals with NULLs.
#'   See module_layer_params_boxplot.R for an example.
#'
#' @return Comma separated string of function arguments, with defaults/NULLs
#'   removed and modified if necessary.
process_args <- function(default_args, input, base_data, modify_args = NULL, allowNULL = NULL) {
  purrr::imap(default_args, ~ filter_out_defaults(.y, .x, input[[.y]])) %>%
    dropNulls(allowNULL) %>%
    purrr::imap(~ ifelse(!is.null(modify_args),
                         do.call(modify_args, list(param = .y, value = .x, base_data = base_data)),
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

camelToSnake <- function(x) {
  paste(unlist(
    purrr::imap(
      strsplit(x, "(?<=[[:lower:]])(?=[[:upper:]])", perl=TRUE)[[1]],
      ~ ifelse(.y > 1,
               paste0("_", tolower(.x)),
               tolower(.x))
    )),
    collapse = ""
  )
}
