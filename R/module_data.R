#' UI for data module
#'
#' @param id  Data ID
#'
#' @return UI for data
#'
dataUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tagList(
    widgetHeader(),
    widgetBody(
      uiOutput(ns("dataset_vars"),
               class = "dataset-vars")
    )
  )
}

#' UI for computed data
#'
#' @param id  Data ID
#'
#' @return UI for data
#'
dataComputedUI <- function(id, stat="identity") {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tagList(
    widgetHeader(),
    widgetBody(
      div(
        class = "dataset-vars",
        em("No computed variables available for this layer.", class = "none-computed hidden"),
        dndselectr::dragZone(
          id = ns('computeddatazone'),
          choices = dataInputChoices(stat_computed_vars[[stat]])
        )
      )
    )
  )
}

#' Server for data module
#'
#' @param input   Shiny inputs
#' @param output  Shiny outputs
#' @param session Shiny user session
#' @param dataset Passed in dataset for visualization
#'
#' @importFrom magrittr %>%
#' @import shiny
#'
dataServer <- function(input, output, session, dataset) {
  var_names <- names(dataset)

  output$dataset_vars <- renderUI({
    dndselectr::dragZone(
      id = session$ns('datazone'),
      choices = dataInputChoices(dataset)
    )
  })

  # load variable subset modules ====
  subset_args <- purrr::map(var_names, ~ callModule(module = dataVarServer,
                                                    id = .,
                                                    var = dataset[[.]]))

  # process subset arguments ====
  processed_args <- reactive({
    # Evaluate reactives
    args <- purrr::map(subset_args, ~ .())

    # Pull out the filter and mutate elements
    filter_args <- unlist(purrr::map(args, "filter"))
    mutate_args <- unlist(purrr::map(args, "mutate"))

    subset_data_code <- NULL

    # Build filter code
    if (length(filter_args)) {
      subset_data_code <- paste0("filter(",
                                 paste(filter_args, collapse = ", \n"),
                                 ")")
    }

    # Build mutate code
    if (length(mutate_args)) {
      subset_data_code <- paste(subset_data_code,
                                ifelse(length(filter_args), "%>%\n", ""),
                                paste0("mutate(",
                                       paste(mutate_args, collapse = ", \n"),
                                       ")"))
    }

    return(subset_data_code)
  })

  return(processed_args)
}

#' UI for data choices, either for dropzone or picker
#'
#' @param vars    Dataset, if not computed; otherwise, element of stat_computed_vars
#' @param zone    "varzone" or "aeszone"
#' @param inherited Array of variable names denoting inherited status (from base or stat)
#' @param session Shiny user session
#'
dataInputChoices <- function(vars = NULL, zone = "varzone", inherited = NULL, session = getDefaultReactiveDomain()) {
  if (is.null(vars)) {
    return(list())
  }

  computed <- is.null(names(vars))

  ns <- session$ns %||% function(x) { x }

  itemsUI <- sapply(names(vars) %||% vars, function(var_name) {
    div(
      class = paste(
        c(zone,
          switch(as.character(computed), "TRUE" = "computed", "FALSE" = dataTypeToUI(vars[[var_name]]))
        ), collapse = " "),
      switch(as.character(computed), "TRUE" = icon("calculator"), "FALSE" = dataTypeToUI(vars[[var_name]], .icon = TRUE)),
      span(var_name, class = paste(c("varname", switch(var_name %in% inherited, ifelse(computed, "default", "inherited"))), collapse = " ")),
      switch(!computed && zone == "varzone", # Include filter
             shinyWidgets::dropdownButton(
               dataVarUI(id = ns(var_name), var = vars[[var_name]]),
               inputId = ns("data-filter-btn"),
               status = "header-icon",
               icon = icon("filter"),
               size = "xs",
               right = TRUE,
               tooltip = shinyWidgets::tooltipOptions(title = "Filter")
             ))
    )
  }, simplify = FALSE, USE.NAMES = TRUE)

  if (computed) {
    names(itemsUI) <- paste0(computed_word, "(", vars, ")")
  }

  return(itemsUI)
}
