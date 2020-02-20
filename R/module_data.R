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
        em("No computed variables available", class = "none-computed hidden"),
        dndselectr::dragZone(
          id = ns('computeddatazone'),
          choices = computedDragZoneItems(stat)
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
      choices = sapply(var_names, function(var_name) {
        div(
          class = paste("varzone",
                        dataTypeToUI(dataset[[var_name]])),
          dataTypeToUI(dataset[[var_name]], .icon = TRUE),
          span(class = "varname", var_name),
          shinyWidgets::dropdownButton(
            dataVarUI(id = session$ns(var_name), var = dataset[[var_name]]),
            inputId = session$ns("data-filter-btn"),
            status = "header-icon",
            icon = icon("filter"),
            size = "xs",
            right = TRUE,
            tooltip = shinyWidgets::tooltipOptions(title = "Filter")
          )
        )
      }, simplify = FALSE, USE.NAMES = TRUE)
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

computedDragZoneItems <- function(stat="identity") {
  if (is.null(stat_computed_vars[[stat]])) {
    return(list())
  }

  sapply(stat_computed_vars[[stat]], function(var_name) {
    div(
      class = "varzone computed",
      icon("calculator"),
      span(class = "varname", var_name)
    )
  }, simplify = FALSE, USE.NAMES = TRUE) %>% {
    names(.) <- paste0("stat(", stat_computed_vars[[stat]], ")")
    .
  }
}
