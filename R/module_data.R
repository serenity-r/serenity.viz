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
      uiOutput(ns("dataset_vars"))
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
dataServer <- function(input, output, session, dataset, layers) {
  var_names <- names(dataset)

  output$dataset_vars <- renderUI({
    tabsetPanel(
      type = "tabs",
      tabPanel(span(icon("database"), "Variables"),
               div(
                 class = "dataset-vars",
                 dndselectr::dragZone(
                   id = session$ns('datazone'),
                   choices = dataInputChoices(dataset)
                 )
               )),
      tabPanel(span(icon("calculator"), "Computed"),
               div(
                 class = "dataset-vars",
                 em("No computed variables available for this layer.", class = "none-computed hidden"),
                 dndselectr::dragZone(
                   id = session$ns('computeddatazone'),
                   choices = dataInputChoices(stat_computed_vars[["identity"]], type = "computed")
                 )
               )),
      tabPanel(span(icon("paint-brush"), "Aesthetics"),
               div(
                 class = "dataset-vars",
                 dndselectr::dragZone(
                   id = session$ns('aestheticsdatazone'),
                   choices = dataInputChoices(gg_aesthetics[["geom-blank"]], type = "aesthetics")
                 )
               ))
    )
  })

  # Handle stat changes
  observe({
    req(layers$selected_stat())
    dndselectr::updateDragZone(session,
                               id = session$ns("computeddatazone"),
                               choices = dataInputChoices(stat_computed_vars[[layers$selected_stat()]], type = "computed"))
    if (is.null(stat_computed_vars[[layers$selected_stat()]])) {
      shinyjs::js$removeClass("hidden", "em.none-computed")
    } else {
      shinyjs::js$addClass("hidden", "em.none-computed")
    }
  })

  # Handle aesthetic changes
  observe({
    req(layers$aesthetics())
    dndselectr::updateDragZone(session,
                               id = session$ns("aestheticsdatazone"),
                               choices = dataInputChoices(layers$aesthetics(), type = "aesthetics"))
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
#' @param default Array of variable names denoting default status (inherited from base or stat)
#' @param session Shiny user session
#'
dataInputChoices <- function(vars = NULL, zone = "varzone", type = "variables", default = NULL, session = getDefaultReactiveDomain()) {
  if (is.null(vars)) {
    return(list())
  }

  ns <- session$ns %||% function(x) { x }

  itemsUI <- sapply(names(vars) %||% vars, function(var_name) {
    div(
      class = paste(
        c(zone,
          type,
          switch(type == "variables", dataTypeToUI(vars[[var_name]]))
        ), collapse = " "),
      switch(type, "computed" = icon("calculator"), "aesthetics" = icon("paint-brush"), "variables" = dataTypeToUI(vars[[var_name]], .icon = TRUE)),
      span(var_name, class = paste(c("varname", switch(var_name %in% default, "default")), collapse = " ")),
      switch((type == "variables") && zone == "varzone", # Include filter
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

  return(itemsUI)
}
