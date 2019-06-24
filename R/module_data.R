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
      uiOutput(ns("dataset_vars"), inline = FALSE)
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
    ns <- session$ns

    tagList(
      lapply(seq_along(var_names), function(var_num) {
        var_name <- var_names[var_num]
        dragulaSelectR::dragZone(
          id = ns(var_name),
          class = paste("varzone",
                        switch(class(dataset[[var_name]]), 'integer' =, 'numeric' = 'numeric', 'factor' = 'factor')),
          choices = setNames(list(
            tagList(
              switch(class(dataset[[var_name]]), 'integer' =, 'numeric' = icon("signal"), 'factor' = icon("shapes")),
              span(class = "varname", var_name),
              shinyWidgets::dropdownButton(
                dataVarUI(id = ns(var_name), var = dataset[[var_name]]),
                inputId = ns("data-filter-btn"),
                status = "header-icon",
                icon = icon("filter"),
                size = "xs",
                right = TRUE,
                tooltip = shinyWidgets::tooltipOptions(title = "Filter")
              )
            )
          ), var_name)
        )
      })
    )
  })

  # Variable divs ====
  output$dataset_vars_old <- renderUI({
    ns <- session$ns

    bsa <- bsplus::bs_accordion(id = ns("vars")) %>%
      bsplus::bs_set_opts(panel_type = "warning", use_heading_link = TRUE)
    lapply(seq_along(var_names), function(var_num) {
      cls <- paste0("grid var ", stringr::str_replace(var_names[var_num], '[.]', '-')) # var class name used to count # of elements for unique id creation
      title <- dragulaSelectR::dragZone(
        id = ns(var_names[var_num]),
        choices = setNames(list(
          div(class = "varname", var_names[var_num])
        ), var_names[var_num])
      )
      id <- var_names[var_num]
      content <- dataVarUI(id = ns(id),
                              var = dataset[[id]])
      bsa <<- bsplus::bs_append(bsa,
                                title = title,
                                content = content,
                                show = FALSE
      )
    })
    bsa
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
