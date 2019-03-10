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
    fillCol(
      flex = c(NA, 1),
      div(
        class = "title",
        h4("Variables")
      ),
      miniUI::miniContentPanel(
        div(
          id = ns("dataset-vars-wrap"),
          class = "dataset-vars",
          uiOutput(ns("dataset_vars"), inline = FALSE)
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

  # Variable divs ====
  output$dataset_vars <- renderUI({
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
