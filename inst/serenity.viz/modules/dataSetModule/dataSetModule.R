# UI ----

dataSetUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  div(
    id = ns("dataset-vars-wrap"),
    class = "dataset-vars",
    uiOutput(ns("dataset_vars"), inline = FALSE)
  )
}

# SERVER ----

dataSet <- function(input, output, session) {
  var_names <- names(serenity.viz.data)

  # _ Variable divs ====
  output$dataset_vars <- renderUI({
    ns <- session$ns

    bsa <- bsplus::bs_accordion(id = ns("vars")) %>%
      bsplus::bs_set_opts(panel_type = "warning", use_heading_link = TRUE)
    lapply(seq_along(var_names), function(var_num) {
      cls <- paste0("grid var ", stringr::str_replace(var_names[var_num], '[.]', '-')) # var class name used to count # of elements for unique id creation
      title <- div(
        id = ns(var_names[var_num]),
        class = cls,
        draggable = TRUE,
        div(class = "varname",
            `data-colnum` = var_num, # Do we need the data-colnum attribute?
            var_names[var_num]
        )
      )
      id <- var_names[var_num]
      content <- dataVarInput(id = ns(id),
                              var = serenity.viz.data[[id]])
      bsa <<- bsplus::bs_append(bsa,
                                title = title,
                                content = content
      )
    })
    bsa
  })

  # _ load variable subset modules ====
  subset_args <- purrr::map(var_names, ~ callModule(module = dataVar,
                                                    id = .,
                                                    var = serenity.viz.data[[.]]))

  # _ process subset arguments ====
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
