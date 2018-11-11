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

  # _ load variable modules ====
  filter_args <- sapply(var_names, function(var_name) {
    return(callModule(module = dataVar,
                      id = var_name,
                      var = serenity.viz.data[[var_name]]))
    }, simplify = FALSE, USE.NAMES = TRUE)

  # _ process filter arguments
  processed_args <- reactive({
    # Create list of filter arguments
    # Not sure why sapply with simplify = "array" isn't working
    args <- unlist(sapply(filter_args, function(filter_var) {
      return(filter_var())
    }))

    # Return filter argument
    if (length(args)) {
      return(paste0("filter(", paste(args, collapse = ", \n"), ")"))
    } else {
      return(NULL)
    }
  })

  return(processed_args)
}
