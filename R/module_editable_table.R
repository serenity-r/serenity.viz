editableTableUI <- function(id, refreshIcon = "sync-alt") {
  ns <- NS(id)

  div(
    id = ns('editable_table'),
    class = "editable_table_panel",
    tags$head(includeCSS(file.path(resourcePath, "css", "editable_table.css"))),
    shinyjs::useShinyjs(),
    div(
      class = "editable_table_icons",
      actionButton(ns('select_all_values'),
                   label = "",
                   icon = icon("check-square")
      ),
      actionButton(ns('unselect_all_values'),
                   label = "",
                   icon = icon("check-square"),
                   class = "regular"
      ),
      actionButton(ns('add_values'),
                   label = "",
                   icon = icon("plus")
      ),
      shinyjs::disabled(
        actionButton(ns('remove_values'),
                     label = "",
                     icon = icon("minus")
        )
      ),
      actionButton(ns('get_values'),
                   label = "",
                   icon = icon(refreshIcon)
      )
    ),
    DT::DTOutput(ns('table'))
  )
}

editableTableServer <- function(input, output, session = getDefaultReactiveDomain(),
                                init = reactive({ numeric(0) }),
                                refreshDT = makeReactiveTrigger(),
                                unique_values = FALSE,
                                min_values = 0,
                                default_num = 2,
                                default_from = reactive({ 0 }),
                                default_to = reactive({ 1 })) {
  # Not a built-in input (handled via datatable)
  table_data <- reactiveValues(values = data.frame(values=numeric(0)))

  output$table <- DT::renderDataTable({
    refreshDT$depend()
    table_data$values},
    options = list(dom = "t",
                   ordering = FALSE,
                   paging = FALSE
    ),
    server = TRUE,
    editable = "cell",
    rownames = FALSE,
    colnames = NULL
  )
  outputOptions(output, "table", suspendWhenHidden = FALSE)

  observeEvent(input$get_values, {
    table_data$values <<- data.frame(
      values = init()
    )
  }, ignoreNULL = FALSE)

  observeEvent(input$table_rows_selected, {
    # Make sure you don't delete too many values
    if (!is.null(input$table_rows_selected) && (nrow(table_data$values) - length(input$table_rows_selected) >= min_values)) {
      shinyjs::enable('remove_values')
    } else {
      shinyjs::disable('remove_values')
    }
  }, ignoreNULL = FALSE)

  observeEvent(input$remove_values, {
    # Need to subset directly on column since subsetting a data frame with one
    #   column will return a vector
    table_data$values <<- data.frame(
      values = table_data$values$values[setdiff(1:nrow(table_data$values), input$table_rows_selected)]
    )
  })

  addValuesModal <- function(init_num = default_num,
                             init_from = switch(as.character(num_selected()),
                                                "0" = default_from(),
                                                table_data$values$values[sort(input$table_rows_selected)[1]]),
                             init_to = switch(as.character(num_selected()),
                                              "0" = ,
                                              "1" = default_to(),
                                              table_data$values$values[sort(input$table_rows_selected)[num_selected()]]),
                             include_from = (num_selected() == 0),
                             include_to = (num_selected() <= 1),
                             failed = list(integer = FALSE,
                                           numeric = FALSE,
                                           range = FALSE)) {
    modalDialog(
      numericInput(session$ns('num_values'),
                   "Add how many points?",
                   value = init_num,
                   min = 1,
                   step = 1
      ),
      div(
        class = "add-values-inline-form",
        numericInput(session$ns('from_values'),
                   "From:",
                   value = init_from),
        checkboxInput(session$ns('include_from'),
                      "Include?",
                      value = include_from)
      ),
      div(
        class = "add-values-inline-form",
        numericInput(session$ns('to_values'),
                     "To:",
                     value = init_to),
        checkboxInput(session$ns('include_to'),
                      "Include?",
                      value = include_to)
      ),
      if (failed$integer)
        div(tags$b("Please choose an integer greater than 0", style = "color: red;")),
      if (failed$numeric)
        div(tags$b("Please enter a number in the From and To fields", style = "color: red;")),
      if (failed$range)
        div(tags$b("Please make sure To is greater than or equal to From", style = "color: red;")),
      h5("Values to be added (except those highlighted in red):"),
      wellPanel(
        class = "show-from-to-values",
        htmlOutput(session$ns('show_from_to_values'))
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton(session$ns("ok"), "OK")
      ),
      size = "s",
      easyClose = FALSE,
      fade = TRUE
    )
  }

  mod_num_values <- reactive({
    input$num_values + as.integer(!input$include_from) + as.integer(!input$include_to)
  })

  from_to_values <- reactive({
    req(input$from_values,
        input$to_values,
        input$num_values,
        is.integer(input$num_values) && input$num_values >= 1,
        is.numeric(input$from_values) && is.numeric(input$to_values),
        input$from_values <= input$to_values)

    seq(from = input$from_values,
        to = input$to_values,
        length.out = mod_num_values())
  })

  from_to_values_new <- reactive({
    from_to_values()[ifelse(input$include_from, 1, 2):ifelse(input$include_to, mod_num_values(), mod_num_values()-1)] %>%
      { switch(as.character(unique_values), "TRUE" = dsetdiff(unique(.), table_data$values$values), "FALSE" = .) }
  })

  output$show_from_to_values <- renderText({
    paste(purrr::map_chr(from_to_values(),
                         ~ ifelse(. %din% from_to_values_new(), ., paste0("<span style='color:red'>", ., "</span>"))),
          collapse = " ")
  })

  num_selected <- reactive({
    length(input$table_rows_selected)
  })

  observeEvent(input$add_values, {
    showModal(addValuesModal())
  })

  observeEvent(input$ok, {
    # Handle error checking
    failed <- list(integer = !is.integer(input$num_values) || input$num_values < 1,
                   numeric = !is.numeric(input$from_values) || !is.numeric(input$to_values),
                   range = is.numeric(input$from_values) && is.numeric(input$to_values) && (input$from_values > input$to_values))

    if (any(unlist(failed))) {
      showModal(addValuesModal(failed = failed)
      )
    } else {
      table_data$values <<- data.frame(
        values = sort(c(table_data$values$values, from_to_values_new()))
      )
      removeModal()
    }
  })

  observeEvent(input$select_all_values, {
    DT::dataTableProxy('table') %>%
      DT::selectRows(1:nrow(table_data$values))
  })

  observeEvent(input$unselect_all_values, {
    DT::dataTableProxy('table') %>%
      DT::selectRows(NULL)
  })

  observeEvent(input$table_cell_edit, {
    row <- input$table_cell_edit$row
    value <- as.numeric(input$table_cell_edit$value)

    if (!unique_values || !(value %din% table_data$values$values)) {
      table_data$values <<- DT::editData(table_data$values,
                                         input$table_cell_edit,
                                         'values',
                                         rownames = FALSE) %>%
        dplyr::arrange(values)
    } else {
      # Need to refresh client table with server info
      refreshDT$trigger()
    }
  })

  return(reactive({ table_data$values }))
}
