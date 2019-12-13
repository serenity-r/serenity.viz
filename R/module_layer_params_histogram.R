layerParamsGeomHistogramUI <- function(id) {
  ns <- NS(id)

  uiOutput(ns("params"))
}

layerParamsGeomHistogramServer <- function(input, output, session, base_data) {
  default_args <- list("bins_width" = TRUE,  # Number of bins (TRUE) or binwidth (FALSE)
                       "bins" = 30,          # Default number of bins
                       "binwidth" = NA,      # Width of bins
                       "breaks" = NA,        # Breaks (if not using bins or binwidth)
                       "closed" = TRUE,      # Bins closed on left (TRUE) or right (FALSE)
                       "use_breaks" = FALSE) # Don't use breaks by default

  # Not a built-in input (handled via datatable)
  layer_data <- reactiveValues(breaks = NULL)

  # Store non-reactive bin_width base_data - used for resetting in breaks
  bin_width_base_data <- NULL

  # Only these inputs should trigger an update to the layer code
  reactive_inputs <- reactive({
    paste(input$bins_width,
          input$bins,
          input$binwidth,
          layer_data$breaks,
          input$closed,
          input$use_breaks)
  })

  # Need this to trigger datatable when breaks are selected
  refreshDT <- makeReactiveTrigger()

  # Gets range data for bins
  rng <- reactive({
    c(base_data()[1, "xmin"], base_data()[nrow(base_data()), "xmax"])
  })

  output$params <- renderUI({
    isolate({
      if (isTruthy(base_data())) {
        # Set default binwidth
        if (is.na(default_args$binwidth)) {
          default_args$binwidth <- diff(rng())/default_args$bins
        }

        # UI
        tagList(
          div(
            class = "switch-numeric-input",
            div(
              class = "SNI-switch",
              shinyWidgets::switchInput(session$ns('bins_width'),
                                        label = 'Bins',
                                        value = input[['bins_width']] %||% default_args[['bins_width']],
                                        onLabel = "Number",
                                        offLabel = "Width",
                                        offStatus = "primary"
              )
            ),
            div(
              class = paste0("SNI-numeric bins", ifelse(default_args[['bins_width']], "", " hidden")),
              numericInput(session$ns('bins'),
                           label = '',
                           value = input[['bins']] %||% default_args[['bins']],
                           min = 1,
                           max = Inf)
            ),
            div(
              class = paste0("SNI-numeric binwidth", ifelse(default_args[['bins_width']], " hidden", "")),
              numericInput(session$ns('binwidth'),
                           label = '',
                           value = input[['binwidth']] %||% default_args[['binwidth']],
                           min = 0,
                           max = diff(rng())) # Might cause problems since isolated - wait and see
            )
          ),
          div(
            class = "inline-switch-no-label",
            tagList(
              span("Bins are"),
              shinyWidgets::switchInput(session$ns('closed'),
                                        value = input[['closed']] %||% default_args[['closed']],
                                        onLabel = "left",
                                        offLabel = "right",
                                        offStatus = "primary",
                                        labelWidth = "0",
                                        size = "mini",
                                        inline = TRUE),
              span("closed.")
            )
          ),
          checkboxInput(session$ns('use_breaks'),
                        label = "Specify breaks?",
                        value = input[['use_breaks']] %||% default_args[['use_breaks']]),
          wellPanel(
            class = paste(c("histogram_breaks_panel",
                            switch(!(input[['use_breaks']] %||% default_args[['use_breaks']]),
                                   "hidden")), collapse = " "),
            tagList(
              div(
                class = "histogram_breaks_icons",
                actionButton(session$ns('select_all_breaks'),
                             label = "",
                             icon = icon("check-square")
                ),
                actionButton(session$ns('unselect_all_breaks'),
                             label = "",
                             icon = icon("check-square"),
                             class = "regular"
                ),
                shinyjs::disabled(
                  actionButton(session$ns('add_breaks'),
                               label = "",
                               icon = icon("plus")
                  )
                ),
                shinyjs::disabled(
                  actionButton(session$ns('remove_breaks'),
                               label = "",
                               icon = icon("minus")
                  )
                ),
                actionButton(session$ns('get_breaks'),
                             label = "",
                             icon = icon("sync-alt")
                )
              ),
              DT::DTOutput(session$ns('breaks'))
            )
          )
        )
      } else {
        span("Please fix layer error before continuing.")
      }
    })
  })

  # _ Make sure params always update ====
  outputOptions(output, "params", suspendWhenHidden = FALSE)

  observeEvent(input$bins_width, {
    if (input$bins_width && isTruthy(input$binwidth)) {
      shinyjs::js$removeClass("hidden", paste0('#', session$ns("params"), ' .SNI-numeric.bins'))
      shinyjs::js$addClass("hidden", paste0('#', session$ns("params"), ' .SNI-numeric.binwidth'))
      updateNumericInput(session, 'bins',
                         value = round(diff(rng())/input$binwidth))
    } else
      if (!input$bins_width && isTruthy(input$bins)) {
        shinyjs::js$addClass("hidden", paste0('#', session$ns("params"), ' .SNI-numeric.bins'))
        shinyjs::js$removeClass("hidden", paste0('#', session$ns("params"), ' .SNI-numeric.binwidth'))
        updateNumericInput(session, 'binwidth',
                           value = diff(rng())/input$bins,
                           max = diff(rng()))
      }
  })

  output$breaks <- DT::renderDataTable({
    refreshDT$depend()
    layer_data$breaks},
    options = list(dom = "t",
                   ordering = FALSE,
                   paging = FALSE
    ),
    server = TRUE,
    editable = "cell",
    rownames = FALSE,
    colnames = NULL
  )
  outputOptions(output, "breaks", suspendWhenHidden = FALSE)

  observeEvent(input$use_breaks, {
    if (input$use_breaks) {
      shinyjs::disable("bins")
      shinyjs::disable("binwidth")
      shinyjs::js$addClass("disabled", paste0("#", session$ns("params"), " .SNI-switch .bootstrap-switch"))
      shinyjs::js$removeClass("hidden", '.histogram_breaks_panel')
      bin_width_base_data <<- base_data()
      refreshDT$trigger() # Need the trigger for some reason
    } else {
      shinyjs::enable("bins")
      shinyjs::enable("binwidth")
      shinyjs::js$removeClass("disabled", paste0("#", session$ns("params"), " .SNI-switch .bootstrap-switch"))
      shinyjs::js$addClass("hidden", '.histogram_breaks_panel')
    }
  })

  observeEvent(input$get_breaks, {
    if (is.null(layer_data$breaks)) {
      layer_data$breaks <- data.frame(
        breaks = rng() # Start with just boundaries
      )
    } else {
      layer_data$breaks <- data.frame(
        breaks = c(bin_width_base_data$xmin, bin_width_base_data[nrow(bin_width_base_data), "xmax"])
      )
    }
  }, ignoreNULL = FALSE, ignoreInit = TRUE) # Make sure it only runs once when NULL on init

  observeEvent(input$breaks_rows_selected, {
    # Only allow adding breaks when one row selected
    if (!is.null(input$breaks_rows_selected) &&
        (length(input$breaks_rows_selected) == 1) &&
        (input$breaks_rows_selected != nrow(layer_data$breaks))) {
      shinyjs::enable('add_breaks')
    } else {
      shinyjs::disable('add_breaks')
    }

    # Make sure you don't delete too many breaks - need at least 2
    if (!is.null(input$breaks_rows_selected) && (nrow(layer_data$breaks) - length(input$breaks_rows_selected) >= 2)) {
      shinyjs::enable('remove_breaks')
    } else {
      shinyjs::disable('remove_breaks')
    }
  }, ignoreNULL = FALSE)

  observeEvent(input$remove_breaks, {
    # Need to subset directly on column since subsetting a data frame with one
    #   column will return a vector
    layer_data$breaks <<- data.frame(
      breaks = layer_data$breaks$breaks[setdiff(1:nrow(layer_data$breaks),input$breaks_rows_selected)]
    )
  })

  addBreaksModal <- function(failed = FALSE) {
    modalDialog(
      numericInput(session$ns('num_breaks'),
                   "Add how many points?",
                   value = 1,
                   min = 1,
                   step = 1
      ),
      if (failed)
        div(tags$b("Please choose an integer greater than 0", style = "color: red;")),
      footer = tagList(
        modalButton("Cancel"),
        actionButton(session$ns("ok"), "OK")
      ),
      size = "s",
      easyClose = FALSE,
      fade = TRUE
    )
  }

  observeEvent(input$add_breaks, {
    showModal(addBreaksModal())
  })

  observeEvent(input$ok, {
    if (is.integer(input$num_breaks) && input$num_breaks >= 1) {
      # Selected row will NOT be last, so we know we have row+1
      row <- input$breaks_rows_selected
      layer_data$breaks <<- data.frame(
        breaks = c(layer_data$breaks$breaks[1:row],
                   seq(from = layer_data$breaks$breaks[row],
                       to = layer_data$breaks$breaks[row+1],
                       length.out = input$num_breaks + 2)[2:(input$num_breaks+1)],
                   layer_data$breaks$breaks[(row+1):nrow(layer_data$breaks)])
      )
      removeModal()
    } else {
      showModal(addBreaksModal(failed = TRUE))
    }
  })

  observeEvent(input$select_all_breaks, {
    DT::dataTableProxy('breaks') %>%
      DT::selectRows(1:nrow(layer_data$breaks))
  })

  observeEvent(input$unselect_all_breaks, {
    DT::dataTableProxy('breaks') %>%
      DT::selectRows(NULL)
  })

  observeEvent(input$breaks_cell_edit, {
    row <- input$breaks_cell_edit$row
    value <- as.numeric(input$breaks_cell_edit$value)

    # Make sure edited value lies between left and right break points
    if (((row == 1) || (layer_data$breaks$breaks[row-1] < value)) &&
        ((row == nrow(layer_data$breaks)) || (value < layer_data$breaks$breaks[row+1]))) {
      # Could just do this manually
      layer_data$breaks <<- DT::editData(layer_data$breaks,
                                         input$breaks_cell_edit,
                                         'breaks',
                                         rownames = FALSE)
    } else {
      # Need to refresh client table with server info
      refreshDT$trigger()
    }
  })

  # This reactive needs to be isolated since we have inputs that don't need
  #   to rerun this code (e.g. buttons in breaks panel).
  geom_params_code <- reactive({
    reactive_inputs()
    isolate({
      processed_geom_params_code <- ''
      if (!is.null(input$bins_width)) {
        args <- default_args[setdiff(names(default_args),
                                     c("bins_width",
                                       "use_breaks",
                                       switch(as.character(input$use_breaks),
                                              "TRUE" = c("binwidth", "bins"),
                                              "FALSE" = c("breaks", ifelse(input$bins_width, "binwidth", "bins")))))]

        processed_geom_params_code <- process_args(args,
                                                   c(reactiveValuesToList(input),
                                                     reactiveValuesToList(layer_data)),
                                                   base_data)
      }
    })

    return(processed_geom_params_code)
  })

  return(geom_params_code)
}
