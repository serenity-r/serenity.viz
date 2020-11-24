layerParamsGeomSegmentUI <- function(id) {
  ns <- NS(id)

  uiOutput(ns("params"))
}

# Refactor: Unnecessary plotting occurs sometimes on base_data change
layerParamsGeomSegmentServer <- function(input, output, session, base_data) {
  default_args <- reactiveValues("arrow" = NULL,
                                 "arrow.fill" = base_data()[["colour"]] %T||% NA_defaults[["colour"]],
                                 "linejoin" = "round")

  layer_data <- reactiveValues(arrow = NULL)

  reactive_inputs <- reactive({
    paste(
      input$arrow.fill,
      input$arrow_fill_inherit,
      input$linejoin,
      layer_data$arrow,
      default_args$arrow.fill
    )
  })

  # Update default for arrow.fill on base_data change
  observeEvent(base_data(), {
    default_aes <- base_data()[["colour"]]
    default_args[["arrow.fill"]] <<- ifelse(length(unique(default_aes)) == 1,
                                            colour_to_hex(unique(default_aes)),
                                            NA)
  }, priority = 1)

  # Initialize starting aes input
  observeEvent(base_data(), {
    update_aes_input(session, 'arrow.fill', 'fill', default_args[['arrow.fill']] %T||% base_data()[["colour"]])
  }, priority = 0, once = TRUE)


  output$params <- renderUI({
    isolate({
      tagList(
        arrowUI(session$ns("myarrow")),
        shinyWidgets::radioGroupButtons(
          session$ns("linejoin"),
          label = "Linejoin:",
          selected = input[['linejoin']] %||% default_args[['linejoin']],
          choices = c(`<div class='linejoin-icon round'></div>` = "round",
                      `<div class='linejoin-icon mitre'></div>` = "mitre",
                      `<div class='linejoin-icon bevel'></div>` = "bevel")
        ) %>% {
          .$attribs$class <- paste(.$attribs$class, "linejoin")
          .
        },
        tags$label("Arrow fill:", class = "control-label") %>% { .$attribs[['for']] = session$ns('arrow.fill'); . },
        div(
          class = "arrow-fill-content",
          create_arrow_fill_input(default_args$arrow.fill %T||% base_data()[["colour"]], input, session)
        )
      )
    })
  })
  outputOptions(output, "params", suspendWhenHidden = FALSE)

  arrow <- callModule(arrowServer, "myarrow")

  # Update arrow input
  observeEvent(arrow(), {
    layer_data$arrow <- arrow()
  })

  # BEGIN: Aesthetic observers to handle resetting to default values
  id <- 'arrow.fill'
  inheritId <- 'arrow_fill_inherit'
  resetId <- 'arrow_fill_reset'
  # Show or hide aesthetic value reset button
  observe({
    default_args_list <- reactiveValuesToList(default_args)
    req(!is.null(input[[inheritId]]),
        !is.null(input[[id]]))
    if (!input[[inheritId]] &&
        ((is.na(default_args_list[[id]]) && (input[[id]] != base_data()[["colour"]])) ||
         ((!is.na(default_args_list[[id]]) && (input[[id]] != default_args_list[[id]]))))) {
      shinyjs::show(resetId)
    } else {
      shinyjs::hide(resetId)
    }
  })

  # Reset aesthetic colour value to default
  observeEvent(input[[resetId]], {
    update_aes_input(session, id, 'fill', default_args[[id]] %T||% base_data()[["colour"]])
  })
  # END: Aesthetic observers to handle resetting to default values

  geom_params_code <- reactive({
    reactive_inputs()
    isolate({
      exclude_arrow_fill <- switch(isTruthy(input$arrow_fill_inherit), 'arrow.fill')
      input_list <- reactiveValuesToList(input)
      processed_geom_params_code <- process_args(reactiveValuesToList(default_args),
                                                 c(input_list[setdiff(names(input_list), exclude_arrow_fill)],
                                                   reactiveValuesToList(layer_data)),
                                                 NULL)
    })

    return(processed_geom_params_code)
  })

  return(geom_params_code)
}

# Mirrored after module_layer_params_geom_boxplot - consider refactoring and
#  creating a general UI and server to aesthetic with resets and inheritance
create_arrow_fill_input <- function(aes_default, input, session) {
  id <- 'arrow.fill'
  inheritId <- 'arrow_fill_inherit'
  aesContent <- create_aes_input(session$ns(id),
                                 'fill',
                                 input[[id]] %||% aes_default)
  tagList(
    div(
      class = 'outlier-aes-header',
      shinyWidgets::materialSwitch(session$ns(inheritId),
                                   "Inherit?",
                                   input[[inheritId]] %||% TRUE,
                                   status = "primary"),
      actionLink(session$ns('arrow_fill_reset'),
                 label = '',
                 class = "reset-aes",
                 style = ifelse(is.null(input[[inheritId]]) || isTruthy(input[[inheritId]]) || (input[[id]] == aes_default), "display: none;", ""),
                 icon = icon("undo"))
    ),
    conditionalPanel(
      condition = paste0("input.", inheritId, " === true"),
      ns = session$ns,
      span("Inheriting from segment colour aesthetic.")
    ),
    conditionalPanel(
      condition = paste0("input.", inheritId, " === false"),
      ns = session$ns,
      aesContent
    )
  )
}
