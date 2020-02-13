layerParamsGeomBoxplotUI <- function(id) {
  ns <- NS(id)

  tagList(
    uiOutput(ns("params")),
    uiOutput(ns("outliers"))
  )
}

layerParamsGeomBoxplotServer <- function(input, output, session, base_data) {
  # This is reactive here since defaults can change when base_data changes
  default_args <- reactiveValues("notch" = FALSE,    # Show notches?
                                 "notchwidth" = 0.5, # Width of notches
                                 "varwidth" = FALSE, # Variable width boxes (weight by n)
                                 "outlier.show" = TRUE,
                                 "outlier.colour" = NA_defaults[["colour"]],
                                 "outlier.fill" = NA_defaults[["fill"]],
                                 "outlier.alpha" = NA_defaults[["alpha"]],
                                 "outlier.shape" = NA_defaults[["shape"]],
                                 "outlier.size" = NA_defaults[["size"]],
                                 "outlier.stroke" = NA_defaults[["stroke"]])

  # Note: Outlier aesthetics fall into 3 categories based on their behavior
  #  (1) colour, fill, alpha:  These three aesthetics do not have defaults and
  #      inherit their default values from the layer aesthetics (or NA_defaults).
  #  (2) shape, size:  These two aesthetics have default values.  In order to
  #      inherit aesthetics from the layer, they must be explicitly specified
  #      as NULL.
  #  (3) stroke:  This aesthetic doesn't have a layer aesthetic to inherit, but
  #      it does have a default value.
  outlier.aesthetics <- c("colour", "fill", "alpha", "shape", "size", "stroke")

  # Used for overrides
  outlier.state <- list(
    "outlier.shape" = NA_defaults[["shape"]],
    "outlier.size" = NA_defaults[["size"]]
  )

  # Update defaults for aesthetics on base_data change (colour, fill, alpha)
  observeEvent(base_data(), {
    for (aes in c("colour", "fill", "alpha")) {
      outlierId <- paste0("outlier.", aes)
      default_aes <- base_data()[[aes]]
      default_args[[outlierId]] <<- ifelse(length(unique(default_aes)) == 1,
                                                  switch(as.character(aes %in% c("colour", "fill")),
                                                         "TRUE" = colour_to_hex(unique(default_aes)),
                                                         "FALSE" = unique(default_aes)),
                                                  NA)
    }
  })

  output$params <- renderUI({
    isolate({
      tagList(
        div(
          class = "switch-numeric-input",
          div(
            class = "SNI-switch",
            shinyWidgets::switchInput(session$ns('notch'),
                                      label = 'Notches?',
                                      value = input[['notch']] %||% default_args[['notch']],
                                      onLabel = "Yes",
                                      offLabel = "No"
            )
          ),
          div(
            class = paste0("SNI-numeric", ifelse(default_args[['notch']], "", " hidden")),
            sliderInput(session$ns('notchwidth'),
                        label = 'Width',
                        value = input[['notchwidth']] %||% default_args[['notchwidth']],
                        min = 0,
                        max = 1,
                        step = 0.05
            )
          )
        ),
        checkboxInput(session$ns('varwidth'),
                      label = "Variable widths?",
                      value = input[['varwidth']] %||% default_args[['varwidth']]
        ),
        shinyWidgets::switchInput(session$ns('outlier.show'),
                                  label = 'Show outliers?',
                                  labelWidth = "100px",
                                  value = input[['outlier.show']] %||% default_args[['outlier.show']],
                                  onLabel = "Yes",
                                  offLabel = "No"
        )
      )
    })
  })

  output$outliers <- renderUI({
    req(!is.null(input$outlier.show))

    if (input$outlier.show) {
      isolate({
        tagList(
          h5("Outlier Aesthetics"),
          bs_accordion(session$ns("outlier_aesthetics")) %>%
            create_outlier_aes_input("colour", default_args$outlier.colour %T||% NA_defaults[["colour"]], input, session, collapsed = FALSE) %>%
            create_outlier_aes_input("fill", default_args$outlier.fill %T||% NA_defaults[["fill"]], input, session) %>%
            create_outlier_aes_input("shape", default_args$outlier.shape %T||% NA_defaults[["shape"]], input, session) %>%
            create_outlier_aes_input("size", default_args$outlier.size %T||% NA_defaults[["size"]], input, session) %>%
            create_outlier_aes_input("stroke", default_args$outlier.stroke %T||% NA_defaults[["stroke"]], input, session) %>%
            create_outlier_aes_input("alpha", default_args$outlier.alpha %T||% NA_defaults[["alpha"]], input, session)
        )
      })
    }
  })
  outputOptions(output, "params", suspendWhenHidden = FALSE)
  outputOptions(output, "outliers", suspendWhenHidden = FALSE)

  observeEvent(input$notch, {
    if (input$notch) {
      shinyjs::js$removeClass("hidden", paste0('#', session$ns("params"), ' .SNI-numeric'))
    } else {
      shinyjs::js$addClass("hidden", paste0('#', session$ns("params"), ' .SNI-numeric'))
    }
  })

  # Aesthetic observers to handle resetting to default values
  purrr::walk(outlier.aesthetics, ~ {
    resetId <- paste0("outlier_", ., "_reset")
    outlierId <- paste0("outlier.", .)
    inheritId <- paste0('outlier_', ., '_inherit')
    return({
      # Show or hide aesthetic value reset button
      observe({
        default_args_list <- reactiveValuesToList(default_args)
        req(!is.null(input[[inheritId]]),
            !is.null(input[[outlierId]]))
        if (!input[[inheritId]] &&
            ((is.na(default_args_list[[outlierId]]) && (input[[outlierId]] != NA_defaults[[.]])) ||
             ((!is.na(default_args_list[[outlierId]]) && (input[[outlierId]] != default_args_list[[outlierId]]))))) {
          shinyjs::show(resetId)
        } else {
          shinyjs::hide(resetId)
        }
      })

      # Reset aesthetic colour value to default
      observeEvent(input[[resetId]], {
        update_aes_input(session, outlierId, ., default_args[[outlierId]] %T||% NA_defaults[[.]])
      })
    })
  })

  # Observe events to handle inheritance for shape and size (sets to NULL)
  purrr::walk(c("shape", "size"), ~ {
    outlierId <- paste0("outlier.", .)
    inheritId <- paste0('outlier_', ., '_inherit')
    return(
      observeEvent(input[[inheritId]], {
        if (input[[inheritId]]) {
          outlier.state[[outlierId]] <<- input[[outlierId]]
          session$sendCustomMessage(type = "nullify", message = session$ns(outlierId))
        } else {
          update_aes_input(session, outlierId, ., outlier.state[[outlierId]])
        }
      })
    )
  })

  geom_params_code <- reactive({
    req(!is.null(input$outlier.show))

    default_args_list <- reactiveValuesToList(default_args)
    pos_outliers <- grepl("outlier", names(default_args_list))

    # First, no outliers
    processed_geom_params_code <- process_args(default_args_list[!pos_outliers], input, NULL)

    # Second, outliers only
    if (!is.null(input$outlier.show)) {
      if (input$outlier.show) {
        exclude_aesthetics <- purrr::map(c("colour", "fill", "alpha"), ~ {
          inheritId <- paste0("outlier_", ., "_inherit")
          outlierId <- paste0("outlier.", .)
          switch(isTruthy(input[[inheritId]]), outlierId)
        }) %>% unlist()
        allowNULL <- purrr::map(c("shape", "size"), ~ {
          inheritId <- paste0("outlier_", ., "_inherit")
          outlierId <- paste0("outlier.", .)
          switch(isTruthy(input[[inheritId]]), outlierId)
        }) %>% unlist()

        processed_geom_params_code <- process_args(default_args_list[pos_outliers][setdiff(names(default_args_list[pos_outliers]), exclude_aesthetics)],
                                                   input, NULL, modify_geom_boxplot_args, allowNULL) %>%
          {

            paste0(processed_geom_params_code,
                   ifelse(nchar(processed_geom_params_code) && nchar(.), ",\n", ""),
                   .)
          }
      } else {
        processed_geom_params_code <- paste0(processed_geom_params_code,
                                             ifelse(nchar(processed_geom_params_code), ",\n", ""),
                                             "outlier.shape = NA")
      }
    }

    return(processed_geom_params_code)
  })

  return(geom_params_code)
}

# Utilizing collapsed argument to work around bsplus bug (collapsed state isn't
#   initialized properly - updates immediately once accordian item is clicked)
create_outlier_aes_input <- function(bs_tag, aes, aes_default, input, session, collapsed = TRUE) {
  inheritId <- paste0('outlier_', aes, '_inherit')
  outlierId <- paste0('outlier.', aes)
  aesContent <- create_aes_input(session$ns(outlierId),
                                 aes,
                                 input[[outlierId]] %||% aes_default)
  title <- tagList(aes, icon(""))
  bs_tag <- bsplus::bs_append(bs_tag,
                              title,
                              content = tagList(
                                div(
                                  class = 'outlier-aes-header',
                                  shinyWidgets::materialSwitch(session$ns(inheritId),
                                                               "Inherit?",
                                                               input[[inheritId]] %||% ifelse(aes %in% c("colour", "fill", "alpha"), TRUE, FALSE),
                                                               status = "primary") %>%
                                    {
                                      if (aes == "stroke") .$attribs$style <- "visibility:hidden;"
                                      .
                                    },
                                  actionLink(session$ns(paste0('outlier_', aes, '_reset')),
                                             label = '',
                                             class = "reset-aes",
                                             style = ifelse(is.null(input[[inheritId]]) || isTruthy(input[[inheritId]]) || (input[[outlierId]] == aes_default), "display: none;", ""),
                                             icon = icon("undo"))
                                ),
                                conditionalPanel(
                                  condition = paste0("input.", inheritId, " === true"),
                                  ns = session$ns,
                                  span("Inheriting from boxplot aesthetics.")
                                ),
                                conditionalPanel(
                                  condition = paste0("input.", inheritId, " === false"),
                                  ns = session$ns,
                                  aesContent
                                )
                              )
  ) %>% {
    .$children[[length(.$children)]]$children[[1]]$attribs$class <- paste(c(.$children[[1]]$children[[1]]$attribs$class,
                                                                            switch(collapsed, "collapsed")),
                                                                          collapse = " ")
    .
  }
}

# For outlier shape and size, NULL should be explicitly specified
#  to inherit from layer.  Used in conjunction with the allowNULL
#  argument to process_args (by default, NULLs get stripped).
modify_geom_boxplot_args <- function(param, value, base_data) {
  return(
    switch(param,
           "outlier.shape" = ,
           "outlier.size" = ifelse(!is.null(value), value, "NULL"),
           value
    )
  )
}
