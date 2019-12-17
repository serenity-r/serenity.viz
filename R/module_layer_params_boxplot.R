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
                                 "coef" = 1.5,       # Whisker length as multiple of IQR
                                 "outlier.show" = TRUE,
                                 "outlier.colour" = NA_defaults[["colour"]],
                                 "outlier.fill" = NA_defaults[["fill"]],
                                 "outlier.alpha" = NA_defaults[["alpha"]],
                                 "outlier.shape" = NA_defaults[["shape"]],
                                 "outlier.size" = NA_defaults[["size"]],
                                 "outlier.stroke" = NA_defaults[["stroke"]])

  # Update defaults for colour, fill, and alpha on base_data change
  observeEvent(base_data(), {
    default_args[["outlier.colour"]] <<- ifelse(length(unique(base_data()$colour)) == 1,
                                                colour_to_hex(unique(base_data()$colour)),
                                                NA)
    default_args[["outlier.fill"]] <<- ifelse(length(unique(base_data()$fill)) == 1,
                                              colour_to_hex(unique(base_data()$fill)),
                                              NA)
    default_args[["outlier.alpha"]] <<- ifelse((length(unique(base_data()$alpha)) == 1) && !is.na(base_data()$alpha),
                                               base_data()$alpha,
                                               NA)
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
        numericInput(session$ns('coef'),
                     label = "Whisker length (x IQR)",
                     value = input[['coef']] %||% default_args[['coef']],
                     min = 0,
                     max = Inf
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
          bsplus::bs_accordion(session$ns("outlier_aesthetics")) %>%
            bsplus::bs_set_opts("default", use_heading_link = TRUE) %>% {
              .$attribs$class <- paste(.$attribs$class, "outlier-aesthetics")
              .
            } %>%
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

  # _ Make sure params always update ====
  outputOptions(output, "params", suspendWhenHidden = FALSE)
  outputOptions(output, "outliers", suspendWhenHidden = FALSE)

  observeEvent(input$notch, {
    if (input$notch) {
      shinyjs::js$removeClass("hidden", paste0('#', session$ns("params"), ' .SNI-numeric'))
    } else {
      shinyjs::js$addClass("hidden", paste0('#', session$ns("params"), ' .SNI-numeric'))
    }
  })

  purrr::walk(c("colour", "fill", "alpha", "shape", "size", "stroke"), ~ {
    resetId <- paste0("outlier_", ., "_reset")
    outlierId <- paste0("outlier.", .)
    inheritId <- paste0('outlier_', ., '_inherit')
    return({
      # Show or hide aesthetic value reset button
      observe({
        default_args_list <- reactiveValuesToList(default_args)
        req(!is.null(input[[inheritId]]))
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

  geom_params_code <- reactive({
    default_args_list <- reactiveValuesToList(default_args)
    pos_outliers <- grepl("outlier", names(default_args_list))

    # First, no outliers
    processed_geom_params_code <- process_args(default_args_list[!pos_outliers], input, NULL)

    # Second, outliers only
    if (!is.null(input$outlier.show)) {
      if (input$outlier.show) {
        processed_geom_params_code <- process_args(default_args_list[pos_outliers][setdiff(names(default_args_list[pos_outliers]),
                                                                                           c(switch(isTruthy(input$outlier_colour_inherit), "outlier.colour"),
                                                                                             switch(isTruthy(input$outlier_fill_inherit), "outlier.fill"),
                                                                                             switch(isTruthy(input$outlier_alpha_inherit), "outlier.alpha")
                                                                                             ))], input, modify_geom_boxplot_args) %>% {
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
                                                               status = "primary"),
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

modify_geom_boxplot_args <- function(param, value, base_data) {
  return(
    switch(param,
           "outlier.shape" = ,
           "outlier.size" = ,
           "outlier.stroke" = switch(!is.na(value), value),
           value
    )
  )
}
