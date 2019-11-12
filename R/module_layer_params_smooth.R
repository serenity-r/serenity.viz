layerParamsGeomSmoothUI <- function(id) {
  ns <- NS(id)

  uiOutput(ns("params"))
}

layerParamsGeomSmoothServer <- function(input, output, session, ggdata) {
  default_args <- list("method" = "auto",   # loess or gam
                       "se" = TRUE,         # Show confidence bands
                       "level" = 0.95,      # Confidence level
                       "fullrange" = FALSE, # Span full range of plot?
                       "family" = "gaussian")

  glm_link_functions <- list(
    "gaussian" = c("Identity" = "identity",
                   "Logarithmic" = "log",
                   "Inverse" = "inverse"),
    "binomial" = c("Logistic" = "logit",
                   "Normal" = "probit",
                   "Cauchy" = "cauchit",
                   "Logarithmic" = "log",
                   "Comp. Log-Log" = "cloglog"),
    "Gamma" = c("Inverse" = "inverse",
                "Identity" = "identity",
                "Logarithmic" = "log"),
    "poisson" = c("Logarithmic" = "log",
                  "Identity" = "identity",
                  "Square-root" = "sqrt")
  )

  output$params <- renderUI({
    isolate({
      tagList(
        selectInput(session$ns('method'),
                    label = 'Regression type',
                    choices = c("Auto" = "auto",
                                "Linear regression" = "lm",
                                "Generalized linear model" = "glm",
                                "Generalized additive model" = "gam",
                                "LOESS" = "loess"),
                    selected = input[['method']] %||% default_args[['method']]),
        conditionalPanel(
          condition = "input.method == 'glm'",
          class = "glm",
          ns = session$ns,
          selectInput(session$ns('family'),
                      label = 'Family',
                      choices = c(
                        "Gaussian" = "gaussian",
                        "Binomial" = "binomial",
                        "Gamma" = "Gamma",
                        "Poisson" = "poisson"
                      ),
                      selected = input[['family']] %||% default_args[['family']]),
          selectInput(session$ns('link'),
                      label = 'Link',
                      choices = glm_link_functions[[input[['family']] %||% default_args[['family']]]],
                      selected = input[['link']] %||% glm_link_functions[[input[['family']] %||% default_args[['family']]]][1])
        ),
        div(
          class = "switch-numeric-input",
          div(
            class = "SNI-switch",
            shinyWidgets::switchInput(session$ns('se'),
                                      label = 'Confidence bands?',
                                      value = input[['se']] %||% default_args[['se']],
                                      onLabel = "Yes",
                                      offLabel = "No"
            )
          ),
          div(
            class = paste0("SNI-numeric", ifelse(default_args[['se']], "", " hidden")),
            numericInput(session$ns('level'),
                         label = 'Level',
                         value = input[['level']] %||% default_args[['level']],
                         min = 0,
                         max = 1)
          )
        ),
        checkboxInput(session$ns('fullrange'),
                      label = 'Span full range of plot?',
                      value = input[['fullrange']] %||% default_args[['fullrange']])
      )
    })
  })

  observeEvent(input$family, {
    updateSelectInput(session, 'link',
                      choices = glm_link_functions[[input$family]],
                      selected = glm_link_functions[[input$family]][1]
    )
  })

  # _ Make sure params always update ====
  outputOptions(output, "params", suspendWhenHidden = FALSE)

  observeEvent(input$se, {
    if (input$se) {
      shinyjs::js$removeClass("hidden", paste0('#', session$ns("params"), ' .SNI-numeric'))
    } else {
      shinyjs::js$addClass("hidden", paste0('#', session$ns("params"), ' .SNI-numeric'))
    }
  })

  geom_params_code <- reactive({
    # Handle family separately
    args <- default_args[setdiff(names(default_args), "family")]

    processed_geom_params_code <- process_args(args, input, ggdata)

    if (input$method == "glm") {
      processed_geom_params_code <- paste(processed_geom_params_code,
                                          ifelse(nchar(processed_geom_params_code) > 0, ",\n", ""),
                                          paste0('method.args = list("family" = ',
                                                 input$family,
                                                 '(',
                                                 ifelse(input$link != glm_link_functions[[input$family]][1],
                                                        paste0('link = "', input$link, '"'),
                                                        ''),
                                                 '))')
      )
    }

    return(processed_geom_params_code)
  })

  return(geom_params_code)
}
