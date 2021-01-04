#' UI for layer aesthetic submodule
#'
#' @param id  ID of layer aesthetic
#'
#' @return UI for layer aesthetic
layerAesUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  div(
    class = "aesthetic",
    uiOutput(ns('aes_header_ui')),
    tagList(
      # Mappings
      conditionalPanel(condition = "input.switch == null || input.switch == false",
                       ns = session$ns,
                       tagList(
                         conditionalPanel(
                           condition = "input.stage == 'start'",
                           ns = session$ns,
                           layerAesStageUI(session$ns("start"))
                         ),
                         conditionalPanel(
                           condition = "input.stage == 'after_stat'",
                           layerAesStageUI(session$ns("after_stat"))
                         ),
                         conditionalPanel(
                           condition = "input.stage == 'after_scale'",
                           layerAesStageUI(session$ns("after_scale"))
                         )
                       )
      ),
      # Values
      conditionalPanel(condition = "input.switch == true",
                       ns = session$ns,
                       layerAesValueUI(session$ns('value'))
      )
    )
  )
}

#' Server for layer aesthetic submodule
#'
#' @param input   Shiny inputs
#' @param output  Shiny outputs
#' @param session Shiny user session
#' @param aesUpdateDependency  Trigger update on layer change
#' @param base_layer_mapping  Need base layer aesthetic mapping to use for inheritance
#' @param inherit.aes Reactive: Is this aesthetic inheritable?
#' @param default_geom_aes Default value for geom aesthetic
#' @param default_stat_aes Reactive value of default stat aesthetic - might be mapping!
#' @param required Reactive value of required status for aesthetic
#' @param dataset Dataset
#' @param computed_vars Reactive value of stat computed variables
#'
#' @importFrom magrittr %>%
#' @import shiny ggplot2
#'
layerAesServer <- function(input, output, session, aesUpdateDependency, base_layer_mapping,
                           inherit.aes, default_geom_aes, default_stat_aes, required,
                           dataset, computed_vars, aesthetics) {
  # Get aesthetic from namespace
  layer_info <- getLayerInfo(session$ns)
  aesthetic <- layer_info$aesthetic
  layer <- layer_info$geom

  output$aes_header_ui <- renderUI({
    aesUpdateDependency()

    isolate({
      tags$header(
        class = "aes-header",
        span(class = "aes-name", aesthetic),
        radioGroupButtons(
          "stages",
          choices = c(`<i class='fa fa-database'></i>` = "start",
                      `<i class='fa fa-calculator'></i>` = "after_stat",
                      `<i class='fa fa-paint-brush'></i>` = "after_scale")
        ),
        div(
          class = paste(c("aes-select", switch((layer == "geom-blank") || is.null(default_geom_aes), "hidden")), collapse = " "),
          shinyWidgets::prettySwitch(
            inputId = session$ns("switch"),
            label = '',
            value = isTruthy(input$switch),
            inline = TRUE
          ),
          icon("sliders-h", class = ifelse(!isTruthy(input$switch), 'inactive', '')) %>%
            {
              .$attribs$id <- session$ns("sliders-h")
              .
            }
        ),
        div(
          class = "header-icons",
          tagList(
            switch(as.character(layer != "geom-blank"),
                   "TRUE" = prettyToggle(
                     inputId = session$ns("linked"),
                     value = input$linked %||% TRUE,
                     label_on = "",
                     label_off = "",
                     status_on = "default",
                     status_off = "default",
                     outline = TRUE,
                     plain = TRUE,
                     icon_on = icon("link"),
                     icon_off = icon("unlink"),
                     inline = TRUE
                   ) %>% {
                     .$attribs$id <- paste0(session$ns("linked"), '-icon')
                     .$attribs$class <- paste(c(.$attribs$class, switch(!inherit.aes() || isTruthy(input$switch), 'hidden')), collapse = " ")
                     .
                   },
                   "FALSE" = prettyToggle(
                     inputId = session$ns("scale"),
                     label_on = "",
                     label_off = "",
                     status_on = "default",
                     status_off = "default",
                     outline = TRUE,
                     plain = TRUE,
                     icon_on = icon("times"),
                     icon_off = icon("ruler"),
                     inline = TRUE
                   ) %>% {
                     .$attribs$class <- paste(c(.$attribs$class, 'hidden', 'disabled'), collapse = " ")
                     .
                   }
            )
          )
        )
      )
    })
  })
  outputOptions(output, "aes_header_ui", suspendWhenHidden = FALSE)

  # reactive: Change inheritable status ----
  # Inheritable mapping exists from base layer or stat
  inheritable <- list(
    from_base = reactive({ (layer != "geom-blank") && inherit.aes() && (input$linked %||% TRUE) }),
    from_stat = reactive({ (layer != "geom-blank") && rlang::is_quosure(default_stat_aes()) })
  )

  # observeEvent: Trigger stage update on base linkage ----
  triggerStageUpdate <- makeReactiveTrigger()
  observeEvent(input$linked, {
    if (input$linked) {
      triggerStageUpdate$trigger()
    }
  }, ignoreInit = TRUE)

  mapping <- list(
    # callModule: Call start mapping module ----
    start = callModule(module = layerAesStageServer,
                       id = session$ns("start"),
                       stage = "start",
                       names = reactive({ names(dataset) }),
                       choices = reactive({
                         dataInputChoices(dataset, zone="aeszone", inherited = switch(inheritable$from_base(), base_layer_mapping$start()))
                       }),
                       preset = reactive({ switch(inheritable$from_base() && !is.null(base_layer_mapping$start()), base_layer_mapping$start()) }),
                       required = required
    ),
    # callModule: Call after_stat mapping module ----
    after_stat = callModule(module = layerAesStageServer,
                            id = session$ns("after_stat"),
                            stage = "after_stat",
                            names = computed_vars,
                            choices = reactive({
                              dataInputChoices(computed_vars(), zone="aeszone", type = "computed", inherited = switch(inheritable$from_stat(), strsplit(rlang::quo_name(default_stat_aes()), "[()]")[[1]][2]))
                            }),
                            preset = reactive({ switch(inheritable$from_stat() && rlang::is_quosure(default_stat_aes()), rlang::quo_name(default_stat_aes())) }),
                            required = required
    ),
    # callModule: Call after_scale mapping module ----
    after_scale = callModule(module = layerAesStageServer,
                             id = session$ns("after_scale"),
                             stage = "after_scale",
                             names = aesthetics,
                             choices = reactive({
                               dataInputChoices(switch(layer != "geom-blank", aesthetics()), zone="aeszone", type = "aesthetics")
                             }),
                             preset = reactive({ switch(inheritable$from_base() && !is.null(base_layer_mapping$scale()), base_layer_mapping$scale()) }),
                             required = required
    )
  )

  # Input checking for default_geom_aes ----

  # https://github.com/tidyverse/ggplot2/issues/4279
  # Remove when available on CRAN
  if (layer == "geom-polygon" && aesthetic == "colour") {
    default_geom_aes <- NA
  }

  if (!is.null(default_geom_aes)) {
    default_geom_aes <- ifelse(!is.na(default_geom_aes), default_geom_aes, NA_defaults[[aesthetic]])
  }

  # Convert default colour values to hex (if applicable)
  if ((aesthetic %in% c('colour', 'fill')) && isTruthy(default_geom_aes)) {
    default_geom_aes <- colour_to_hex(default_geom_aes)
  }

  # Convert default linetype values to string (if applicable)
  if ((aesthetic %in% c('linetype')) && isTruthy(default_geom_aes)) {
    default_geom_aes <- linetype_to_string(default_geom_aes)
  }

  # callModule: Call value module ----
  value <- callModule(module = layerAesValueServer,
                      id = session$ns("value"),
                      default_geom_aes = default_geom_aes,
                      required = required)

  observeEvent(input$switch, {
    if (input$switch) {
      shinyjs::js$addClass('inactive', paste(paste0('#', session$ns('aes_header_ui')), '.aes-select', '.fa-database'))
      shinyjs::js$removeClass('inactive', paste(paste0('#', session$ns('aes_header_ui')), '.aes-select', '.fa-sliders-h'))
    } else {
      shinyjs::js$removeClass('inactive', paste(paste0('#', session$ns('aes_header_ui')), '.aes-select', '.fa-database'))
      shinyjs::js$addClass('inactive', paste(paste0('#', session$ns('aes_header_ui')), '.aes-select', '.fa-sliders-h'))
    }
  }, ignoreInit = TRUE)

  # If linking turned on, set mapping to base layer
  observeEvent(input$linked, {
    req(inheritable()$base)

    dndselectr::updateDropZoneInput(session, 'mapping', presets = base_layer_mapping() %||% NA)
  }, ignoreInit = TRUE)

  # If mapping changed, deactivate if mapping no longer same as base
  observeEvent(input$mapping, {
    req(inheritable()$base && !identical(input$mapping, base_layer_mapping()))

    shinyWidgets::updatePrettyToggle(session, 'linked', value = FALSE)
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  #### Handle stat changes ----

  # Update dropZone on stat change
  observeEvent(computed_vars(), {
    dndselectr::updateDropZoneInput(session,
                                    inputId = 'mapping',
                                    choices = c(
                                      dataInputChoices(dataset, zone="aeszone", inherited = switch(inheritable()$base, base_layer_mapping())),
                                      dataInputChoices(computed_vars(), zone="aeszone", type = "computed", inherited = switch(inheritable()$stat, strsplit(rlang::quo_name(default_stat_aes()), "[()]")[[1]][2])),
                                      dataInputChoices(switch(layer != "geom-blank", aesthetics()), zone="aeszone", type = "aesthetics")
                                    )
    )
    shinyWidgets::updatePickerInput(session,
                                    inputId = 'aes-choose-data',
                                    choices = list(" " = "",
                                                   "Variables" = names(dataset),
                                                   "Computed" = switch(!is.null(computed_vars()),
                                                                       paste0("after_stat(", computed_vars(), ")")),
                                                   "Aesthetics" = switch(layer != "geom-blank",
                                                                         paste0("after_scale(", aesthetics(), ")"))
                                    ),
                                    choicesOpt = list(
                                      content = c(htmltools::doRenderTags(em("Clear variable")),
                                                  sapply(dataInputChoices(dataset, zone="aeszone", inherited = switch(inheritable()$base, base_layer_mapping())),
                                                         function(x) { htmltools::doRenderTags(x) }),
                                                  sapply(dataInputChoices(computed_vars(), zone="aeszone", type = "computed", inherited = switch(inheritable()$stat, strsplit(rlang::quo_name(default_stat_aes()), "[()]")[[1]][2])),
                                                         function(x) { htmltools::doRenderTags(x) }),
                                                  sapply(dataInputChoices(switch(layer != "geom-blank", aesthetics()), zone="aeszone", type = "aesthetics"),
                                                         function(x) { htmltools::doRenderTags(x) })
                                      )),
                                    selected = input$mapping  # pickerInput needs current selection
    )
  })

  # _ Aesthetic to code ====
  reactive_inputs <- reactive({
    paste(input$mapping,
          input$value,
          input$switch,
          input$customize,
          input$linked,
          customized$mapping,
          customized$value,
          inheritable(),
          base_layer_mapping())
  })

  aesToCode <- reactive({
    req(!is.null(input$switch))

    reactive_inputs()

    isolate({
      arg <- list(mappings = c(), values = c())
      if (!input$switch) {

        # Mapping
        if (isTruthy(input$customize) && (nchar(customized$mapping) > 0)) {
          # Custom override
          arg$mappings <- paste(aesthetic, "=", customized$mapping)
        } else if (!is.null(input$mapping) &&
                   ((layer == "geom-blank") ||
                    (!inheritable()$base && !inheritable()$stat) ||
                    (!inheritable()$base && inheritable()$stat &&
                     ((input$mapping != rlang::quo_name(default_stat_aes())) ||
                      (inherit.aes() && !is.null(base_layer_mapping())))
                    )
                   )) {
          # Set mapping (non-null)
          arg$mappings <- paste(aesthetic, "=",
                                ifelse(!stringr::str_detect(input$mapping, ' '),
                                       input$mapping,
                                       paste0("`", input$mapping, "`")))
        } else if (is.null(input$mapping) &&
                   ((!is.null(base_layer_mapping()) && inherit.aes() && !input$linked) ||
                    (is.null(base_layer_mapping()) && inheritable()$stat && inheritable()$base) ||
                    (!inheritable()$base && inheritable()$stat))) {
          # Set mapping to null
          arg$mappings <- paste(aesthetic, "= NULL")
        }
      } else {
        # Value
        if (isTruthy(input$customize) && (!is.null(customized$value) && (nchar(customized$value) > 0))) {
          # Custom override
          arg$values <- paste(aesthetic, "=",
                              switch(aesthetic,
                                     "colour" = ,
                                     "linetype" = ,
                                     "fill" = paste0('"', customized$value, '"'),
                                     customized$value)
          )
        } else if (!is.null(input$value) &&
                   ((input$value != default_geom_aes) ||
                    (inheritable()$base))) {
          # Set value (non-null)
          arg$values <- paste(aesthetic, "=",
                              switch(aesthetic,
                                     "colour" = ,
                                     "linetype" = ,
                                     "fill" = paste0('"', input$value, '"'),
                                     input$value)
          )
        }
      }
    })
    arg
  })

  return(aesToCode)
}
