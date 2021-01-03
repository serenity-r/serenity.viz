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
    uiOutput(ns('aes_content_ui'))
  )

  # Hidden dropzone input for assigning aesthetic mapping
  # title <- uiOutput(ns('aes_dropzone_ui'))

  # Visible aesthetic input - can be mapping or value
  # content <- uiOutput(ns('aes_input_ui'))
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
  entangled <- FALSE

  customized <- reactiveValues(mapping = "", values = "")

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

  # Reactive: inheritable ----
  # Inheritable mapping exists from base layer or stat
  inheritable <- reactive({
    list(
      base = (layer != "geom-blank") && inherit.aes() && (input$linked %||% TRUE),
      stat = (layer != "geom-blank") && rlang::is_quosure(default_stat_aes())
    )
  })

  # Capture intentional mapping change to stop auto-initialization on stat changes
  mapping_modified <- NULL
  observeEvent(input$mapping, {
    mapping_modified <<- !is.null(mapping_modified)
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

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
        ) %>% {
          .$attribs$class <- paste(c(.$attribs$class, "stagezone",
                                     switch(is.null(input$mapping), "hiddenn")), collapse=" ")
          .
        },
        div(
          class = paste(c("aes-select", switch((layer == "geom-blank") || is.null(default_geom_aes), "hidden")), collapse = " "),
          icon("database", class = ifelse(isTruthy(input$switch), 'inactive', '')),
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
            ),
            prettyToggle(
              inputId = session$ns("customize"),
              value = input$customize %||% FALSE,
              label_on = "",
              label_off = "",
              status_on = "default",
              status_off = "default",
              outline = TRUE,
              plain = TRUE,
              icon_on = icon("times"),
              icon_off = icon("pencil"),
              inline = TRUE
            )
          )
        )
      )
    })
  })
  outputOptions(output, "aes_header_ui", suspendWhenHidden = FALSE)

  # _ Aesthetic mapping/input ====
  # This can be
  #   (1) a dropzone for mapping variables,
  #   (2) a placeholder (if inherited), or
  #   (3) a shiny input when no mapping set
  output$aes_content_ui <- renderUI({
    req(!is.null(input$switch))

    aesUpdateDependency()

    isolate({
      if (inheritable()$base && !is.null(base_layer_mapping())) {
        init_mapping <- base_layer_mapping()
      } else
        if (isTruthy(input$mapping)) {
          init_mapping <- input$mapping
        } else
          if (inheritable()$stat && !isTruthy(mapping_modified) && rlang::is_quosure(default_stat_aes())) {
            init_mapping <- rlang::quo_name(default_stat_aes())
            shinyWidgets::updatePrettyToggle(session, 'linked', value = FALSE)
          } else {
            init_mapping <- NULL
          }

      init_value <- input$value %T||% default_geom_aes
      # Icons
      icons <- switch(required(), span(class = "required")) %>%
        div(class = "aes-content-icons")

      # Content
      if (!isTruthy(input$switch)) {
        # Mapping exists (or) first time loading
        content <- tagList(
          dndselectr::dropZoneInput(session$ns("mapping"),
                                    choices = c(
                                      dataInputChoices(dataset, zone="aeszone", inherited = switch(inheritable()$base, base_layer_mapping())),
                                      dataInputChoices(computed_vars(), zone="aeszone", type = "computed", inherited = switch(inheritable()$stat, strsplit(rlang::quo_name(default_stat_aes()), "[()]")[[1]][2])),
                                      dataInputChoices(switch(layer != "geom-blank", aesthetics()), zone="aeszone", type = "aesthetics")
                                    ),
                                    presets = init_mapping,
                                    placeholder = "Drag or select variable",
                                    maxInput = 1,
                                    replaceOnDrop = TRUE),
          shinyWidgets::pickerInput(
            inputId = session$ns("aes-choose-data"),
            label = NULL,
            selected = init_mapping,
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
              )
            ),
            options = list(
              title = "Nothing selected",
              size = 6,
              `live-search` = ifelse(length(c(names(dataset), computed_vars())) > 6, TRUE, FALSE),
              `dropup-auto` = FALSE
            )
          ) %>% {
            .$attribs$class <- paste(.$attribs$class, "aes-choose-data")
            .
          }
        )
      } else {
        # Should satisfy !is.null(default_geom_aes)
        content <- tagList(
          create_aes_input(session$ns('value'),
                           aesthetic,
                           init_value),
          actionButton(
            session$ns("aes-reset-value"),
            label = "",
            icon = icon("undo"),
            class = switch(init_value == default_geom_aes, "disabled")
          )
        )
      }
      content <- content %>%
        conditionalPanel(condition = "input.customize == null || input.customize == false",
                         ns = session$ns,
                         class = "aes-content")

      # Custom content
      if (!isTruthy(input$switch)) {
        # Mapping
        custom <- tagList(
          textInput(
            session$ns("custom_mapping"),
            label = "",
            value = input$custom_mapping %T||% init_mapping %T||% ""
          ),
          actionButton(
            session$ns("custom_mapping_ready"),
            label = "",
            icon = icon("check"),
            class = paste0(c("custom",
                             switch(
                               (is.null(input$custom_mapping) && (init_mapping == customized$mapping)) ||
                                 (!is.null(input$custom_mapping) && (input$custom_mapping == customized$mapping)),
                               "disabled"
                             )), collapse = " ")
          )
        )
      } else {
        custom <- tagList(
          textInput(
            session$ns("custom_value"),
            label = "",
            value = input$custom_value %T||% init_value %T||% default_geom_aes
          ),
          actionButton(
            session$ns("custom_value_ready"),
            label = "",
            icon = icon("check"),
            class = paste0(c("custom",
                             switch(
                               (is.null(input$custom_value) && (init_value == customized$value)) ||
                                 (!is.null(input$custom_value) && (input$custom_value == customized$value)),
                               "disabled"
                             )), collapse = " ")
          )
        )
      }
      custom <- custom %>%
        conditionalPanel(condition = "input.customize == true",
                         ns = session$ns,
                         class = "aes-custom-content")

      tags$section(
        class = ifelse(input$switch, 'value-section', 'mapping-section'),
        icons,
        content,
        custom
      )
    })
  })
  outputOptions(output, "aes_content_ui", suspendWhenHidden = FALSE)

  custom_server("mapping", input, customized, session)
  custom_server("value", input, customized, session)

  # Is mapping set?
  # mapping_set <- reactive({
  #   isTruthy(input$map_start) || isTruthy(input$map_after_stat) || isTruthy(input$map_after_scale)
  # })

  # Entangle aesthetic picker and dropzone
  observeEvent(input$`aes-choose-data`, {
    if (!entangled &&
        !isTRUE(all.equal(ifelse(is.null(input$`aes-choose-data`), "", input$`aes-choose-data`),
                          ifelse(is.null(input$mapping), "", input$mapping)))) {
      entangled <<- TRUE
      dndselectr::updateDropZoneInput(session, 'mapping', presets = input$`aes-choose-data` %T||% NA)
    } else {
      entangled <<- FALSE
    }
  }, ignoreInit = TRUE)
  observeEvent(input$mapping, {
    if (!entangled &&
        !isTRUE(all.equal(ifelse(is.null(input$`aes-choose-data`), "", input$`aes-choose-data`),
                          ifelse(is.null(input$mapping), "", input$mapping)))) {
      entangled <<- TRUE
      shinyWidgets::updatePickerInput(session, "aes-choose-data", selected = input$mapping %||% "")
    } else {
      entangled <<- FALSE
    }
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  observeEvent(input$switch, {
    if (input$switch) {
      shinyjs::js$addClass('inactive', paste(paste0('#', session$ns('aes_header_ui')), '.aes-select', '.fa-database'))
      shinyjs::js$removeClass('inactive', paste(paste0('#', session$ns('aes_header_ui')), '.aes-select', '.fa-sliders-h'))
    } else {
      shinyjs::js$removeClass('inactive', paste(paste0('#', session$ns('aes_header_ui')), '.aes-select', '.fa-database'))
      shinyjs::js$addClass('inactive', paste(paste0('#', session$ns('aes_header_ui')), '.aes-select', '.fa-sliders-h'))
    }
  }, ignoreInit = TRUE)

  # Show or hide aesthetic value reset button
  observe({
    req(!is.null(input$value))

    if (input$value != default_geom_aes) {
      shinyjs::js$removeClass("disabled", paste0('#', session$ns("aes-reset-value")))
    } else {
      shinyjs::js$addClass("disabled", paste0('#', session$ns("aes-reset-value")))
    }
  })

  # Reset aesthetic value to default
  observeEvent(input$`aes-reset-value`, {
    update_aes_input(session, 'value', aesthetic, default_geom_aes)
  })

  #### Handle linking ----

  # Show/Hide link button
  observe({
    req(!is.null(input$switch), !is.null(input$customize))

    if (inherit.aes() && !input$switch && !input$customize) {
      shinyjs::js$removeClass('hidden', paste0('#', session$ns("linked"), '-icon'))
    } else {
      shinyjs::js$addClass('hidden', paste0('#', session$ns("linked"), '-icon'))
    }
  })

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

# UTILS ----

`%||%` <- function(a, b) if (!is.null(a)) a else b

`%T||%` <- function(a, b) if (isTruthy(a)) a else b

aes_wrap <- function(content, class=NULL) {
  tagList(
    div(
      class = paste(c('aes-wrap', class), collapse = " "),
      content
    )
  )
}

create_aes_empty <- function(content='Not set', class=NULL) {
  tagList(
    span(
      content
    ) %>%
      aes_wrap(class)
  )
}

# Importing .data from rlang
# https://community.rstudio.com/t/how-to-solve-no-visible-binding-for-global-variable-note/28887

#' Colour translator
#'
#' @param col R color specification
#'
#' @importFrom rlang .data
colour_to_hex <- function(col) {
  if (!grepl("^#[0-9a-fA-F]{6}", col)) {
    return(farver::encode_colour(farver::decode_colour(col)))
  } else {
    return(col)
  }
}

# Linetype translator
#   Linetype goes from [0..6] to linetype name
linetype_choices <- c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash")
linetype_to_string <- function(linetype) {
  if (is.numeric(linetype) &&
      (linetype >= 0) && (linetype <= 6)) {
    return(linetype_choices[linetype+1])
  } else {
    return(linetype)
  }
}

# Create aesthetic input control
# aes_val is assumed to be truthy

#' Create aes inputs
#'
#' @param inputId Id of Shiny input
#' @param aes Name of aesthetic
#' @param aes_val Value of aesthetic
#' @param class Class to add to aesthetic wrapper div
#'
#' @importFrom magrittr %>%
create_aes_input <- function(inputId, aes, aes_val, class=NULL) {
  tagList(
    switch(aes,
           'shape' = sliderInput(inputId = inputId,
                                 label = "",
                                 min = 0,
                                 max = 25,
                                 step = 1,
                                 value = aes_val),
           'colour' = ,
           'fill' = colourpicker::colourInput(inputId = inputId,
                                              label = "",
                                              value = colour_to_hex(aes_val)),
           'weight' = ,
           'size' = ,
           'stroke' = ,
           'width' = ,
           'height' = sliderInput(inputId = inputId,
                                  label = "",
                                  min = 0.1,
                                  max = 10,
                                  step = 0.1,
                                  value = aes_val),
           'alpha' = sliderInput(inputId = inputId,
                                 label = "",
                                 min = 0,
                                 max = 1,
                                 value = aes_val),
           'linetype' = selectInput(inputId = inputId,
                                    label = "",
                                    choices = linetype_choices,
                                    selected = linetype_to_string(aes_val)),
           ''
    ) %>%
      aes_wrap(class)
  )
}

#' Update aes inputs
#'
#' @param session The session object passed to function given to shinyServer.
#' @param inputId Id of input object
#' @param aes Name of aesthetic
#' @param aes_val Value of aesthetic
#'
update_aes_input <- function(session, inputId, aes, aes_val) {
  switch(aes,
         'colour' = ,
         'fill' = colourpicker::updateColourInput(session, inputId, value = colour_to_hex(aes_val)),
         'alpha' = ,
         'shape' = ,
         'weight' = ,
         'size' = ,
         'stroke' = ,
         'width' = ,
         'height' = updateSliderInput(session, inputId, value = aes_val),
         'linetype' = updateSelectInput(session, inputId, selected = linetype_to_string(aes_val)),
         ''
  )
}

# Customize override server functions for mapping and value
custom_server <- function(type, input, customized, session) {
  return({
    # Let Enter key in custom_<type> input press custom_<type>_ready button
    shinyjs::onevent("keypress", paste0("custom_", type),
                     function(event) {
                       if (event$key == "Enter") {
                         shinyjs::click(paste0("custom_", type, "_ready"))
                       }
                     }
    )

    # Copy input to custom if appropriate
    observeEvent(input[[type]], {
      updateTextInput(session, paste0("custom_", type), value = input[[type]])
      customized[[type]] <<- input[[type]]
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    # Enable/disable custom ready button
    observeEvent(paste(input[[paste0("custom_", type)]], customized[[type]]), {
      if (input[[paste0("custom_", type)]] != customized[[type]]) {
        shinyjs::enable(paste0("custom_", type, "_ready"))
      } else {
        shinyjs::disable(paste0("custom_", type, "_ready"))
      }
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    # Equalize input on reset ready button
    observeEvent(input[[paste0("custom_", type, "_ready")]], {
      customized[[type]] <<- input[[paste0("custom_", type)]]
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
  })
}

stageUI <- function(type = "variable") {
  span(
    class=paste("fa-stack fa-2x stage", type),
    icon("square", class="fa-stack-2x"),
    icon(switch(type,
                "variable" = "database",
                "computed" = "calculator",
                "aesthetic" = "paint-brush"), class="fa-stack-1x snug")
  )
}

