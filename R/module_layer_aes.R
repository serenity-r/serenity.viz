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
#' @param geom_blank_input  Need geom_blank values to check for inheritance
#' @param inherit.aes Reactive: Is this aesthetic inheritable?
#' @param default_aes Default value for aesthetic
#' @param dataset Dataset
#' @param renderNum Closure used to mark number of times aesthetic is rendered
#'
#' @importFrom magrittr %>%
#' @import shiny ggplot2
#'
layerAesServer <- function(input, output, session, aesUpdateDependency, geom_blank_input,
                           inherit.aes, default_aes, dataset, renderNum) {
  # Get aesthetic from namespace
  aesthetic <- stringr::str_split(session$ns(''), '-')[[1]] %>% { .[length(.)-1] }
  layer <- paste(stringr::str_split(session$ns(''), '-')[[1]][2:3], collapse="-")
  geom_blank_ns <- geom_blank_NS(session$ns)
  entangled <- FALSE

  # Convert default colour values to hex (if applicable)
  if ((aesthetic %in% c('colour', 'fill')) && isTruthy(default_aes)) {
    default_aes <- colour_to_hex(default_aes)
  }

  # Convert default linetype values to string (if applicable)
  if ((aesthetic %in% c('linetype')) && isTruthy(default_aes)) {
    default_aes <- linetype_to_string(default_aes)
  }

  # Inheritable mapping exists
  inheritable <- reactive({
    inherit.aes() && isTruthy(geom_blank_input) &&
      isTruthy(geom_blank_input[[geom_blank_ns("mapping")]]) &&
      isTruthy(geom_blank_input[[geom_blank_ns("mapping")]]())
  })

  output$aes_header_ui <- renderUI({
    aesUpdateDependency()

    isolate({
      tags$header(
        class = "aes-header",
        span(class = "aes-name", aesthetic),
        div(
          class = paste(c("aes-select", switch((layer == "geom-blank") || is.null(default_aes), "hidden")), collapse = " "),
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
        switch(as.character(layer != "geom-blank"),
               "TRUE" = serenity.viz::prettyToggle(
                 inputId = session$ns("customize"),
                 label_on = "",
                 label_off = "",
                 status_on = "default",
                 status_off = "default",
                 outline = TRUE,
                 plain = TRUE,
                 icon_on = icon("times"),
                 icon_off = icon("pencil"),
                 inline = TRUE
               ),
               "FALSE" = serenity.viz::prettyToggle(
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
    renderNum$nextNum()

    isolate({
      init_mapping <- input$mapping %T||% switch(inheritable() && (renderNum$getNum() == 1), geom_blank_input[[geom_blank_ns("mapping")]](), NULL)
      # Icons
      if (!isTruthy(input$switch)) {
        icons <- tagList(
          actionLink(session$ns("aes-reset-mapping"),
                     label = '',
                     style = ifelse(!inheritable() || (!is.null(init_mapping) && (init_mapping == geom_blank_input[[geom_blank_ns("mapping")]]())), "display: none;", ""),
                     icon = icon("undo")
          )
        )
      } else {
        if (isTruthy(default_aes)) {
          icons <- tagList(
            actionLink(session$ns("aes-reset-value"),
                       label = '',
                       style = ifelse(is.na(default_aes) || (input$value == default_aes), "display: none;", ""),
                       icon = icon("undo")
            )
          )
        } else {
          icons <- NULL
        }
      }
      icons <- icons %>% div(class = "aes-content-icons")

      # Content
      if (!isTruthy(input$switch)) {
        # Mapping exists (or) first time loading
        content <- tagList(
          dragulaSelectR::dropZoneInput(session$ns("mapping"),
                                        choices = sapply(names(dataset), function(var_name) {
                                          div(
                                            class = paste("aeszone",
                                                          dataTypeToUI(dataset[[var_name]])),
                                            dataTypeToUI(dataset[[var_name]], .icon = TRUE),
                                            span(class = "varname", var_name)
                                          )
                                        }, simplify = FALSE, USE.NAMES = TRUE),
                                        presets = init_mapping,
                                        placeholder = "Drag or select variable",
                                        maxInput = 1,
                                        replaceOnDrop = TRUE),
          shinyWidgets::pickerInput(
            inputId = session$ns("aes-choose-data"),
            label = NULL,
            selected = init_mapping,
            choices = c("", names(dataset)),
            choicesOpt = list(
              content = c(htmltools::doRenderTags(em("Clear variable")),
                          sapply(names(dataset), function(var_name) {
                            htmltools::doRenderTags(
                              div(
                                class = paste("aeszone",
                                              dataTypeToUI(dataset[[var_name]])),
                                dataTypeToUI(dataset[[var_name]], .icon = TRUE),
                                span(class = "varname", var_name)
                              )
                            )
                          })
              )),
            options = list(
              title = "Nothing selected",
              size = 6,
              `live-search` = ifelse(length(names(dataset)) > 6, TRUE, FALSE),
              `dropup-auto` = FALSE
            )
          ) %>% {
            .$attribs$class <- paste(.$attribs$class, "aes-choose-data")
            .
          },
          span(
            class = paste(c("aes-content-inherited", switch(inheritable() && !is.null(input$mapping) && (input$mapping == geom_blank_input[[geom_blank_ns("mapping")]]()), "inherited")), collapse = " ")
          )
        )
      } else {
        # Should satisfy !is.null(default_aes)
        content <- create_aes_input(session$ns('value'),
                                    aesthetic,
                                    isolate(input$value) %T||% ifelse(!is.na(default_aes), default_aes, NA_defaults[[aesthetic]]))
      }
      content <- content %>% div(class = "aes-content")

      # Custom content
      if (!isTruthy(input$switch)) {
      }

      tags$section(
        class = ifelse(input$switch, 'value-section', 'mapping-section'),
        icons,
        content
      )
    })
  })
  outputOptions(output, "aes_content_ui", suspendWhenHidden = FALSE)

  # Entangle aesthetic picker and dropzone
  observeEvent(input$`aes-choose-data`, {
    if (!entangled &&
        !isTRUE(all.equal(ifelse(is.null(input$`aes-choose-data`), "", input$`aes-choose-data`),
                          ifelse(is.null(input$mapping), "", input$mapping)))) {
      entangled <<- TRUE
      dragulaSelectR::updateDropZoneInput(session, 'mapping', presets = input$`aes-choose-data` %T||% NA)
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
    req(!is.null(input$value) && !is.na(default_aes))

    if (input$value != default_aes) {
      shinyjs::show("aes-reset-value")
    } else {
      shinyjs::hide("aes-reset-value")
    }
  })

  # Show or hide aesthetic mapping reset button
  observe({
    req(!is.null(inheritable()))

    if (inheritable() && (is.null(input$mapping) || (input$mapping != geom_blank_input[[geom_blank_ns("mapping")]]()))) {
      shinyjs::show("aes-reset-mapping")
      shinyjs::js$removeClass('inherited', paste(paste0('#', session$ns('aes_content_ui')), ' > section.mapping-section span.aes-content-inherited'))
    } else {
      shinyjs::hide("aes-reset-mapping")
      shinyjs::js$addClass('inherited', paste(paste0('#', session$ns('aes_content_ui')), ' > section.mapping-section span.aes-content-inherited'))
    }
  })

  # Reset aesthetic value to default
  observeEvent(input$`aes-reset-value`, {
    update_aes_input(session, 'value', aesthetic, default_aes)
  })

  # Reset aesthetic mapping to base layer (default)
  observeEvent(input$`aes-reset-mapping`, {
    dragulaSelectR::updateDropZoneInput(session, 'mapping', presets = geom_blank_input[[geom_blank_ns("mapping")]]())
  })

  # _ Aesthetic to code ====
  reactive_inputs <- reactive({
    paste(input$mapping,
          input$value,
          input$switch,
          input$customize,
          input$custom_ready,
          inheritable())
  })

  aesToCode <- reactive({
    req(!is.null(input$switch))

    reactive_inputs()

    isolate({
      arg <- list(mappings = c(), values = c())
      if (!is.null(input$switch)) {
        if (!input$switch) {
          if (!is.null(input$mapping) &&
              ((layer == "geom-blank") ||
               !inheritable() ||
               (inheritable() && (input$mapping != geom_blank_input[[geom_blank_ns("mapping")]]())))) {
            arg$mappings <- paste(aesthetic, "=",
                                  ifelse(!stringr::str_detect(input$mapping, ' '),
                                         input$mapping,
                                         paste0("`", input$mapping, "`")))
          } else if (is.null(input$mapping) && inheritable()) {
            arg$mappings <- paste(aesthetic, "= NULL")
          }
        } else
          if (!is.null(input$value)) {
            if (is.na(default_aes) ||
                (input$value != default_aes) ||
                (inheritable())) {
              arg$values <- paste(aesthetic, "=",
                                  switch(aesthetic,
                                         "colour" = ,
                                         "linetype" = ,
                                         "fill" = paste0('"', input$value, '"'),
                                         input$value)
              )
            }
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

geom_blank_NS <- function(ns) {
  f <- function(id) {
    ns(id) %>%
    { stringr::str_replace(., paste0(stringr::str_split(., '-')[[1]][1], '-'), '') } %>%
    { stringr::str_replace(., stringr::str_split(., '-')[[1]][2], 'blank') } %>%
    { stringr::str_replace(., stringr::str_split(., '-')[[1]][4], '1') }
  }
  return(f)
}

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

# Set color palette rosetta stone
# http://www.melissaclarkson.com/resources/R_guides/documents/colors_Ver2.pdf
crgb <- col2rgb(cc <- colors())
colnames(crgb) <- cc
colours_tbl <- dplyr::tbl_df(t(crgb)) %>%
  dplyr::mutate(name = cc,
                hex = rgb(red, green, blue, maxColorValue = 255)) %>%
  dplyr::select(name, hex, red, green, blue)

# Importing .data from rlang
# https://community.rstudio.com/t/how-to-solve-no-visible-binding-for-global-variable-note/28887

#' Colour translator
#'
#' @param col R color specification
#'
#' @importFrom rlang .data
colour_to_hex <- function(col) {
  if (!grepl("^#[0-9a-fA-F]{6}", col)) {
    return(dplyr::filter(colours_tbl, .data$name == col)$hex)
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
           'stroke' = sliderInput(inputId = inputId,
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
         'stroke' = updateSliderInput(session, inputId, value = aes_val),
         'linetype' = updateSelectInput(session, inputId, selected = linetype_to_string(aes_val)),
         ''
  )
}
