#' UI for layer aesthetic submodule
#'
#' @param id  ID of layer aesthetic
#'
#' @return UI for layer aesthetic
layerAesUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  uiOutput(ns('aes_ui'),
           class = 'aesthetic')

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
#' @param triggerAesUpdate  Trigger update on layer change
#' @param geom_blank_input  Need geom_blank values to check for inheritance
#' @param inherit.aes Reactive: Is this aesthetic inheritable?
#' @param default_aes Default value for aesthetic
#' @param dataset Dataset
#' @param renderNum Closure used to mark number of times aesthetic is rendered
#'
#' @importFrom magrittr %>%
#' @import shiny ggplot2
#'
layerAesServer <- function(input, output, session, triggerAesUpdate, geom_blank_input,
                           inherit.aes, default_aes, dataset, renderNum) {
  # Get aesthetic from namespace
  aesthetic <- stringr::str_split(session$ns(''), '-')[[1]] %>% { .[length(.)-1] }
  layer <- paste(stringr::str_split(session$ns(''), '-')[[1]][2:3], collapse="-")
  geom_blank_ns <- geom_blank_NS(session$ns)

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

  # _ Aesthetic mapping/input ====
  # This can be
  #   (1) a dropzone for mapping variables,
  #   (2) a placeholder (if inherited), or
  #   (3) a shiny input when no mapping set
  #
  # Triggered when layer selected or
  output$aes_ui <- renderUI({
    ns <- session$ns
    triggerAesUpdate()
    input$switch
    renderNum$nextNum()

    isolate({
      if (!isTruthy(input$switch) || (input$switch == FALSE)) {
        # Mapping exists (or) first time loading
        content <- dragulaSelectR::dropZoneInput(ns("mapping"),
                                                 choices = sapply(names(dataset), function(var_name) {
                                                   div(
                                                     class = paste("aeszone",
                                                                   switch(class(dataset[[var_name]]), 'integer' =, 'numeric' = 'numeric', 'factor' = 'factor')),
                                                     switch(class(dataset[[var_name]]), 'integer' =, 'numeric' = icon("signal"), 'factor' = icon("shapes")),
                                                     span(class = "varname", var_name)
                                                   )
                                                 }, simplify = FALSE, USE.NAMES = TRUE),
                                                 presets = input$mapping %T||% switch(inheritable() && (renderNum$getNum() == 2), geom_blank_input[[geom_blank_ns("mapping")]](), NULL),
                                                 placeholder = "Drag or select variable",
                                                 maxInput = 1,
                                                 replaceOnDrop = TRUE)
      } else {
          # Fall back on default values
          #   UI doesn't depend on value, so isolate

        content <- ifelse(isTruthy(input$value) || isTruthy(default_aes),
                          create_aes_input(ns('value'),
                                           aesthetic,
                                           input$value %T||% default_aes
                          ),
                          create_aes_empty(aesthetic)
        )
      }

      tmp <- icon("sliders-h", class = ifelse(input$switch %T||% FALSE, '', 'inactive'))
      tmp$attribs$id <- ns("sliders-h")
      tagList(
        tags$header(
          class = "aes-header",
          span(class = "aes-name", aesthetic),
          div(
            class = "aes-select",
            icon("database", class = ifelse(input$switch %T||% FALSE, 'inactive', '')),
            shinyWidgets::prettySwitch(
              inputId = ns("switch"),
              label = '',
              value = input$switch %T||% FALSE,
              inline = TRUE
            ),
            tmp
          ),
          shinyWidgets::dropdownButton(
            HTML("Hello, World!"),
            inputId = ns("aes-settings-btn"),
            status = "header-icon",
            icon = icon("gear"),
            size = "xs",
            right = TRUE,
            tooltip = shinyWidgets::tooltipOptions(title = "Aesthetic Settings", placement = "left"))
        ),
        tags$section(
          class = ifelse(input$switch, 'value-section', 'mapping-section'),
          uiOutput(ns('aes-section-header')),
          content
        )
      )
    })
  })

  # Can't isolate majority of this or get infinite loop with mapping inputs
  output$`aes-section-header` <- renderUI({
    ns <- session$ns
    input$switch
    inherit.aes()

    if (!input$switch %||% FALSE) {
      tagList(
        shinyWidgets::dropdown(
          shinyWidgets::pickerInput(
            inputId = ns("aes-choose-data"),
            label = "Select variable",
            selected = input$`aes-choose-data` %T||% NULL, # Can't isolate this for some reason
            choices = names(dataset),
            choicesOpt = list(
              content = sapply(names(dataset), function(var_name) {
                htmltools::doRenderTags(
                  div(
                    class = paste("aeszone",
                                  switch(class(dataset[[var_name]]), 'integer' =, 'numeric' = 'numeric', 'factor' = 'factor')),
                    switch(class(dataset[[var_name]]), 'integer' =, 'numeric' = icon("signal"), 'factor' = icon("shapes")),
                    span(class = "varname", var_name)
                  )
                )
              })
            ),
            options = list(
              title = "Nothing selected",
              size = 6,
              `live-search` = ifelse(length(names(dataset)) > 6, TRUE, FALSE),
              `dropup-auto` = FALSE
            )
          ),
          inputId = ns("aes-choose-dropdown"),
          status = "header-icon",
          size = "xs",
          right = TRUE,
          tooltip = shinyWidgets::tooltipOptions(title = "Choose variables", placement = "left")
        ),
        actionLink(ns("aes-reset-mapping"),
                   label = '',
                   style = ifelse(isolate(!inheritable() || (!is.null(input$mapping) && (input$mapping == geom_blank_input[[geom_blank_ns("mapping")]]()))), "display: none;", ""),
                   icon = icon("undo"))
      )
    } else {
      actionLink(ns("aes-reset-value"),
                 label = '',
                 style = ifelse(isolate(input$value == default_aes), "display: none;", ""),
                 icon = icon("undo"))
    }
  })

  # Entangle aesthetic picker and dropzone
  observeEvent(input$`aes-choose-data`, {
    ns <- session$ns
    if (!isTRUE(all.equal(ifelse(is.null(input$`aes-choose-data`), "", input$`aes-choose-data`),
                          ifelse(is.null(input$mapping), "", input$mapping)))) {
      dragulaSelectR::updateDropZoneInput(session, 'mapping', presets = input$`aes-choose-data` %||% NA)
    }
  }, ignoreNULL = FALSE)
  observeEvent(input$mapping, {
    ns <- session$ns
    if (!isTRUE(all.equal(ifelse(is.null(input$`aes-choose-data`), "", input$`aes-choose-data`),
                          ifelse(is.null(input$mapping), "", input$mapping)))) {
      shinyWidgets::updatePickerInput(session, "aes-choose-data", selected = input$mapping %||% "")
    }
  }, ignoreNULL = FALSE)

  # Not sure why shinyjs::toggleClass, addClass or removeClass doesn't work here
  observe({
    req(!is.null(input$switch))
    ns <- session$ns

    if (input$switch) {
      # shinyjs::addClass(class = 'inactive', selector = paste(paste0('#', ns('aes_ui')), '.aes-select', '.fa-database'))
      # shinyjs::removeClass(class = 'inactive', selector = paste(paste0('#', ns('aes_ui')), '.aes-select', '.fa-sliders-h'))
      shinyjs::js$addClass('inactive', paste(paste0('#', ns('aes_ui')), '.aes-select', '.fa-database'))
      shinyjs::js$removeClass('inactive', paste(paste0('#', ns('aes_ui')), '.aes-select', '.fa-sliders-h'))
    } else {
      # shinyjs::removeClass(class = 'inactive', selector = paste(paste0('#', ns('aes_ui')), '.aes-select', '.fa-database'))
      # shinyjs::addClass(class = 'inactive', selector = paste(paste0('#', ns('aes_ui')), '.aes-select', '.fa-sliders-h'))
      shinyjs::js$removeClass('inactive', paste(paste0('#', ns('aes_ui')), '.aes-select', '.fa-database'))
      shinyjs::js$addClass('inactive', paste(paste0('#', ns('aes_ui')), '.aes-select', '.fa-sliders-h'))
    }
  })

  # Show or hide aesthetic value reset button
  observe({
    req(!is.null(input$value))

    if (input$value != default_aes) {
      shinyjs::show("aes-reset-value")
    } else {
      shinyjs::hide("aes-reset-value")
    }
  })

  # Show or hide aesthetic mapping reset button
  observe({
    if (inheritable() && (is.null(input$mapping) || (input$mapping != geom_blank_input[[geom_blank_ns("mapping")]]()))) {
      shinyjs::show("aes-reset-mapping")
    } else {
      shinyjs::hide("aes-reset-mapping")
    }
  })

  # Reset aesthetic value to default
  observeEvent(input$`aes-reset-value`, {
    update_aes_input(session, 'value', aesthetic, default_aes)
  })

  # Reset aesthetic mapping to base layer (default)
  observeEvent(input$`aes-reset-mapping`, {
    dragulaSelectR::updateDropZoneInput(session, 'mapping', geom_blank_input[[geom_blank_ns("mapping")]]())
  })

  # _ Make sure inputs always update ====
  outputOptions(output, "aes_ui", suspendWhenHidden = FALSE)

  # _ Aesthetic to code ====
  aesToCode <- reactive({
    req(!is.null(input$switch))

    arg <- list(mappings = c(), values = c())
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
        if ((input$value != default_aes) ||
            (inheritable())) { #  && (!is.null(input$mapping))
          arg$values <- paste(aesthetic, "=",
                              switch(aesthetic,
                                     "colour" = ,
                                     "linetype" = ,
                                     "fill" = paste0('"', input$value, '"'),
                                     input$value)
          )
        }
      }
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

aes_wrap <- function(content, default='') {
  tagList(
    div(
      class = paste0('aes-wrap ', default),
      content
    )
  )
}

create_aes_empty <- function(default='') {
  tagList(
    span(
      'Not set'
    ) %>%
      aes_wrap(default)
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

# Colour translator
#   Right now assume col is an R colour
colour_to_hex <- function(col) {
  if (!grepl("^#[0-9a-fA-F]{6}", col)) {
    return(dplyr::filter(colours_tbl, name == col)$hex)
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
#' @param default Not used
#'
#' @importFrom magrittr %>%
create_aes_input <- function(inputId, aes, aes_val, default='') {
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
      aes_wrap(default)
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
