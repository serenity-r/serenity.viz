#' UI for layer aesthetic submodule
#'
#' @param id  ID of layer aesthetic
#' @param bsa Bootstrap accordian
#'
#' @return UI for layer aesthetic
#'
layerAesUI <- function(id, bsa) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  # Hidden dropzone input for assigning aesthetic mapping
  title <- uiOutput(ns('aes_dropzone_ui'), inline = FALSE)

  # Visible aesthetic input - can be mapping or value
  content <- uiOutput(ns('aes_input_ui'), inline = FALSE)

  bsplus::bs_append(bsa, title = title, content = content)
}

#' Server for layer aesthetic submodule
#'
#' @param input   Shiny inputs
#' @param output  Shiny outputs
#' @param session Shiny user session
#' @param triggerAesUpdate  Trigger update on layer change
#' @param geom_blank_input  Need geom_blank values to check for inheritance
#' @param inherit.aes Is this aesthetic inheritable?
#' @param default_aes Default value for aesthetic
#'
#' @importFrom magrittr %>%
#' @import shiny ggplot2
#'
layerAesServer <- function(input, output, session, triggerAesUpdate, geom_blank_input,
                           inherit.aes, default_aes, dataset) {
  # Get aesthetic from namespace
  aesthetic <- stringr::str_split(session$ns(''), '-')[[1]] %>% { .[length(.)-1] }
  layer <- paste(stringr::str_split(session$ns(''), '-')[[1]][2:3], collapse="-")

  # Convert default colour values to hex (if applicable)
  if ((aesthetic %in% c('colour', 'fill')) && isTruthy(default_aes)) {
    default_aes <- colour_to_hex(default_aes)
  }

  # Need a trigger for when to update aes_input_ui
  mapping_exists <- makeReactiveTrigger(!is.null(input$dropzone))
  observeEvent(input$dropzone, {
    if ((!mapping_exists$get() && !is.null(input$dropzone)) ||
        (is.null(input$dropzone) && mapping_exists$get())) {
          mapping_exists$trigger()
    }
    mapping_exists$set(!is.null(input$dropzone))
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  # _ Aesthetic dropzone ====
  output$aes_dropzone_ui <- renderUI({
    ns <- session$ns
    triggerAesUpdate()

    isolate({
      # Should not depend on any inputs
      dragulaSelectR::dropZoneInput(ns("dropzone"),
                                    choices = names(dataset),
                                    presets = input$mapping, # FIX: This doesn't work: input$dropzone %||% input$mapping
                                    hidden = TRUE,
                                    placeholder = stringr::str_split(ns(''),'-')[[1]][6],
                                    highlight = TRUE,
                                    maxInput = 1,
                                    replaceOnDrop = TRUE)
    })
  })

  # _ Aesthetic mapping/input ====
  # This can be
  #   (1) a dropzone for mapping variables,
  #   (2) a placeholder (if inherited), or
  #   (3) a shiny input when no mapping set
  #
  # Triggered when layer selected or
  output$aes_input_ui <- renderUI({
    ns <- session$ns
    triggerAesUpdate()
    mapping_exists$depend()

    isolate({
      geom_blank_ns <- geom_blank_NS(ns)
      inherit <- (inherit.aes && isTruthy(geom_blank_input) &&
                    isTruthy(geom_blank_input[[geom_blank_ns("dropzone")]]) &&
                    isTruthy(geom_blank_input[[geom_blank_ns("dropzone")]]()))

      if (isTruthy(input$mapping) || isTruthy(input$dropzone)) {
        # Mapping exists
        dragulaSelectR::dropZoneInput(ns("mapping"),
                                      choices = names(dataset),
                                      presets = input$mapping %||% input$dropzone,
                                      maxInput = 1,
                                      replaceOnDrop = TRUE)
      } else
        if (inherit) {
          # Inherited mappings override values
          create_aes_empty("inherited")
        } else {
          # Fall back on default values
          #   UI doesn't depend on value, so isolate
          ifelse(isTruthy(input$value) || isTruthy(default_aes),
                 create_aes_input(ns('value'),
                                  aesthetic,
                                  input$value %T||% default_aes
                 ),
                 create_aes_empty(aesthetic)
          )
        }
    })
  })

  # _ Make sure inputs always update ====
  outputOptions(output, "aes_input_ui", suspendWhenHidden = FALSE)

  # _ Entangle dropzone and mapping/input ====
  dragulaSelectR::entangle(session, 'dropzone', 'mapping')

  # _ Aesthetic to code ====
  aesToCode <- reactive({
    arg <- list(mappings = c(), values = c())
    if (!is.null(input$mapping)) {
      arg$mappings <- paste(aesthetic, "=", input$mapping)
    } else
      if (!is.null(input$value) && (input$value != default_aes)) {
        arg$values <- paste(aesthetic, "=",
                            switch(aesthetic,
                                  "colour" = ,
                                  "linetype" = ,
                                  "fill" = paste0('"', input$value, '"'),
                                  input$value)
                            )
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

# Create aesthetic input control
# aes_val is assumed to be truthy
#'
#' @param inputId
#' @param aes
#' @param aes_val
#' @param default
#'
#' @return
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
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
                                    choices = c("solid", "dashed", "longdash", "dotted", "twodash", "dotdash", "blank"),
                                    selected = aes_val),
           ''
    ) %>%
      aes_wrap(default)
  )
}

