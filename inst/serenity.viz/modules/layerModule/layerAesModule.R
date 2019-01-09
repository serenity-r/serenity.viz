# UI ----

# Module UI function
layerAesUI <- function(id, bsa) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  # Hidden dropzone input for assigning aesthetic mapping
  title <- uiOutput(ns('aes_dropzone_ui'), inline = FALSE)

  # Visible aesthetic input - can be mapping or value
  content <- uiOutput(ns('aes_input_ui'), inline = FALSE)

  bsplus::bs_append(bsa, title = title, content = content)
}

# SERVER ----
layerAes <- function(input, output, session, layers_selected, geom_blank_input,
                     inherit.aes, default_aes) {
  # Get aesthetic from namespace
  aesthetic <- stringr::str_split(session$ns(''), '-')[[1]] %>% { .[length(.)-1] }

  # In order to update the mapping correctly, we need to know if there is
  #  a change in the truthiness of the mapping variable - e.g., going from
  #  specified mapping to no mapping
  mapping <- NULL
  triggerRenderMapping <- makeReactiveTrigger()
  observeEvent(input$dropzone, {
    if ((is.null(mapping) && !is.null(input$dropzone)) ||
        (is.null(input$dropzone) && !is.null(mapping))) {
          triggerRenderMapping$trigger()
    }
    mapping <<- input$dropzone
  }, ignoreNULL = FALSE)

  # _ Aesthetic dropzone ====
  output$aes_dropzone_ui <- renderUI({
    layers_selected()
    ns <- session$ns

    isolate({
      # Should not depend on any inputs
      dropZoneInput(ns("dropzone"),
                    choices = names(serenity.viz.data),
                    presets = input$dropzone,
                    hidden = TRUE,
                    placeholder = stringr::str_split(ns(''),'-')[[1]][5],
                    highlight = TRUE)
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
    layers_selected()
    triggerRenderMapping$depend()

    ns <- session$ns

    isolate({
      geom_blank_ns <- geom_blank_NS(ns)
      inherit <- (inherit.aes && isTruthy(geom_blank_input) &&
                  isTruthy(geom_blank_input[[geom_blank_ns("dropzone")]]) &&
                  isTruthy(geom_blank_input[[geom_blank_ns("dropzone")]]()))

      if (isTruthy(input$mapping) || isTruthy(mapping)) {
        # Mapping exists
        dropZoneInput(ns("mapping"),
                      choices = names(serenity.viz.data),
                      presets = input$mapping %||% mapping)
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

  # observe({
  #   if (is.null(input$lsuId) || input$lsuId == "") {
  #     shinyjs::disable("submit")
  #   } else {
  #     shinyjs::enable("submit")
  #   }
  # })

  # _ Entangle dropzone and mapping/input ====
  entangle(session, 'dropzone', 'mapping')

  # _ Aesthetic to code ====
  aesToCode <- reactive({
    NULL
  })

  return(aesToCode)
}

# UTILS ----

`%||%` <- function(a, b) if (!is.null(a)) a else b

`%T||%` <- function(a, b) if (isTruthy(a)) a else b

geom_blank_NS <- function(ns) {
  f <- function(id) {
    ns(id) %>%
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
           'linetype' = sliderInput(inputId = inputId,
                                    label = "",
                                    min = 0,
                                    max = 6,
                                    value = aes_val),
           ''
    ) %>%
      aes_wrap(default)
  )
}

