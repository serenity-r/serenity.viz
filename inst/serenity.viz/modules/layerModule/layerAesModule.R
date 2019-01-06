# UI ----

# Module UI function
layerAesUI <- function(id, bsa) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  # Hidden dropzone input
  title <- uiOutput(ns('aes_dropzone'), inline = FALSE)

  # Visible aesthetic input
  content <- uiOutput(ns('aes_input'), inline = FALSE)

  bsplus::bs_append(bsa, title = title, content = content)
}

# SERVER ----
layerAes <- function(input, output, session) {
  output$aes_dropzone <- renderUI({
    ns <- session$ns
    dropZoneInput(ns('dropzone'),
                  choices = names(serenity.viz.data),
                  presets = input$dropzone,
                  hidden = TRUE,
                  placeholder = stringr::str_split(ns(''),'-')[[1]][5],
                  highlight = TRUE)
  })

  # This can be
  #   (1) a dropzone for mapping variables,
  #   (2) a placeholder (if inherited), or
  #   (3) a shiny input when no mapping set
  output$aes_input <- renderUI({
    ns <- session$ns
    dropZoneInput(ns('mapping'),
                  choices = names(serenity.viz.data),
                  presets = input$dropzone)
  })

  entangle(session, 'dropzone', 'mapping')

  aesToCode <- reactive({
    NULL
  })

  return(aesToCode)
}
