# UI ----

# Module UI function
layerAesUI <- function(id, bsa) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  # Hidden dropzone input
  title <- dropZoneInput(ns('dropzone'),
                         choices = names(serenity.viz.data),
                         hidden = TRUE,
                         placeholder = id,
                         highlight = TRUE)

  # This can be
  #   (1) a dropzone for mapping variables,
  #   (2) a placeholder (if inherited), or
  #   (3) a shiny input when no mapping set
  content <- uiOutput(ns('aes_input'), inline = FALSE)

  bsplus::bs_append(bsa, title = title, content = content)
}

# SERVER ----
layerAes <- function(input, output, session) {
  # This contains the layer and aes id
  ns <- session$ns

  output$aes_input <- renderUI({
    aes_input <- dropZoneInput(ns('mapping'),
                               choices = names(serenity.viz.data))
    entangle(session, ns('dropzone'), ns('mapping'))
    aes_input
  })

  return(NULL)
}
