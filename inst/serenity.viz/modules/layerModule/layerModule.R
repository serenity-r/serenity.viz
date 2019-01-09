# UI ----

layerUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  div(
    id = ns("layer-aes-wrap"),
    class = "layer-aes",
    uiOutput(ns("layer_aes"), inline = FALSE)
  )
}

# SERVER ----

layerMod <- function(input, output, session, layers_selected, geom_blank_input) {
  # UTILS ----

  # This contains the layer id
  ns <- session$ns

  # Get layer, geom, and aesthetics information
  layer_id <- gsub("-$", "", ns(''))
  geom_type <- paste(stringr::str_split(layer_id, '-')[[1]][1:2], collapse="-")
  geom_proto <- eval(parse(text=paste0(stringr::str_replace(geom_type, "-", "_"), "()")))
  if (geom_type == "geom-blank") {
    aesthetics <- gg_aesthetics[["default"]]
  } else {
    aesthetics <- geom_proto$geom$aesthetics()
  }

  # MAIN ----

  # _ Aesthetic divs ====
  output$layer_aes <- renderUI({
    layers_selected()

    ns <- session$ns
    bsa <- bsplus::bs_accordion(id = ns("aes")) %>%
      bsplus::bs_set_opts(panel_type = "success", use_heading_link = TRUE)
    lapply(aesthetics, function(aes) {
      bsa <<- layerAesUI(id = ns(aes), bsa)
    })
    bsa
  })

  # _ load variable subset modules ====
  layer_args <- purrr::map(aesthetics, ~ callModule(module = layerAes, id = .,
                                                    layers_selected,
                                                    geom_blank_input,
                                                    inherit.aes = geom_proto$inherit.aes,
                                                    default_aes = geom_proto$geom$default_aes[[.]]))

  # _ process subset arguments ====
  layer_code <- reactive({
    # Evaluate reactives
    args <- purrr::map(layer_args, ~ .())

    # processed_layers_code <- paste(purrr::map(geom_blank, ~ .()), collapse = " | ")
    processed_layers_code <- geom_type

    return(processed_layers_code)
  })

  return(layer_code)
}
