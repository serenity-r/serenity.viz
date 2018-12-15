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

layerMod <- function(input, output, session) {

  # This contains the layer id
  ns <- session$ns

  # Get layer, geom, and aesthetics information
  layer_id <- gsub("-$", "", ns(''))
  geom_type <- paste(stringr::str_split(layer_id, '-')[[1]][1:2], collapse="-")
  aesthetics <- eval(parse(text=paste0(stringr::str_replace(geom_type, "-", "_"), "()")))$geom$aesthetics()

  # _ Aesthetic divs ====
  #
  # Depends:
  #   geom_type()
  #
  output$layer_aes <- renderUI({
    # Only want aesthetics UI dependent on layer changes
    # Individual outputs have their own updating functions
    isolate({
      bsa <- bsplus::bs_accordion(id = n("acc")) %>%
        bsplus::bs_set_opts(panel_type = "success", use_heading_link = TRUE)
      lapply(aesthetics, function(aes) {
        dropzoneId <- paste0(aes, '-dropzone-', layer_id)
        var_name <- input[[dropzoneId]]
        if (!is.null(var_name)) {
          # Check if mapping is actually the variable name
          content <- div(id = paste0(var_name,'-map-1'), # Only 1 for now until facet wrap added
                         class = paste0('grid map ', var_name),
                         draggable = TRUE,
                         div(class = 'varname',
                             `data-colnum` = 1,
                             var_name
                         )
          )
        }
        bsa <<- bsplus::bs_append(bsa,
                                  title = dropZoneInput(
                                    inputId = paste0(aes, '-dropzone-', layer_id),
                                    class = "grid",
                                    div(id = aes,
                                        class = "aesname",
                                        aes
                                    )
                                  ),
                                  content = content
        )
      })
    })
    bsa
  })

  # _ load variable subset modules ====
  subset_args <- purrr::map(var_names, ~ callModule(module = dataVar,
                                                    id = .,
                                                    var = serenity.viz.data[[.]]))

  # _ process subset arguments ====
  processed_args <- reactive({
    # Evaluate reactives
    args <- purrr::map(subset_args, ~ .())

    # Pull out the filter and mutate elements
    filter_args <- unlist(map(args, "filter"))
    mutate_args <- unlist(map(args, "mutate"))

    subset_data_code <- NULL

    # Build filter code
    if (length(filter_args)) {
      subset_data_code <- paste0("filter(",
                                 paste(filter_args, collapse = ", \n"),
                                 ")")
    }

    # Build mutate code
    if (length(mutate_args)) {
      subset_data_code <- paste(subset_data_code,
                                ifelse(length(filter_args), "%>%\n", ""),
                                paste0("mutate(",
                                       paste(mutate_args, collapse = ", \n"),
                                       ")"))
    }

    return(subset_data_code)
  })

  return(processed_args)
}
