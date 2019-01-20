# This stores returned reactives from layer modules
layer_modules <- reactiveValues()

# Render ----------------------

# _ Subsetted data ====
subsetted_data <- callModule(module = dataSet,
                             id = attributes(serenity.viz.data)$df_name)

# _ ggplot code ====
output$aesthetics <- renderUI({
  layerUI(id = input$layers_selected)
})

# Get the names of the visible layers
visible_layers <- reactive({
  setdiff(input$layers, input$layers_invisible)
})

# Preps geom_blank dropzone inputs for layer modules
geom_blank_inputs_to_reactives <- function() {
  geom_blank_inputs <- as.list(paste0('geom-blank-ds-1-', gg_aesthetics[["default"]], '-dropzone'))
  names(geom_blank_inputs) <- paste0('geom-blank-ds-1-', gg_aesthetics[["default"]], '-dropzone')
  if (any(names(geom_blank_inputs) %in% names(input))) {
    return(geom_blank_inputs %>%
             purrr::map(~ reactive({ input[[.]] })))
  } else {
    return(NULL)
  }
}

# Update layer module output reactives - create only once!
observeEvent(input$layers, {
  # Adding new layers
  purrr::map(setdiff(input$layers, names(layer_modules)), ~ { layer_modules[[.]] <- callModule(module = layerMod, id = .,
                                                                                               reactive({input$layers_selected}),
                                                                                               geom_blank_inputs_to_reactives())} )
  # Remove old layers
  purrr::map(setdiff(names(layer_modules), input$layers), ~ { layer_modules[[.]] <- NULL })
})

# Get layer code
layer_code <- reactive({
  paste(purrr::map(reactiveValuesToList(layer_modules)[visible_layers()], ~ .()), collapse = "+\n")
})

# _ Aesthetic divs ====
#
# Depends:
#   geom_type()
#
# output$aesthetics <- renderUI({
#   layer_id()
#
#   # Only want aesthetics UI dependent on layer changes
#   # Individual outputs have their own updating functions
#   isolate({
#     bsa <- bsplus::bs_accordion(id = "acc") %>%
#       bsplus::bs_set_opts(panel_type = "success", use_heading_link = TRUE)
#     lapply(aesthetics(), function(aes) {
#       # Main ggplot2 object -> Mapping only!
#       if (geom_type() == "geom-blank") {
#         # Is aesthetic already set to a mapping?
#         if (rlang::is_quosure(mapping()[[aes]])) {
#           var_name <- as.character(rlang::get_expr(mapping()[[aes]]))
#           content <- div(id = paste0(var_name,'-map-1'), # Only 1 for now until facet wrap added
#                          class = paste0('grid map ', var_name),
#                          draggable = TRUE,
#                          div(class = 'varname',
#                              `data-colnum` = 1,
#                              var_name
#                          )
#           )
#         } else {
#           content <- span(
#             'Not set'
#           )
#         }
#       } else {
#         var_name <- NULL
#
#         # Check layer mapping first
#         if ((length(mapping()) > 0)  && rlang::is_quosure(mapping()[[aes]])) {
#           # We've got an aesthetic mapping
#           var_name <- as.character(rlang::get_expr(mapping()[[aes]]))
#           inherited <- ''
#         } else if (values$layers[[layer_id()]]$inherit.aes && rlang::is_quosure(values$gg$mapping[[aes]])) {
#           # Inherit mapping
#           var_name <- as.character(rlang::get_expr(values$gg$mapping[[aes]]))
#           inherited <- 'inherited'
#         }
#
#         # This assumes mappings are ONLY variable names - can be more general
#         if (!is.null(var_name)) {
#           content <- div(id = paste0(var_name,'-map-1'), # Only 1 for now until facet wrap added
#                          class = paste0('grid map ', var_name),
#                          draggable = TRUE,
#                          div(class = paste0('varname ', inherited),
#                              `data-colnum` = 1,
#                              var_name
#                          )
#           )
#         } else {
#           # No mapping, so going to check settings and create input
#           layer <- values$layers[[layer_id()]]
#
#           # Manually set by user if this is not NULL
#           aes_val <- layer$aes_params[[aes]]
#           default <- ''
#           # If NULL, set to default value if specified (which might be NA!!!)
#           if (is.null(aes_val) && !is.null(layer$geom$default_aes[[aes]])) {
#             aes_val <- layer$geom$default_aes[[aes]]
#             default <- 'default'
#           }
#
#           # If NULL (e.g. GROUP) or NA (e.g. fill), not set yet and required or not necessary
#           inputId <- paste0(aes, '-input-', layer_id())
#
#           # _ Set aesthetic inputs ####
#           content <- ifelse(is.null(aes_val) || is.na(aes_val),
#                             create_aes_empty(aes, default),
#                             create_aes_input(inputId, aes, aes_val, default))
#         }
#       }
#
#       bsa <<- bsplus::bs_append(bsa,
#                                 title = dropZoneInput(
#                                   inputId = paste0(aes, '-dropzone-', layer_id()),
#                                   class = "grid",
#                                   div(id = aes,
#                                       class = "aesname",
#                                       aes
#                                   )
#                                 ),
#                                 content = content
#       )
#     })
#   })
#   bsa
# })

# _ Plot ====
output$viz <- renderPlot({
  failure <- FALSE
  # Try to plot.  If unsuccessful, pass error message to help pane.
  # We need the print statement here or we can't capture errors
  tryCatch(print(eval(parse(text=ggcode()))),
           error = function(e) {
             shinyjs::show(id = "help-pane", anim = FALSE)
             shinyjs::html(id = "help-pane", html = e$message)
             failure <<- TRUE
           },
           finally = {
             if (!failure) {
               shinyjs::hide(id = "help-pane", anim = FALSE)
             }
           })
})

# _ Code ====
output$code <- renderPrint({
  print(ggcode())
})

# Events ----------------------

# _ Done ====
# User is done - tried this, but didn't work
#   https://stackoverflow.com/questions/34731975/how-to-listen-for-more-than-one-event-expression-within-a-shiny-eventreactive-ha
observeEvent(input$done, {
  shinyjs::js$close_window()
  stopApp()
})

# _ Cancel ====
observeEvent(input$cancel, {
  shinyjs::js$close_window()
  stopApp()
})

## _ Ready Layer One ====
#
# Comments:
#   Need to isolate changes to reactive variable to avoid infinite loop
#   Add inherited and default to make this work right!!!
# observe({
#   lapply(isolate(aesthetics()), function(aes) {
#     # Only run once aesthetic inputs are present
#     # req((paste0(aes, '-dropzone-', layer_id()) %in% names(reactiveValuesToList(input))))
#
#     # First, set mapping if present
#     var <- input[[paste0(aes, '-dropzone-', layer_id())]]
#     if (!is.null(var) && var != '') {
#       isolate({
#         if (geom_type() == "geom-blank") {
#           values$gg$mapping[[aes]] <- rlang::quo(!!rlang::sym(var))
#         } else {
#           # Only set layer mapping if (1) does not inherit from base, or (2) does and base not set
#           # TODO: Allow for override of inherited mapping in the future
#           if (!values$layers[[layer_id()]]$inherit.aes ||
#               (values$layers[[layer_id()]]$inherit.aes &&
#                (!rlang::is_quosure(values$gg$mapping[[aes]]) ||
#                 (values$gg$mapping[[aes]] != var)))) {
#             # Set mapping aesthetic
#             values$layers[[layer_id()]]$mapping[[aes]] <- rlang::quo(!!rlang::sym(var))
#
#             # Remove from aes_params (seems to prioritize over mapping if set)
#             values$layers[[layer_id()]]$aes_params[[aes]] <- NULL
#
#             # Change inherited status
#             session$sendInputMessage(paste0(aes, '-dropzone-', layer_id()), list(action = 'change_inherited_status'))
#           }
#         }
#         values$gg$labels[[aes]] <- var
#       })
#     } else {
#       # No mapping - set by input if present (has to be layer for now!!!)
#       aes_input <- input[[paste0(aes, '-input-', layer_id())]]
#       isolate({
#         if ((geom_type() != "geom-blank") && !is.null(aes_input)) {
#           # TODO:  Default values can be NA!!!!!Create a button for setting a value...
#           default_value <- values$layers[[layer_id()]]$geom$default_aes[[aes]]
#
#           # Convert default colour values to hex
#           if ((aes %in% c('colour', 'fill')) && !is.na(default_value)) {
#             default_value <- colour_to_hex(default_value)
#           }
#
#           if (is.null(default_value) || (!is.na(default_value) && (default_value != aes_input))) {
#             # No default - set parameter
#             values$layers[[layer_id()]]$aes_params[[aes]] <- aes_input
#             session$sendInputMessage(paste0(aes, '-dropzone-', layer_id()), list(action = 'default_off'))
#           } else {
#             # Gonna use default value by, er, default
#             values$layers[[layer_id()]]$aes_params[[aes]] <- NULL
#             session$sendInputMessage(paste0(aes, '-dropzone-', layer_id()), list(action = 'default_on'))
#           }
#
#           # Force update of plot
#           forcePlot$trigger()
#         } else {
#           # Condition: No mapping set and geom_blank
#           # ind <- which(aesthetics() == aes) - 1
#           # span('Not set')
#         }
#       })
#     }
#   })
# })
