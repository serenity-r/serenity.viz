# This stores returned reactives from layer modules
layer_modules <- reactiveValues()

# Render ----------------------

# _ Subsetted data ====
subsetted_data <- callModule(module = dataSet,
                             id = attributes(serenity.viz.data)$df_name)

# _ ggplot code ====
output$aesthetics <- renderUI({
  req(input$layers_selected)
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
