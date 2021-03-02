library(shiny)
library(profvis)
library(serenity.viz)

ui <- function() {
  fluidPage(
    shinyjs::useShinyjs(),
    tags$head(includeCSS(file.path(system.file("www", package = "serenity.viz"), "css", "app.css"))),
    checkboxInput("inherit",
                  "Inherit from base?",
                  value = FALSE),
    profvis_ui("profiler"),
    hr(),
    layerAestheticsUI(id = 'geom-blank', name = 'Base Layer')
  )
}

server <- function(input, output, session) {
  callModule(profvis_server, "profiler")

  base_layer <- layerAestheticsServer(
    id = 'geom-blank',
    geom = 'geom-blank',
    layer_selected = reactive({ 'geom-blank' }),
    base_layer_stages = NULL,
    dataset = iris,
    inherit_aes = reactive({ FALSE }),
    layer_stat = reactive({ "identity" })
  )
}

shinyApp(ui, server)
