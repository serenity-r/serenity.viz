library(shiny)
library(serenity.viz)

ui <- function() {
  fluidPage(
    shinyjs::useShinyjs(),
    tags$head(includeCSS(file.path(system.file("www", package = "serenity.viz"), "css", "app.css"))),
    h4("Size [Module: size]"),
    div(
      class = "aesthetic",
      layerAesValueUI("size"), # Slider
    ),
    verbatimTextOutput("size_output"),
    h4("Colour [Module: colour]"),
    div(
      class = "aesthetic",
      layerAesValueUI("colour"), # Colourpicker
    ),
    verbatimTextOutput("colour_output"),
    h4("Linetype [Module: linetype]"),
    div(
      class = "aesthetic",
      layerAesValueUI("linetype") # Select
    ),
    verbatimTextOutput("linetype_output"),
    h4("Shape (showing initial value in output) [Module: shape]"),
    div(
      class = "aesthetic",
      layerAesValueUI("shape") # Slider (which will show initial value)
    ),
    verbatimTextOutput("shape_output"),
  )
}

server <- function(input, output, session) {
  initial = list(size = 0.5, colour = "#333333", linetype = "solid", shape = 19)
  aes_values <- sapply(c("size", "colour", "linetype", "shape"), function(aesthetic) {
    layerAesValueServer(
      id = aesthetic,
      aesthetic = aesthetic,
      initial = initial[[aesthetic]],
      show_initial = reactive({ aesthetic == "shape" })
    )
  })

  output$size_output <- renderText({
    aes_values$size()
  })

  output$colour_output <- renderText({
    aes_values$colour()
  })

  output$linetype_output <- renderText({
    aes_values$linetype()
  })

  output$shape_output <- renderText({
    aes_values$shape()
  })

  exportTestValues(size = aes_values$size(),
                   colour = aes_values$colour(),
                   linetype = aes_values$linetype(),
                   shape = aes_values$shape())
}

shinyApp(ui, server)
