library(shiny)
library(serenity.viz)
library(DT)

ui <- function() {
  fluidPage(
    unitChooserUI("mychooser",
                  title = "Your Value"),
    verbatimTextOutput("unit_results")
  )
}

server <- function(input, output, session) {
  unit <- callModule(module = unitChooserServer,
                     id = "mychooser")

  output$unit_results <- renderPrint({
    unit()
  })
}

shinyApp(ui, server)
