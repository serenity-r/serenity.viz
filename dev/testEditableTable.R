library(shiny)
library(serenity.viz)
library(DT)

ui <- function() {
  fluidPage(
    editableTableUI("mytable"),
    verbatimTextOutput("table_values")
  )
}

server <- function(input, output, session) {
  values <- callModule(module = editableTableServer,
                       id = "mytable",
                       init = reactive({ data.frame(values = c(0,0.5,1)) }),
                       unique_values = TRUE,
                       session = session)

  output$table_values <- renderPrint({
    attributes(values())
  })
}

shinyApp(ui, server)
