library(shiny)
library(reactlog)
library(serenity.viz)

reactlog_enable()

ui <- function() {
  fluidPage(
    shinyjs::useShinyjs(),
    tags$head(includeCSS(file.path(system.file("www", package = "serenity.viz"), "css", "app.css"))),
    fluidRow(
      column(4,
             tags$h3("Input (character)"),
             div(
               class = "aesthetic",
               tags$section(
                 class = "value-section",
                 div(
                   class = "aes-content",
                   uiOutput("colour_ui"),
                 )
               )
             ),
             tags$h3(HTML("Custom override<br>[Module: test]")),
             div(
               class = "aesthetic",
               tags$section(
                 class = "value-section",
                 layerAesCustomUI("test")
               )
             ),
             tags$h3("Output"),
             verbatimTextOutput("custom_string")
      ),
      column(4,
             textInput(
               "custom_value",
               "Set Custom Value",
               "#000000"
             )
      )
    ),
    reactlog_module_ui()
  )
}

server <- function(input, output, session) {

  output$colour_ui <- renderUI({
    cat("Rendering...\n")
    colourpicker::colourInput(inputId = "colour",
                              label = "",
                              value = "#73F0E8")
  })

  custom_string <- layerAesCustomServer("test",
                                        custom_for = reactive({ input$colour }),
                                        custom_value = reactive({ isolate(input$custom_value) }),
                                        waitforit = 1)

  output$custom_string <- renderText({
    custom_string()
  })

  reactlog_module_server()
}

shinyApp(ui, server)
