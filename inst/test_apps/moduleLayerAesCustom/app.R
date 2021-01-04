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
                   colourpicker::colourInput(inputId = "colour",
                                             label = "",
                                             value = "#73F0E8")
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
             ),
             tags$h3(HTML("Custom override with outside control<br>[Module: test_with_control")),
             div(
               class = "aesthetic",
               tags$section(
                 class = "value-section",
                 layerAesCustomUI("test_with_control")
               )
             ),
             tags$h3("Output"),
             verbatimTextOutput("custom_string_with_control")

      ),
      column(4,
             tags$h3("Input (numeric)"),
             div(
               class = "aesthetic",
               tags$section(
                 class = "value-section",
                 div(
                   class = "aes-content",
                   sliderInput(inputId = "size",
                               label = "",
                               min = 0,
                               max = 1,
                               value = 0.5)
                 )
               )
             ),
             tags$h3(HTML("Custom override<br> [Module: test_size]")),
             div(
               class = "aesthetic",
               tags$section(
                 class = "value-section",
                 layerAesCustomUI("test_size")
               )
             ),
             tags$h3("Output"),
             verbatimTextOutput("custom_string_size")
      )
    ),
    reactlog_module_ui()
  )
}

server <- function(input, output, session) {
  custom_string <- layerAesCustomServer("test",
                                        custom_for = reactive({ input$colour }),
                                        custom_value = reactive({ NULL }))

  custom_string_with_control <- layerAesCustomServer("test_with_control",
                                                     custom_for = reactive({ input$colour }),
                                                     custom_value = reactive({ input$custom_value }))

  custom_string_size <- layerAesCustomServer("test_size",
                                             custom_for = reactive({ input$size }),
                                             custom_value = reactive({ NULL }))

  output$custom_string <- renderText({
    custom_string()
  })

  output$custom_string_with_control <- renderText({
    custom_string_with_control()
  })

  output$custom_string_size <- renderText({
    custom_string_size()
  })

  reactlog_module_server()
}

shinyApp(ui, server)
