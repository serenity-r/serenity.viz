library(shinyWidgets)
library(shiny)
library(dndselectr)

layerUI <- function(aesthetic) {
  div(
    class = "layer-wrap",
    tagList(
      div(
        class = "layer-title",
        tagList(
          div(
            icon("chart-bar"),
            span("Bar Plot")
          ),
          span("Other stuff")
        )
      ),
      wellPanel(
        class = "layer-settings",
        "Stuff to say"
      ),
      div(
        class = "layer-icons",
        tagList(
          div(
            icon("cog")
          ),
          div(
            class = "ds-toggle-visible",
            icon("eye")
          ),
          div(
            icon("question")
          )
        )
      )
    )
  )
}

shinyApp(
  ui = fluidPage(
    tags$style(HTML("
      .ds-dropoption i.fa-eye, .ds-dropoption i.fa-eye-slash {
        visibility: visible;
      }

      .ds-toggle-visible {
        margin-left: 0;
        float: unset;
      }

      .layer-wrap {
        min-height:60px;
        height:60px;
        position:relative;
      }

      .layer-title {
        display: inline-block;
        width: 30%;
        vertical-align: top;
      }

      .layer-title > div {
        font-size: 20px; font-weight: 500;
      }

      .layer-settings {
        display:inline-block;
        width: 60%;
        height: 50px;
        margin: 5px 0;
        padding: 5px;
      }

      .layer-icons {
        position:absolute;
        right:0;
        top:0;
        bottom: 0;
        display: flex;
        flex-direction: column;
        justify-content: space-around;
        align-items: center;
      }

      .layer-icons > div {
        flex: 0 1 auto;
      }
    ")),
    fluidRow(
      column(6,
             h3("Dragzone"),
             dragZone("dragzone", choices = list(one = "One",
                                                 two = "Two",
                                                 three = "Three",
                                                 four = "Four")),
             h3("Values"),
             verbatimTextOutput("showme"),
             h3("Multivalues"),
             verbatimTextOutput("showmeMultivalues")
      ),
      column(6,
             h3("Dropzone"),
             dropZoneInput("dropzone", choices = list(one = layerUI("one"),
                                                      two = layerUI("two"),
                                                      three = layerUI("three"),
                                                      four = layerUI("four")),
                           multivalued = TRUE,
                           presets = c("one", "two"))
      )
    )
  ),
  server = function(input, output) {
    output$showme <- renderPrint({ input$dropzone })
    output$showmeMultivalues <- renderPrint({ multivalues( input$dropzone ) })
  }
)
