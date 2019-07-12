library(shiny)
library(dragulaSelectR)

layerUI <- function(plotname) {
  div(
    class = "layer-wrap",
    tagList(
      div(
        class = "layer-title",
        tagList(
          icon("chart-bar"),
          div(plotname)
        )
      ),
      div(
        class = "layer-icons",
        tagList(
          icon("question"),
          icon("check")
        )
      )
    )
  )
}

shinyApp(
  ui = fluidPage(
    tags$style(HTML("
      .ds-dropoption {
        height:90px !important;
        width:100px !important;
        position:relative;
        border: 1px solid #979797;
        box-sizing: border-box;
        box-shadow: 0px 2px 2px rgba(0, 0, 0, 0.25);
        cursor: pointer !important;
      }

      .ds-locked, .ds-selected {
        background: none !important;
      }

      .layer-wrap {
        position:relative;
      }

      .layer-title {
        display: block;
        position: absolute;
        top: 10px;
        margin-left: auto;
        margin-right: auto;
        left: 0;
        right: 0;
      }

      .layer-title > i {
        font-size: 50px; font-weight: 500;
      }

      .layer-title > div {
        width: 100%;
      }

      .layer-icons > i {
        position: absolute;
        visibility: hidden;
      }

      .layer-icons > i.fa-question {
        right: 0;
      }

      .layer-icons > i.fa-check {
        left: 0;
      }

      .layer-wrap:hover i.fa-question {
        visibility: visible;
      }

      .ds-dropoption.ds-selected i.fa-check {
        visibility: visible;
        color: green;
      }
    ")),
    fluidRow(
      column(12,
             h3("Dropzone"),
             dropZoneInput("dropzone", choices = list(one = layerUI("Bar Plot"),
                                                      two = layerUI("Scatter Plot")),
                           flex = TRUE,
                           selectable = TRUE,
                           direction = "horizontal",
                           presets = list(values = c("one", "two"),
                                          locked = c("one", "two"))
             )
      )
    )
  ),
  server = function(input, output) {
    output$showme <- renderPrint({ input$dropzone })
  }
)
