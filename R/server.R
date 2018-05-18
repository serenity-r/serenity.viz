#' Server for Serenity Viz.
#'
#' @param input input
#' @param output output
#' @param session session
#'
#' @import shiny
server <- function(input, output, session) {
  values <- reactiveValues(
    selectedNum = 0
  )

  # User is done - tried this, but didn't work
  #   https://stackoverflow.com/questions/34731975/how-to-listen-for-more-than-one-event-expression-within-a-shiny-eventreactive-ha
  observeEvent(input$done, {
    shinyjs::js$closeWindow()
    stopApp()
  })
  observeEvent(input$cancel, {
    shinyjs::js$closeWindow()
    stopApp()
  })

  # Render geom icons
  output$selectedGeoms <- renderUI({
    lapply(seq_along(geoms), function(colNum) {
      cls <- paste0("col ", geoms[colNum])
      if (colNum == values$selectedNum) {
        cls <- paste0(cls, " selected")
      }
      div(
        class = cls,
        div(id = geoms[colNum],
          class = "selected-col-inner",
          `data-colnum` = colNum
        )
      )
    })
  })

  # Receive event from JS: a geom was selected/deselected
  observeEvent(input$jsColNum, {
    newNum <- input$jsColNum[1]

    # Deactivate help pane
    if ((newNum == values$selectedNum) || (newNum < 1 || newNum > length(geoms))) {
      values$selectedNum <- 0
      shinyjs::toggle(id = "help-pane", anim = FALSE)
      return()
    }

    # Activate help pane
    if (values$selectedNum == 0) {
      shinyjs::toggle(id = "help-pane", anim = FALSE)
    }

    # Update help text
    shinyjs::html(id = "help-pane",
                  html = help_panes[[geoms_[newNum]]])

    # Select geom
    values$selectedNum <- newNum
  })

  output$scatterPlot <- renderPlot({
    ggplot2::ggplot(data = iris,
           aes(x = Sepal.Length,
               fill = Species)) +
      geom_bar(stat = "bin")
  })
}
