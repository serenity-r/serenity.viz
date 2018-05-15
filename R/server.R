#' Server for Serenity Viz.
#'
#' @param input input
#' @param output output
#' @param session session
#'
#' @import shiny ggplot2 plotly miniUI
server <- function(input, output, session) {
  output$scatterPlot <- renderPlot({
    ggplot(data = iris,
           aes(x = Sepal.Length,
               y = Sepal.Width)) +
      geom_point()
  })
}
