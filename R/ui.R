#' UI for Serenity Viz
#'
#' @import shiny miniUI
#'
#' @return An HTML tag object that can be rendered as HTML using as.character().
#'
ui <- function() {
  resetButtonUI <- miniButtonBlock(actionButton("refresh", "Refresh"))

  miniPage(
    gadgetTitleBar("Serenity Viz",
                   left = miniTitleBarCancelButton(),
                   right = miniTitleBarButton("done", "Done", primary = TRUE)),
    fillRow(
      flex = c(1, 2, 1),
      fillCol(
        miniContentPanel("Data"),
        miniContentPanel("Geoms")
      ),
      fillCol(
        flex = c(2, 1),
        plotOutput("scatterPlot",
                   height = "100%"),
        miniContentPanel(
          wellPanel("ggplot(data = iris,
                            aes(x = Sepal.Length,
                                y = Sepal.Width)) +
                     geom_point()"),
          padding = 0,
          scrollable = FALSE)
      ),
      miniContentPanel("Properties")
    )
  )
}
