#' UI for Serenity Viz
#'
#' @import shiny miniUI
#'
#' @return An HTML tag object that can be rendered as HTML using as.character().
#'
ui <- function() {
  resourcePath <- system.file("gadgets", "serenity.viz",
                              package = "serenity.viz")

  resetButtonUI <- miniButtonBlock(actionButton("refresh", "Refresh"))

  miniPage(
    shinyjs::useShinyjs(),
    shinyjs::extendShinyjs(
      script = file.path(resourcePath, "js", "shinyjs-funcs.js"),
      functions = c("closeWindow")
    ),
    tags$head(includeCSS(file.path(resourcePath, "css", "app.css"))),
    gadgetTitleBar("Serenity Viz",
                   left = miniTitleBarCancelButton(),
                   right = miniTitleBarButton("done", "Done", primary = TRUE)),
    fillRow(
      flex = c(1, 2, 1),
      fillCol(
        miniContentPanel(
          selectInput('variables',
                      'Iris Data',
                      colnames(iris),
                      multiple=TRUE,
                      selectize=FALSE,
                      size=ncol(iris))
        ),
        miniContentPanel(
          wellPanel(
            div(
              id = "selected-cols-row",
              uiOutput("selectedGeoms", inline = TRUE)
            ),
            height = "100%",
            padding = 5
          )
        )
      ),
      fillCol(
        flex = c(2, 1),
        plotOutput("scatterPlot",
                   height = "100%"),
        miniContentPanel(
          wellPanel("ggplot(data = iris,
                            aes(x = Sepal.Length,
                                fill = Species)) +
                        geom_bar(stat = 'bin')"),
          padding = 0,
          scrollable = FALSE)
      ),
      miniContentPanel(
        div(id = "dropzone")
      )
    ),
    shinyjs::hidden(
      absolutePanel(id="help-pane",
                    top = 44,
                    left = "25%",
                    width = "50%",
                    height = "64%",
                    draggable = FALSE,
                    HTML(markdown::markdownToHTML(fragment.only=TRUE, text=c(
                        "This is an absolutePanel that uses `bottom` and `right` attributes.

It also has `draggable = TRUE`, so you can drag it to move it around the page.  The slight transparency is due to `style = 'opacity: 0.92'`.

You can put anything in absolutePanel, including inputs and outputs:"
                      )))
      )
    )
  )
}
