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

      # Variables and geoms
      fillCol(
        miniContentPanel(
          wellPanel(
            div(
              id = "selected-vars-col",
              uiOutput("dataVariables", inline = FALSE)
            )
          )
        ),
        miniContentPanel(
          wellPanel(
            div(
              id = "selected-geoms-row",
              uiOutput("selectedGeoms", inline = TRUE)
            ),
            height = "100%",
            padding = 5
          )
        )
      ),

      # Layers, plot, and code
      fillCol(
        flex = c(8, 4),
        tagList(
          dropZoneInput("selected-layers-row",
                        div(
                          id = "geom-blank-layer-0",
                          class = "col geom-blank unsortable selected",
                          div(id = "layer-blank",
                              class = "layer-inner",
                              `data-colnum` = 1
                          )
                        )
          ),
          plotOutput("viz",
                     height = "100%")
        ),
        miniContentPanel(
          id = "cpanel-code",
          verbatimTextOutput("code"),
          padding = 0,
          scrollable = FALSE
        )
      ),

      # Aesthetics
      miniContentPanel(
        id = "selected-aes-col",
        wellPanel(
          uiOutput("aesthetics", inline = FALSE)
        )
      )
    ),
    shinyjs::hidden(
      absolutePanel(id="help-pane",
                    top = 123,
                    left = "25%",
                    width = "50%",
                    height = "61%",
                    draggable = FALSE
      )
    )
  )
}
