library(miniUI)

resourcePath <- system.file("serenity.viz", "www", package = "serenity.viz")

resetButtonUI <- miniButtonBlock(actionButton("refresh", "Refresh"))

miniPage(
  shinyjs::useShinyjs(),
  shinyjs::extendShinyjs(
    script = file.path(resourcePath, "js", "shinyjs-funcs.js"),
    functions = c("close_window")
  ),
  tags$head(includeCSS(file.path(resourcePath, "css", "app.css"))),
  gadgetTitleBar("Serenity Viz",
                 left = miniTitleBarCancelButton(),
                 right = miniTitleBarButton("done", "Done", primary = TRUE)),
  fillRow(
    flex = c(1, 2, 1),

    # Variables and geoms
    fillCol(
      flex = c(7, 5),
      miniContentPanel( # Variables
        wellPanel(
          dataSetUI(id = attributes(serenity.viz.data)$df_name)
        )
      ),
      miniContentPanel( # Geoms
        wellPanel(
          div(
            id = "selected-geoms-row",
            uiOutput("geoms", inline = TRUE)
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
        scrollable = TRUE
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
