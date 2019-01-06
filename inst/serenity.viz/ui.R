resourcePath <- system.file("serenity.viz", "www", package = "serenity.viz")

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
            dragZone("geoms",
                     choices = sapply(geoms, function(geom) { "" }, simplify = FALSE, USE.NAMES = TRUE))
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
        dropZoneInput("layers",
                      choices = c("geom-blank" = "",
                                  sapply(geoms, function(geom) { "" }, simplify = FALSE, USE.NAMES = TRUE)),
                      presets = list(
                        values = "geom-blank",
                        selected = "geom-blank",
                        locked = "geom-blank",
                        freeze = "geom-blank")
                      ,
                      multivalued = TRUE,
                      selectable = TRUE,
                      selectOnDrop = TRUE,
                      togglevis = TRUE,
                      direction = "horizontal",
                      removeOnSpill = FALSE
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
