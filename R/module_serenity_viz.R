#' UI for Serenity Viz module
#'
#' @param id Serenity Viz module ID
#' @param dataset Passed in dataset for visualization
#' @param titlebar Show title bar with Done and Cancel buttons?
#' @param showcode Show code for plots?
#'
#' @return UI for Serenity Viz module
#'
#' @import shiny
#' @export
#'
serenityVizUI <- function(id, dataset, titlebar = FALSE, showcode = FALSE, height = NULL) {
  ns <- NS(id)

  miniUI::miniPage(
    style = switch(!is.null(height), paste("height:", height), height),
    shinyjs::useShinyjs(),
    shinyjs::extendShinyjs(
      script = file.path(resourcePath, "js", "shinyjs-funcs.js"),
      functions = c("close_window")
    ),
    tags$head(includeCSS(file.path(resourcePath, "css", "app.css"))),
    switch(titlebar,
           miniUI::gadgetTitleBar("Serenity Viz",
                                  left = miniUI::miniTitleBarCancelButton(ns("cancel")),
                                  right = miniUI::miniTitleBarButton(ns("done"), "Done", primary = TRUE)),
           NULL),
    fillRow(
      flex = c(1, 2, 1),

      # Variables and geoms
      fillCol(
        flex = c(NA, 7, NA, 5),
        h3("Variables"),
        miniUI::miniContentPanel(
          wellPanel(
            dataUI(id = ns(attributes(dataset)$df_name)),
            height = "100%"
          )
        ),
        h3("Plot Types"),
        miniUI::miniContentPanel(
          wellPanel(
            div(
              id = ns("selected-geoms-row"),
              class = "selected-geoms-row",
              dragulaSelectR::dragZone(ns("geoms"),
                                       class = "geoms",
                                       choices = sapply(geoms, function(geom) { "" }, simplify = FALSE, USE.NAMES = TRUE))
            ),
            height = "100%",
            padding = 5
          )
        )
      ),

      # Layers, plot, and code
      fillCol(
        flex = c(2, 6, 4),
        tagList(
          uiOutput(ns("layersUI")),
          plotOutput(ns("viz"), height = "70%"),
          switch(showcode,
                 verbatimTextOutput(ns("code")),
                 NULL)
        )
      ),

      # Aesthetics
      fillCol(
        miniUI::miniContentPanel(
          id = ns("selected-aes-col"),
          class = "selected-aes-col",
          wellPanel(
            uiOutput(ns("aesthetics")),
            height = "100%"
          )
        )
      )
    ),
    shinyjs::hidden(
      absolutePanel(id = ns("help-pane"),
                    class = "help-pane",
                    top = 123,
                    left = "25%",
                    width = "50%",
                    height = "61%",
                    draggable = FALSE
      )
    )
  )
}

#' Server code for Serenity Viz module
#'
#' @param input Shiny input
#' @param output Shiny output
#' @param session Shiny session
#' @param dataset Passed in dataset for visualization
#'
#' @importFrom magrittr %>%
#' @import shiny ggplot2 dplyr forcats
#' @export
#'
serenityVizServer <- function(input, output, session, dataset, trigger=NULL) {
  layer_id <- paste(stringr::str_split(gsub("-$", "", session$ns('')), '-')[[1]][2:5], collapse="-")

  # This stores returned reactives from layer modules
  layer_modules <- reactiveValues()

  subsetted_data <- callModule(module = dataServer,
                               id = attributes(dataset)$df_name,
                               dataset = dataset)

  output$layersUI <- renderUI({
    if (!is.null(trigger)) trigger()
    isolate({
      dragulaSelectR::dropZoneInput(session$ns("layers"),
                                    class = "layers",
                                    choices = c("geom-blank" = "",
                                                sapply(geoms, function(geom) { "" }, simplify = FALSE, USE.NAMES = TRUE)),
                                    presets = list(
                                      values = idsToGeoms(input$layers) %||% "geom-blank",
                                      selected = idsToGeoms(input$layers_selected) %||% "geom-blank",
                                      locked = "geom-blank",
                                      freeze = "geom-blank"),
                                    multivalued = TRUE,
                                    selectable = TRUE,
                                    selectOnDrop = TRUE,
                                    togglevis = TRUE,
                                    direction = "horizontal",
                                    removeOnSpill = FALSE
      )
    })
  })

  output$aesthetics <- renderUI({
    req(input$layers_selected)
    layerUI(id = session$ns(input$layers_selected))
  })

  # Get the names of the visible layers
  visible_layers <- reactive({
    setdiff(input$layers, input$layers_invisible)
  })

  # Preps geom_blank dropzone inputs for layer modules
  geom_blank_inputs_to_reactives <- function() {
    geom_blank_inputs <- as.list(paste0('geom-blank-ds-1-', gg_aesthetics[["geom-blank"]], '-dropzone'))
    names(geom_blank_inputs) <- paste0('geom-blank-ds-1-', gg_aesthetics[["geom-blank"]], '-dropzone')
    if (any(names(geom_blank_inputs) %in% names(input))) {
      return(geom_blank_inputs %>%
               purrr::map(~ reactive({ input[[.]] })))
    } else {
      return(NULL)
    }
  }

  # Update layer module output reactives - create only once!
  observeEvent(input$layers, {
    # Adding new layers
    purrr::map(setdiff(input$layers, names(layer_modules)), ~ { layer_modules[[.]] <- callModule(module = layerServer, id = .,
                                                                                                 reactive({input$layers_selected}),
                                                                                                 geom_blank_inputs_to_reactives(),
                                                                                                 dataset = dataset)} )
    # Remove old layers
    purrr::map(setdiff(names(layer_modules), input$layers), ~ { layer_modules[[.]] <- NULL })
  }, priority = 1) # Needs to happen before layer_code reactive

  # Get layer code
  layer_code <- reactive({
    req(visible_layers())
    paste(purrr::map(reactiveValuesToList(layer_modules)[visible_layers()], ~ .()), collapse = "+\n")
  })

  # _ Plot ====
  output$viz <- renderPlot({
    failure <- FALSE
    # Try to plot.  If unsuccessful, pass error message to help pane.
    # We need the print statement here or we can't capture errors
    tryCatch(print(eval(parse(text=ggcode()))),
             error = function(e) {
               shinyjs::show(id = "help-pane", anim = FALSE)
               shinyjs::html(id = "help-pane", html = e$message)
               failure <<- TRUE
             },
             finally = {
               if (!failure) {
                 shinyjs::hide(id = "help-pane", anim = FALSE)
               }
             })
  })

  # _ Code ====
  output$code <- renderPrint({
    print(ggcode())
  })

  ggcode <- reactive({
    req(layer_code())
    code <- attributes(dataset)$df_name
    if (isTruthy(subsetted_data())) {
      code <- paste(code,
                    "%>%\n",
                    subsetted_data()
      )
    }

    if (isTruthy(layer_code())) {
      code <- paste(code,
                    "%>%\n",
                    layer_code()
      )
    }

    return(styler::style_text(code))
  })

  # Events ----------------------

  # _ Done ====
  # User is done - tried this, but didn't work
  #   https://stackoverflow.com/questions/34731975/how-to-listen-for-more-than-one-event-expression-within-a-shiny-eventreactive-ha
  observeEvent(input$done, {
    shinyjs::js$close_window()
    stopApp(returnValue = ggcode())
  })

  # _ Cancel ====
  observeEvent(input$cancel, {
    shinyjs::js$close_window()
    stopApp(returnValue = NULL)
  })

  return(ggcode)
}

# GLOBALS ----

resourcePath <- system.file("www", package = "serenity.viz")

geoms <- c("geom-bar", "geom-point", "geom-line", "geom-dotplot", "geom-boxplot", "geom-violin", "geom-rug")

help_panes <- lapply(geoms, function(x) {
  paste0("<h2>", x, "</h2>
         <div class='axis' id='xaxis'></div>
         <div class='axis' id='yaxis'></div>")
})
names(help_panes) <- stringr::str_replace(geoms, "-", "_")

makeReactiveTrigger <- function(init_val = NULL) {
  rv <- reactiveValues(a = 0)
  val <- init_val
  list(
    get = function() {
      val
    },
    set = function(new_val) {
      val <<- new_val
    },
    depend = function() {
      rv$a
      invisible()
    },
    trigger = function() {
      rv$a <- isolate(rv$a + 1)
    }
  )
}

gg_aesthetics <- list(
  "geom-blank" = c("x",
                   "y",
                   "alpha",
                   "colour",
                   "fill",
                   "linetype",
                   "size",
                   "weight",
                   "xmin",
                   "xmax",
                   "ymin",
                   "ymax"),
  "geom-bar" = c("x",
                 "alpha",
                 "colour",
                 "fill",
                 "linetype",
                 "size",
                 "weight"),
  "geom-point" = ggplot2::GeomPoint$aesthetics(),
  "geom-line" = ggplot2::GeomLine$aesthetics(),
  "geom-dotplot" = ggplot2::GeomDotplot$aesthetics(),
  "geom-boxplot" = c(ggplot2::GeomBoxplot$aesthetics(), "y"),
  "geom-violin" = ggplot2::GeomViolin$aesthetics(),
  "geom-rug" = ggplot2::GeomRug$aesthetics()
)
ordering <- unique(unlist(gg_aesthetics))
gg_aesthetics <- purrr::map(gg_aesthetics, ~ ordering[ordering %in% .])

idsToGeoms <- function(id) {
  switch(!is.null(id),
         sapply(id, FUN = function(x) { paste(stringr::str_split(x, '-')[[1]][1:2], collapse="-") }, simplify = "array"),
         NULL)
}
