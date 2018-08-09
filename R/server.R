#' Server for Serenity Viz.
#'
#' @param input input
#' @param output output
#' @param session session
#'
#' @import shiny bsplus ggplot2
#' @importFrom magrittr %>%
server <- function(input, output, session) {
  values <- reactiveValues(
    selectedGeomNum = 0,
    selectedLayerId = "geom-blank-layer-0", # Defaults to main layer
    gg = ggplot2::ggplot(data = iris),
    layers = list()
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

  # Render variable divs
  output$dataVariables <- renderUI({
    var_names <- colnames(iris)
    lapply(seq_along(var_names), function(varNum) {
      cls <- paste0("grid var ", stringr::str_replace(var_names[varNum], '[.]', '-')) # var class name used to count # of elements for unique id creation
      div(
        id = var_names[varNum],
        class = cls,
        draggable = TRUE,
        div(class = "varname",
            `data-colnum` = varNum,
            var_names[varNum]
        )
      )
    })
  })

  # Render aesthetics divs
  output$aesthetics <- renderUI({
    # REFACTOR: Do some of this in global.R??  Probably faster...
    geom_type <- paste(stringr::str_split(values$selectedLayerId, '-')[[1]][1:2], collapse="-")
    if (geom_type == "geom-blank") {
      aes_names <- aesthetics[["default"]]
    } else {
      aes_names <- eval(parse(text=paste0(stringr::str_replace(geom_type, "-", "_"), "()")))$geom$aesthetics()
    }
    bsa <- bs_accordion(id = "acc") %>%
      bs_set_opts(panel_type = "success", use_heading_link = TRUE)
    lapply(seq_along(aes_names), function(aesNum) {
      bsa <<- bs_append(bsa,
                        title = dropZoneInput(
                          inputId = paste0(aes_names[aesNum], '-dropzone'),
                          class = "grid",
                          div(id = aes_names[aesNum],
                              class = "aesname",
                              aes_names[aesNum]
                          )
                        ),
                        content = getAestheticUI(aes_names[aesNum])
                        )
    })
    bsa
  })

  # Render geom icons
  output$selectedGeoms <- renderUI({
    lapply(seq_along(geoms), function(colNum) {
      cls <- paste0("col geom ", geoms[colNum])
      if (colNum == values$selectedGeomNum) {
        cls <- paste0(cls, " selected")
      }
      div(
        id = geoms[colNum],
        class = cls,
        draggable = TRUE,
        div(class = "selected-geom-inner",
            `data-colnum` = colNum
        ),
        rmarkdown::html_dependency_font_awesome() # This is really needed in the layers
      )
    })
  })

  # Receive event from JS: a geom was selected/deselected
  observeEvent(input$jsGeomNum, {
    newNum <- input$jsGeomNum[1]

    # Deactivate help pane
    if ((newNum == values$selectedGeomNum) || (newNum < 1 || newNum > length(geoms))) {
      values$selectedGeomNum <- 0
      shinyjs::toggle(id = "help-pane", anim = FALSE)
      return()
    }

    # Activate help pane
    if (values$selectedGeomNum == 0) {
      shinyjs::toggle(id = "help-pane", anim = FALSE)
    }

    # Update help text
    shinyjs::html(id = "help-pane",
                  html = help_panes[[geoms_[newNum]]])

    # Select geom
    values$selectedGeomNum <- newNum
  })

  # Receive event from JS: a layer was selected
  observeEvent(input$jsLayerId, {
    # Select layer
    values$selectedLayerId <- input$jsLayerId[1]

    # Is layer new?
    num_layers <- length(input$`selected-layers-row`) - 1 # Ignore blank layer
    if (length(values$gg$layers) < num_layers) {
      # New layer added - add to gg object (temporary until all required aesthetics are filled)

      geom_type <- paste(stringr::str_split(values$selectedLayerId, '-')[[1]][1:2], collapse="-")
      values$gg <- values$gg + eval(parse(text=paste0(stringr::str_replace(geom_type, "-", "_"), "()")))
      names(values$gg$layers) <- c(names(values$gg$layers[1:(num_layers-1)]), values$selectedLayerId)
      # values$gg$layers[[values$selectedLayerId]] <- eval(parse(text=paste0(stringr::str_replace(geom_type, "-", "_"), "()")))
    } else {
      # Just a reshuffling of layers - address accordingly
      values$gg$layers <- values$gg$layers[input$`selected-layers-row`[1 + (1:num_layers)]]
    }
  })

  # Check to make sure layer is ready to render
  readyLayerOne <- reactive({
  })

  # Check to make sure plot is ready to render
  readyPlotOne <- reactive({
    # Grabbed from ggplot2::ggplot_build in plot_build.r
    plot <- values$gg
    layers <- plot$layers
    data <- plot$data
    facet <- plot$facet
    coord <- plot$coordinates
    plot_env <- plot$plot_env

    layer_data <- lapply(layers, function(y) y$layer_data(data))
    layout <- ggproto(NULL, Layout, facet = facet, coord = coord)

    # Apply function to layer and matching data
    by_layer <- function(f) {
      out <- vector("list", length(data))
      for (i in seq_along(data)) {
        out[[i]] <- f(l = layers[[i]], d = data[[i]])
      }
      out
    }
    data <- layout$setup(layer_data, data, plot_env)
    data <- by_layer(function(l, d) l$compute_aesthetics(d, plot))
    data <- by_layer(function(l, d) l$compute_statistic(d, layout))
    data <- by_layer(function(l, d) l$map_statistic(d, plot))

    all(unlist(by_layer(function(l, d) length(setdiff(l$geom$required_aes, names(d))) == 0)))
  })

  output$viz <- renderPlot({
    tryCatch(print(values$gg),
             error = function(e) {
               shinyjs::show(id = "help-pane", anim = FALSE)
               shinyjs::html(id = "help-pane", html = e$message)
             })
  })

  output$code <- renderPrint({
    values$gg$layers
  })

  getAestheticUI <- function(aes) {
    # Need to check if is_quosure(gg$mapping$alpha) first (i.e. is mapping set?).  If so,
    # create mapping variable.  If not, create input control and use information from
    # values$gg and values$layers to set values.
    ui <- ""

    cat(paste(aes,'\n'))
    if (aes == 'x') {
      if (rlang::is_quosure(values$gg$mapping[['x']])) {
      }
    } else
      if (aes == 'alpha') {
        ui <- sliderInput(inputId = paste0(aes, '-slider'),
                          label = "",
                          min = 0,
                          max = 1,
                          value = 1)
      } else
        if (aes == 'size') {
          ui <- sliderInput(inputId = paste0(aes, '-slider'),
                            label = "",
                            min = 0.1,
                            max = 10,
                            step = 0.1,
                            value = 0.5)
        } else
          if (aes == 'colour') {
            ui <- colourpicker::colourInput(inputId = paste0(aes, '-colourpicker'),
                                            label = "",
                                            value = "black")
          } else
            if (aes == 'fill') {
              ui <- colourpicker::colourInput(inputId = paste0(aes, '-colourpicker'),
                                              label = "",
                                              value = "black")
            }

    return(ui)
  }
}
