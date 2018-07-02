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
    gg = ggplot2::ggplot(data = iris)
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
      cls <- paste0("grid var ", var_names[varNum])
      div(
        class = cls,
        draggable = TRUE,
        div(id = var_names[varNum],
            class = "varname",
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
                              `data-colnum` = aesNum,
                              aes_names[aesNum]
                          )
                        ),
                        content = sliderInput(inputId = paste0(aes_names[aesNum], '-slider'),
                                              label = "",
                                              min = 0,
                                              max = 1,
                                              value = 0.5)
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
        )
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
      values$gg <- try(values$gg + eval(parse(text=paste0(stringr::str_replace(geom_type, "-", "_"), "()"))))
      names(values$gg$layers) <- c(names(values$gg$layers[1:(num_layers-1)]), values$selectedLayerId)
      # values$gg$layers[[values$selectedLayerId]] <- eval(parse(text=paste0(stringr::str_replace(geom_type, "-", "_"), "()")))
    } else {
      # Just a reshuffling of layers - address accordingly
      values$gg$layers <- values$gg$layers[input$`selected-layers-row`[1 + (1:num_layers)]]
    }
  })

  output$viz <- renderPlot({

  })

  output$code <- renderPrint({
    values$gg$layers
  })
}
