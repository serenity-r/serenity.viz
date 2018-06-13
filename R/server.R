#' Server for Serenity Viz.
#'
#' @param input input
#' @param output output
#' @param session session
#'
#' @import shiny magrittr
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
    geom_type <- ifelse(values$selectedNum == 0, "default", geoms_[input$jsColNum[1]])
    aes_names <- aesthetics[[geom_type]]
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
      if (colNum == values$selectedNum) {
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
    iris %>% ggplot2::ggplot()
  })

  output$code <- renderText({
    input$`selected-layers-row`
    })
}
