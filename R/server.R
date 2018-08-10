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
    geom_num = 0,
    gg = ggplot2::ggplot(data = iris),
    layers = list()
  )

  # User is done - tried this, but didn't work
  #   https://stackoverflow.com/questions/34731975/how-to-listen-for-more-than-one-event-expression-within-a-shiny-eventreactive-ha
  observeEvent(input$done, {
    shinyjs::js$close_window()
    stopApp()
  })
  observeEvent(input$cancel, {
    shinyjs::js$close_window()
    stopApp()
  })

  # Render variable divs
  output$data_variables <- renderUI({
    var_names <- colnames(iris)
    lapply(seq_along(var_names), function(var_num) {
      cls <- paste0("grid var ", stringr::str_replace(var_names[var_num], '[.]', '-')) # var class name used to count # of elements for unique id creation
      div(
        id = var_names[var_num],
        class = cls,
        draggable = TRUE,
        div(class = "varname",
            `data-colnum` = var_num,
            var_names[var_num]
        )
      )
    })
  })

  # Render aesthetics divs
  output$aesthetics <- renderUI({
    # REFACTOR: Do some of this in global.R??  Probably faster...
    if (geom_type() == "geom-blank") {
      aes_names <- aesthetics[["default"]]
    } else {
      aes_names <- eval(parse(text=paste0(stringr::str_replace(geom_type(), "-", "_"), "()")))$geom$aesthetics()
    }
    bsa <- bs_accordion(id = "acc") %>%
      bs_set_opts(panel_type = "success", use_heading_link = TRUE)
    lapply(seq_along(aes_names), function(aes_num) {
      bsa <<- bs_append(bsa,
                        title = dropZoneInput(
                          inputId = paste0(aes_names[aes_num], '-dropzone'),
                          class = "grid",
                          div(id = aes_names[aes_num],
                              class = "aesname",
                              aes_names[aes_num]
                          )
                        ),
                        content = uiOutput(aes_names[aes_num], inline = FALSE)
                        )
    })
    bsa
  })

  # Render geom icons
  output$geoms <- renderUI({
    lapply(seq_along(geoms), function(col_num) {
      cls <- paste0("col geom ", geoms[col_num])
      if (col_num == values$geom_num) {
        cls <- paste0(cls, " selected")
      }
      div(
        id = geoms[col_num],
        class = cls,
        draggable = TRUE,
        div(class = "selected-geom-inner",
            `data-colnum` = col_num
        ),
        rmarkdown::html_dependency_font_awesome() # This is really needed in the layers
      )
    })
  })

  # Receive event from JS: a geom was selected/deselected
  observeEvent(input$js_geom_num, {
    new_geom_num <- input$js_geom_num[1]

    # Deactivate help pane
    if ((new_geom_num == values$geom_num) || (new_geom_num < 1 || new_geom_num > length(geoms))) {
      values$geom_num <- 0
      shinyjs::toggle(id = "help-pane", anim = FALSE)
      return()
    }

    # Activate help pane
    if (values$geom_num == 0) {
      shinyjs::toggle(id = "help-pane", anim = FALSE)
    }

    # Update help text
    shinyjs::html(id = "help-pane",
                  html = help_panes[[geoms_[new_geom_num]]])

    # Select geom
    values$geom_num <- new_geom_num
  })

  # Receive event from JS: a layer was selected
  observeEvent(input$js_layer_id, {
    # Is layer new?
    num_layers <- length(input$`selected-layers-row`) - 1 # Ignore blank layer
    if (length(values$gg$layers) < num_layers) {
      # New layer added - add to gg object (temporary until all required aesthetics are filled)

      # geom_type <- paste(stringr::str_split(layer_id(), '-')[[1]][1:2], collapse="-")
      values$gg <- values$gg + eval(parse(text=paste0(stringr::str_replace(geom_type(), "-", "_"), "()")))
      names(values$gg$layers) <- c(names(values$gg$layers[1:(num_layers-1)]), layer_id())
      # values$gg$layers[[layer_id()]] <- eval(parse(text=paste0(stringr::str_replace(geom_type(), "-", "_"), "()")))
    } else {
      # Just a reshuffling of layers - address accordingly
      values$gg$layers <- values$gg$layers[input$`selected-layers-row`[1 + (1:num_layers)]]
    }
  })

  output$viz <- renderPlot({
    tryCatch(print(values$gg),
             error = function(e) {
               shinyjs::show(id = "help-pane", anim = FALSE)
               shinyjs::html(id = "help-pane", html = e$message)
             })
  })

  output$code <- renderPrint({
    # values$gg$layers
    layer_id()
  })

  ## State reactives

  # Important to use eventReactive here as we want layer_id set on start of program, which is
  #   why ignoreNULL and ignoreInit are both FALSE.  NULL corresponds to the main (blank) layer
  layer_id <- eventReactive(input$js_layer_id, {
    return(ifelse(is.null(input$js_layer_id), 'geom-blank-layer-0', input$js_layer_id[1]))
  }, ignoreNULL = FALSE, ignoreInit = FALSE)

  # The selected geom type responds only to the layer_id
  geom_type <- reactive({
    paste(stringr::str_split(layer_id(), '-')[[1]][1:2], collapse="-")
  })

  # What is the current mapping?  State determined by selected layer
  mapping <- reactive({
    ifelse(geom_type() == "geom-blank", values$gg$mapping, values$gg$layers[[layer_id()]]$mapping)
  })

  #
  # Layers need to be a reactive value
  #

  purrr::walk(unique(unlist(aesthetics)), function(aes) {
    ui <- ""

    if (aes == 'x') {
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

    output[[aes]] <<- renderUI({ui})
  })
}
