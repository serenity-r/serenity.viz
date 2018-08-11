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

  # Render ----------------------

  # _ Variable divs ====
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

  # _ Aesthetic divs ====
  output$aesthetics <- renderUI({
    # REFACTOR: Do some of this in global.R??  Probably faster...
    if (geom_type() == "geom-blank") {
      aes_names <- aesthetics[["default"]]
    } else {
      aes_names <- eval(parse(text=paste0(stringr::str_replace(geom_type(), "-", "_"), "()")))$geom$aesthetics()
    }
    bsa <- bs_accordion(id = "acc") %>%
      bs_set_opts(panel_type = "success", use_heading_link = TRUE)
    lapply(aes_names, function(aes) {
      # Main ggplot2 object -> Mapping only!
      if (geom_type() == "geom-blank") {
        # Is aesthetic already set to a mapping?
        if (rlang::is_quosure(mapping()[[aes]])) {
          # Use gg$mapping rather than mapping() reactive, as we know what this is
          var_name <- as.character(rlang::get_expr(mapping()[[aes]]))
          content <- div(id = paste0(var_name,'-map-1'), # Only 1 for now until facet wrap added
                         class = paste0('grid map ', var_name),
                         draggable = TRUE,
                         div(class = 'varname',
                             `data-colnum` = 1,
                             var_name
                         )
                      )
        } else {
          content <- span(
            'Not set'
          )
        }
      } else {
        var_name <- NULL

        # Check layer mapping first
        if (!is.null(mapping()) && rlang::is_quosure(mapping()[[aes]])) {
          # We've got an aesthetic mapping
          var_name <- as.character(rlang::get_expr(mapping()[[aes]]))
        } else if (layers()[[layer_id()]]$inherit.aes && rlang::is_quosure(values$gg$mapping[[aes]])) {
          # Inherit mapping
          var_name <- as.character(rlang::get_expr(values$gg$mapping[[aes]]))
        }

        # This assumes mapping are ONLY variable names - can be more general
        if (!is.null(var_name)) {
          content <- div(id = paste0(var_name,'-map-1'), # Only 1 for now until facet wrap added
                         class = paste0('grid map ', var_name),
                         draggable = TRUE,
                         div(class = 'varname',
                             `data-colnum` = 1,
                             var_name
                         )
          )
        } else {
          # No mapping, so going to check settings and create input
          layer <- layers()[[layer_id()]]

          # Manually set by user if this is not NULL
          aes_val <- layer$aes_params[[aes]]

          # If NULL, set to default value if specified
          if (is.null(aes_val)) {
            aes_val <- layer$geom$default_aes[[aes]]
          }

          # _ Set aesthetic inputs ####
          inputId <- paste0(aes, '-input')
          content <- switch(aes,
            'shape' = sliderInput(inputId = inputId,
                                         label = "",
                                         min = 0,
                                         max = 25,
                                         step = 1,
                                         value = aes_val),
            'colour' = ,
            'fill' = colourpicker::colourInput(inputId = inputId,
                                               label = "",
                                               value = ifelse(!is.na(aes_val), aes_val, 'black')),
            'size' = ,
            'stroke' = sliderInput(inputId = inputId,
                                   label = "",
                                   min = 0.1,
                                   max = 10,
                                   step = 0.1,
                                   value = aes_val),
            'alpha' = sliderInput(inputId = inputId,
                                  label = "",
                                  min = 0,
                                  max = 1,
                                  value = ifelse(!is.na(aes_val), aes_val, 1)),
            'linetype' = sliderInput(inputId = inputId,
                                     label = "",
                                     min = 0,
                                     max = 6,
                                     value = aes_val),
            ''
            )
        }
      }

      bsa <<- bs_append(bsa,
                        title = dropZoneInput(
                          inputId = paste0(aes, '-dropzone'),
                          class = "grid",
                          div(id = aes,
                              class = "aesname",
                              aes
                          )
                        ),
                        content = content
                        )
    })
    bsa
  })

  # _ Geom icons ====
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

  # _ Plot ====
  output$viz <- renderPlot({
    tryCatch(print(values$gg),
             error = function(e) {
               shinyjs::show(id = "help-pane", anim = FALSE)
               shinyjs::html(id = "help-pane", html = e$message)
             })
  })

  # _ Code ====
  output$code <- renderPrint({
    # values$gg$layers
    layers()
  })

  # Events ----------------------

  # _ Done ====
  # User is done - tried this, but didn't work
  #   https://stackoverflow.com/questions/34731975/how-to-listen-for-more-than-one-event-expression-within-a-shiny-eventreactive-ha
  observeEvent(input$done, {
    shinyjs::js$close_window()
    stopApp()
  })

  # _ Cancel ====
  observeEvent(input$cancel, {
    shinyjs::js$close_window()
    stopApp()
  })

  # _ A geom was selected/deselected ====
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

  ## Reactives ----------------------

  # _ Get Layer Id ====
  # Important to use eventReactive here as we want layer_id set on start of program, which is
  #   why ignoreNULL and ignoreInit are both FALSE.  NULL corresponds to the main (blank) layer
  layer_id <- eventReactive(input$js_layer_id, {
    ifelse(is.null(input$js_layer_id), 'geom-blank-layer-0', input$js_layer_id[1])
  }, ignoreNULL = FALSE, ignoreInit = FALSE)

  # _ Get Geom ====
  # The selected geom type responds only to the layer_id
  geom_type <- reactive({
    paste(stringr::str_split(layer_id(), '-')[[1]][1:2], collapse="-")
  })

  # _ Get layers ====
  # Triggered when a new layer is selected
  layers <- reactive({
    # Is layer new?
    num_layers <- length(input$`selected-layers-row`) - 1 # Ignore blank layer
    if (num_layers > 0) {
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
    }
    values$gg$layers
  })

  # _ Get mapping ====
  # What is the current mapping?  State determined by selected layer
  # Note: ifelse didn't work here
  mapping <- reactive({
    if (geom_type() == "geom-blank") {
      return(values$gg$mapping)
    } else {
      return(layers()[[layer_id()]]$mapping)
    }
  })
}
