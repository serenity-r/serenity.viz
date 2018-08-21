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
    gg = ggplot2::ggplot(data = iris), # , aes(x = Sepal.Width, y = Sepal.Length)),
    layers = list()
  )

  # https://github.com/daattali/advanced-shiny/tree/master/reactive-trigger
  makeReactiveTrigger <- function() {
    rv <- reactiveValues(a = 0)
    list(
      depend = function() {
        rv$a
        invisible()
      },
      trigger = function() {
        rv$a <- isolate(rv$a + 1)
      }
    )
  }

  forcePlot <- makeReactiveTrigger()

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
            `data-colnum` = var_num, # Do we need the data-colnum attribute?
            var_names[var_num]
        )
      )
    })
  })

  # _ Aesthetic divs ====
  #
  # Depends:
  #   geom_type()
  #
  output$aesthetics <- renderUI({
    layer_id()

    # Only want aesthetics UI dependent on layer changes
    # Individual outputs have their own updating functions
    isolate({
      bsa <- bs_accordion(id = "acc") %>%
        bs_set_opts(panel_type = "success", use_heading_link = TRUE)
      lapply(aesthetics(), function(aes) {
        # Main ggplot2 object -> Mapping only!
        if (geom_type() == "geom-blank") {
          # Is aesthetic already set to a mapping?
          if (rlang::is_quosure(mapping()[[aes]])) {
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
          if ((length(mapping()) > 0)  && rlang::is_quosure(mapping()[[aes]])) {
            # We've got an aesthetic mapping
            var_name <- as.character(rlang::get_expr(mapping()[[aes]]))
            inherited <- ''
          } else if (values$layers[[layer_id()]]$inherit.aes && rlang::is_quosure(values$gg$mapping[[aes]])) {
            # Inherit mapping
            var_name <- as.character(rlang::get_expr(values$gg$mapping[[aes]]))
            inherited <- 'inherited'
          }

          # This assumes mappings are ONLY variable names - can be more general
          if (!is.null(var_name)) {
            content <- div(id = paste0(var_name,'-map-1'), # Only 1 for now until facet wrap added
                           class = paste0('grid map ', var_name),
                           draggable = TRUE,
                           div(class = paste0('varname ', inherited),
                               `data-colnum` = 1,
                               var_name
                           )
            )
          } else {
            # No mapping, so going to check settings and create input
            layer <- values$layers[[layer_id()]]

            # Manually set by user if this is not NULL
            aes_val <- layer$aes_params[[aes]]
            default <- ''

            # If NULL, set to default value if specified (which might be NA!!!)
            if (is.null(aes_val) && !is.null(layer$geom$default_aes[[aes]])) {
              aes_val <- layer$geom$default_aes[[aes]]
              default <- 'default'
            }

            # If NULL (e.g. GROUP) or NA (e.g. fill), not set yet and required or not necessary
            inputId <- paste0(aes, '-input-', layer_id())
            if (is.null(aes_val)) {
              content <- span(
                'Not set'
              )
            } else {
              # _ Set aesthetic inputs ####

              if (is.na(aes_val)) {
                content <- span(
                  'Not set'
                )
              } else {
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
                                                                     value = dplyr::filter(colours_tbl, name == aes_val)$hex), # Assumes colour always R name not hex
                                  'weight' = ,
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
                                                        value = aes_val),
                                  'linetype' = sliderInput(inputId = inputId,
                                                           label = "",
                                                           min = 0,
                                                           max = 6,
                                                           value = aes_val),
                                  ''
                )
              }

              # Surrounding div for buttons and labels
              content <- div(
                id = paste0(aes, '-wrap'),
                class = paste0('aes-wrap ', default),
                content
              )
            }
          }
        }

        bsa <<- bs_append(bsa,
                          title = dropZoneInput(
                            inputId = paste0(aes, '-dropzone-', layer_id()),
                            class = "grid",
                            div(id = aes,
                                class = "aesname",
                                aes
                            )
                          ),
                          content = content
        )
      })
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
    forcePlot$depend()

    failure <- FALSE
    # Try to plot.  If unsuccessful, pass error message to help pane.
    tryCatch(print(values$gg),
             error = function(e) {
               shinyjs::show(id = "help-pane", anim = FALSE)
               shinyjs::html(id = "help-pane", html = e$message)
               failure <<- TRUE
             })
    if (!failure) {
      shinyjs::hide(id = "help-pane", anim = FALSE)
    }
  })

  # _ Code ====
  output$code <- renderPrint({
    # values$layers[[layer_id()]]$mapping
    # active_layers()
    values$layers
    print(values$layers[[layer_id()]]$mapping)
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
  #
  # Depends:
  #   input$js_geom_num
  #
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

  # _ Update Layers ====
  #
  # Comments:
  #   Only triggered via new layer or reshuffling (i.e. `selected-layers-row` dropzone changes)
  #
  observeEvent(input$`selected-layers-row`, {
    # Is layer new?
    num_layers <- length(input$`selected-layers-row`) - 1 # Ignore blank layer

    if (num_layers > 0) {
      if (length(values$layers) < num_layers) {
        # New layer added - add to gg object (temporary until all required aesthetics are filled)
        values$layers[[layer_id()]] <- eval(parse(text=paste0(stringr::str_replace(geom_type(), "-", "_"), "()")))

        # Geom mapping starts as NULL - set to aes()
        values$layers[[layer_id()]]$mapping <- aes()
      } else {
        # Just a reshuffling of layers - address accordingly
        values$layers <- values$layers[input$`selected-layers-row`[1 + (1:num_layers)]]
      }
    }
  }, priority = 1)

  ## _ Ready Layer One ====
  #
  # Comments:
  #   Need to isolate changes to reactive variable to avoid infinite loop
  #   Add inherited and default to make this work right!!!
  observe({
    lapply(isolate(aesthetics()), function(aes) {
      # First, set mapping if present
      var <- input[[paste0(aes, '-dropzone-', layer_id())]]
      if (!is.null(var) && var != '') {
        isolate({
          if (geom_type() == "geom-blank") {
            values$gg$mapping[[aes]] <- quo(!!sym(var))
          } else {
            # Only set layer mapping if (1) does not inherit from base, or (2) does and base not set
            # TODO: Allow for override of inherited mapping in the future
            if (!values$layers[[layer_id()]]$inherit.aes ||
                (values$layers[[layer_id()]]$inherit.aes &&
                 (!rlang::is_quosure(values$gg$mapping[[aes]]) ||
                  (values$gg$mapping[[aes]] != var)))) {
              # Set mapping aesthetic
              values$layers[[layer_id()]]$mapping[[aes]] <- quo(!!sym(var))

              # Remove from aes_params (seems to prioritize over mapping if set)
              values$layers[[layer_id()]]$aes_params[[aes]] <- NULL

              # Change inherited status
              session$sendInputMessage(paste0(aes, '-dropzone-', layer_id()), list(action = 'change_inherited_status'))
            }
          }
          values$gg$labels[[aes]] <- var
        })
      } else {
        # No mapping - set by input if present (has to be layer for now!!!)
        aes_input <- input[[paste0(aes, '-input-', layer_id())]]
        isolate({
          if ((geom_type() != "geom-blank") && !is.null(aes_input)) {
            # TODO:  Default values can be NA!!!!!Create a button for setting a value...
            default_value <- values$layers[[layer_id()]]$geom$default_aes[[aes]]

            # Convert default colour values to hex
            if ((aes %in% c('colour', 'fill')) && !is.na(default_value)) {
              default_value <- dplyr::filter(colours_tbl, name == default_value)$hex
            }

            if (is.null(default_value) || (!is.na(default_value) && (default_value != aes_input))) {
              # No default - set parameter
              values$layers[[layer_id()]]$aes_params[[aes]] <- aes_input
              session$sendInputMessage(paste0(aes, '-dropzone-', layer_id()), list(action = 'default_off'))
            } else {
              # Gonna use default value by, er, default
              values$layers[[layer_id()]]$aes_params[[aes]] <- NULL
              session$sendInputMessage(paste0(aes, '-dropzone-', layer_id()), list(action = 'default_on'))
            }

            # Force update of plot
            forcePlot$trigger()
          } else {
            # Condition: No mapping set and geom_blank
            # ind <- which(aesthetics() == aes) - 1
            # span('Not set')
          }
        })
      }
    })
  })

  # Print only active layers
  observe({
    values$gg$layers <- active_layers()
  })

  ## Reactives ----------------------

  # _ Get Current Layer Id ====
  #
  # Depends:
  #   input$js_layer_id
  #
  # Comments:
  #   Important to use eventReactive here as we want layer_id set on start of program, which is
  #   why ignoreNULL and ignoreInit are both FALSE.  NULL corresponds to the main (blank) layer
  #
  layer_id <- eventReactive(input$js_layer_id, {
    ifelse(is.null(input$js_layer_id), 'geom-blank-layer-0', input$js_layer_id[1])
  }, ignoreNULL = FALSE, ignoreInit = FALSE)


  # _ Get Current Geom ====
  #
  # Depends:
  #   layer_id()
  #
  # Comments:
  #   The selected geom type responds only to the layer_id
  #
  geom_type <- reactive({
    paste(stringr::str_split(layer_id(), '-')[[1]][1:2], collapse="-")
  })

  # _ Get Active Layers ====
  #
  # Depends:
  #   values$layers
  #   input$js_active_layers
  #
  # Comments:
  #   Triggered via change in values$layers as well as layer show/hide event in shinyjs-funcs.js
  #   Send message to selected-layers-row dropzone input asking for active layers.  Response goes
  #     to input$js_active_layers.
  #
  active_layers <- reactive({
    if (length(values$layers) > 0) {
      # Has a hide/show layer button been pressed yet?
      # Note: Need to check if named input in names of the input reactiveVariables object
      #   is.null(input$js_active_layers) won't work when the value of the input is NULL!!!
      if ('js_active_layers' %in% names(input)) {
        return(values$layers[input$js_active_layers])
      } else {
        return(values$layers)
      }
    } else {
      return(list())
    }
  })

  # _ Get Current Mapping ====
  #
  # Depends:
  #   layer_id()
  #   geom_type()
  #   values$layers
  #   values$gg$mapping
  #
  # Comments:
  #   What is the current mapping?  State determined by selected layer
  #   Note: ifelse didn't work here
  #
  mapping <- reactive({
    if (geom_type() == "geom-blank") {
      return(values$gg$mapping)
    } else {
      return(values$layers[[layer_id()]]$mapping)
    }
  })

  # _ Get Current Aesthetics ====
  #
  # Depends:
  #   geom_type()
  #
  # Comments:
  #   May not need this.
  #
  aesthetics <- reactive({
    if (geom_type() == "geom-blank") {
      return(gg_aesthetics[["default"]])
    } else {
      return(eval(parse(text=paste0(stringr::str_replace(geom_type(), "-", "_"), "()")))$geom$aesthetics())
    }
  })

}
