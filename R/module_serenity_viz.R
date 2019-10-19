#' UI for Serenity Viz module
#'
#' @param id Serenity Viz module ID
#' @param dataset Passed in dataset for visualization
#' @param titlebar Show title bar with Done and Cancel buttons?
#' @param showcode Show code for plots?
#' @param height Specify height
#'
#' @return UI for Serenity Viz module
#'
#' @import shiny phosphorr
#' @export
#'
serenityVizUI <- function(id, dataset, titlebar = FALSE, showcode = TRUE, height = NULL) {
  ns <- NS(id)

  miniUI::miniPage(
    style = switch(!is.null(height), paste("height:", height), height),
    shinyjs::useShinyjs(),
    shinyjs::extendShinyjs(
      script = file.path(resourcePath, "js", "shinyjs-funcs.js"),
      functions = c("close_window", "toggleClass", "addClass", "removeClass", "myshow", "myhide")
    ),
    bsplus::use_bs_tooltip(),
    tags$head(includeCSS(file.path(resourcePath, "css", "app.css"))),
    switch(titlebar,
           miniUI::gadgetTitleBar("Serenity Viz",
                                  left = miniUI::miniTitleBarCancelButton(ns("cancel")),
                                  right = miniUI::miniTitleBarButton(ns("done"), "Done", primary = TRUE)),
           NULL),
    phosphorrOutput(ns("pjsbox"), height="100%")
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
#' @import shiny ggplot2 dplyr forcats phosphorr
#' @export
#'
serenityVizServer <- function(input, output, session, dataset) {
  if (is.null(attr(dataset, "df_name"))) {
    attr(dataset, "df_name") <- deparse(substitute(dataset))
  }

  # Server code for layer dropzones
  dragulaSelectR::dropZoneServer(session, "layers", layerUI)
  dragulaSelectR::dropZoneServer(session, "base-layer", layerUI)

  # This stores returned reactives from layer modules
  layer_modules <- reactiveValues()

  # Store log for warnings
  ggplot2_log <- reactiveVal("")

  output$pjsbox <- renderPhosphorr({
    ns <- session$ns

    phosphorr() %>%
      addWidget(id = ns("widget-geoms-and-layers"),
                ui = tagList(
                  widgetHeader(
                    div(
                      style = "display: flex; flex-direction: row; justify-content: flex-end;",
                      actionButton(
                        inputId = ns("add-layer-button"),
                        label = "Add Layer",
                        icon = icon("plus"),
                        style = "padding: 0; display: none;",
                        class = "add-layer"
                      ),
                      actionButton(
                        inputId = ns("remove-layer"),
                        label = "",
                        icon = icon("minus"),
                        style = "border: transparent; padding: 0;"
                      ),
                      prettyToggle(
                        inputId = ns("layer-chooser"),
                        label_on = "",
                        label_off = "",
                        status_on = "default",
                        status_off = "default",
                        outline = TRUE,
                        plain = TRUE,
                        icon_on = icon("times"),
                        icon_off = icon("plus"),
                        inline = TRUE
                      )
                    )
                  ),
                  widgetBody(
                    class = "widget-geoms-and-layers",
                    uiOutput(ns("widget-layers-body"))
                  )
                ),
                title = "Layers",
                icon = icon("layer-group"),
                closable = FALSE) %>%
      addWidget(id = ns('widget-ggplot'),
                refwidget = ns('widget-geoms-and-layers'),
                insertmode = "split-right",
                relsize = 0.6,
                ui = tagList(
                  widgetHeader(
                    uiOutput(ns('widget-ggplot-header'))
                  ),
                  widgetBody(
                    miniUI::miniContentPanel(
                      class = "ggplot",
                      style = "padding: 19px;",
                      plotOutput(ns("viz"), height = "100%"),
                      shinyjs::hidden(
                        absolutePanel(id = ns("help-pane"),
                                      class = "help-pane",
                                      top = "20px",
                                      draggable = FALSE
                        )
                      )
                    )
                  )
                ),
                title = "Plot",
                icon = icon("image"),
                closable = FALSE) %>%
      addWidget(id = ns("widget-code"),
                refwidget = ns('widget-ggplot'),
                insertmode = "split-bottom",
                relsize = 0.25,
                ui = widgetBody(uiOutput(ns("code"),
                                         class="terminal-dark-theme")),
                title = "Code",
                icon = icon("code")) %>%
      addWidget(id = ns("widget-vars"),
                refwidget = ns("widget-geoms-and-layers"),
                insertmode = "split-bottom",
                relsize = 0.65,
                ui = dataUI(id = ns(attributes(dataset)$df_name)),
                title = "Variables",
                icon = icon("database"),
                closable = FALSE) %>%
      addWidget(id = ns("aesthetics"),
                refwidget = ns("widget-vars"),
                insertmode = "split-right",
                ui = uiOutput(ns("aesthetics")),
                title = "Aesthetics",
                icon = icon("paint-brush"),
                closable = FALSE) %>%
      addWidget(id = ns("widget-messages"),
                refwidget = ns("widget-code"),
                insertmode = "tab-after",
                ui = widgetBody(uiOutput(ns("log"),
                                         class="terminal-dark-theme")),
                title = "Messages",
                icon = icon("info")) %>%
      addWidget(id = ns("widget-labels"),
                refwidget = ns('widget-ggplot'),
                insertmode = "tab-after",
                ui = labelsUI(ns("labels")),
                title = "Labels",
                icon = icon("tags"),
                closable = FALSE)
  })

  output$`widget-layers-body` <- renderUI({
    ns <- session$ns

    tagList(
      div(
        class = "layers-wrapper",
        bsplus::bs_collapse(
          id = ns("base_layer_panel"),
          isolate({ # See Example 22 in DragulaSelectR
            dragulaSelectR::dropZoneInput(
              ns("base_layer"),
              class = "layers",
              choices = list(
                'geom-blank' = layerUI("geom-blank")
              ),
              server = layerUI,
              presets = list(values = "geom-blank-ds-1",
                             locked = "geom-blank-ds-1",
                             freeze = "geom-blank-ds-1"),
              multivalued = TRUE,
              selectable = TRUE
            )
          })
        ),
        bsplus::bs_button(
          icon("caret-down"),
          class = "toggle-base-layer"
        ) %>% bsplus::bs_embed_tooltip(title = "Base Layer") %>%
          bsplus::bs_attach_collapse(ns("base_layer_panel")),
        dragulaSelectR::dropZoneInput(
          ns("layers"),
          class = "layers",
          choices = sapply(geoms, function(geom) { layerUI(geom) }, simplify = FALSE, USE.NAMES = TRUE),
          server = layerUI,
          placeholder = "Add a layer",
          multivalued = TRUE,
          selectable = TRUE,
          selectOnDrop = TRUE,
          removeOnSpill = TRUE
        )
      ),
      div(
        class = "layer-chooser-wrapper",
        style = "display: none;",
        dragulaSelectR::dropZoneInput(ns("ds-layer-chooser"), choices = sapply(geoms, function(geom) { layerChoiceUI(geom) }, simplify = FALSE),
                                      class = "layer-chooser",
                                      flex = TRUE,
                                      selectable = TRUE,
                                      direction = "horizontal",
                                      presets = list(values = geoms,
                                                     locked = geoms)
        )
      )
    )
  })

  observeEvent(input$`layer-chooser`, {
    ns <- session$ns

    dragulaSelectR::unselect(session, "ds-layer-chooser")
    if (input$`layer-chooser`) {
      # Toggle header views
      shinyjs::js$myhide(paste0('#', ns("remove-layer")))

      # Toggle body views
      shinyjs::js$myhide('.layers-wrapper')
      shinyjs::js$myshow('.layer-chooser-wrapper')
    } else {
      # Toggle header views
      shinyjs::js$myshow(paste0('#', ns("remove-layer")))
      shinyjs::js$myhide(paste0('#', ns("add-layer-button")))

      # Toggle body views
      shinyjs::js$myshow('.layers-wrapper')
      shinyjs::js$myhide('.layer-chooser-wrapper')
    }
  })

  observeEvent(input$`ds-layer-chooser_selected`, {
    ns <- session$ns

    if (!is.null(input$`ds-layer-chooser_selected`)) {
      shinyjs::js$myshow(paste0('#', ns("add-layer-button")))
    }
  })

  output$`widget-ggplot-header` <- renderUI({
    ns <- session$ns

    tagList(
      prettyToggle(
        inputId = ns("maximize"),
        label_on = "",
        label_off = "",
        status_on = "default",
        status_off = "default",
        outline = TRUE,
        plain = TRUE,
        icon_on = icon("window-minimize"),
        icon_off = icon("window-maximize"),
        inline = TRUE
      ),
      shinyWidgets::dropdownButton(
        HTML("Hello, World!"),
        inputId = ns("plot-params-btn"),
        status = "header-icon",
        icon = icon("gear"),
        size = "xs",
        right = TRUE,
        tooltip = shinyWidgets::tooltipOptions(title = "Plot Parameters", placement = "left"))
    )
  })

  # The next two observe events handle selection of layers
  observeEvent(input$base_layer_selected, {
    if (!is.null(input$layers_selected)) {
      dragulaSelectR::unselect(session, "layers")
    }
  }, ignoreInit = TRUE)

  observeEvent(input$layers_selected, {
    if (!is.null(input$base_layer_selected) && !is.null(input$layers_selected)) {
      dragulaSelectR::unselect(session, "base_layer")
    } else if (is.null(input$base_layer_selected) && is.null(input$layers_selected)) {
      dragulaSelectR::select(session, "geom-blank-ds-1", "base_layer")
    }
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  observeEvent(input$`add-layer-button`, {
    shinyWidgets::updatePrettyToggle(session, "layer-chooser", value = FALSE)
    dragulaSelectR::appendToDropzone(session, input$`ds-layer-chooser_selected`, "layers")
  })

  observeEvent(input$`remove-layer`, {
    if (!is.null(input$layers_selected)) {
      dragulaSelectR::removeSelected(session, "layers")
    }
  })

  observeEvent(input$maximize, {
    ns <- session$ns

    if (input$maximize) {
      phosphorr::phosphorrProxy(ns('pjsbox')) %>% phosphorr::maximizeWidget(ns('widget-ggplot'))
    } else {
      phosphorr::phosphorrProxy(ns('pjsbox')) %>% phosphorr::minimizeWidget(ns('widget-ggplot'))
    }
  })

  # Data module
  subsetted_data <- callModule(module = dataServer,
                               id = attributes(dataset)$df_name,
                               dataset = dataset)

  # Aesthetics UI
  output$aesthetics <- renderUI({
    req(selected_layer())
    layerAestheticsUI(id = paste0(session$ns(selected_layer()),'-aesthetics'))
  })

  all_layers <- reactive({
    req(input$base_layer)
    c(input$base_layer, input$layers)
  })

  selected_layer <- reactive({
    input$layers_selected %||% "geom-blank-ds-1"
  })

  # Get the names of the visible layers
  visible_layers <- reactive({
    setdiff(all_layers(), input$layers_invisible)
  })

  # Preps geom_blank dropzone inputs for layer modules
  geom_blank_inputs_to_reactives <- function() {
    geom_blank_inputs <- as.list(paste0('geom-blank-ds-1-aesthetics-', gg_aesthetics[["geom-blank"]], '-mapping'))
    names(geom_blank_inputs) <- paste0('geom-blank-ds-1-aesthetics-', gg_aesthetics[["geom-blank"]], '-mapping')
    if (any(names(geom_blank_inputs) %in% names(input))) {
      return(geom_blank_inputs %>%
               purrr::map(~ reactive({ input[[.]] })))
    } else {
      return(NULL)
    }
  }

  # Update layer module output reactives - create only once!
  observeEvent(all_layers(), {
    # Adding new layers
    purrr::map(setdiff(all_layers(), names(layer_modules)), ~ {
      layer_modules[[.]] <- callModule(module = layerServer, id = .,
                                       selected_layer,
                                       geom_blank_inputs_to_reactives(),
                                       dataset = dataset,
                                       ggdata = reactive({
                                         if ((. != 'geom-blank-ds-1') && isTruthy(ggobj) && isTruthy(ggobj())) {
                                           return(layer_data(ggobj(), which(input$layers == .)))
                                         } else {
                                           return(NULL)
                                         }
                                         }))
    })

    # Remove old layers
    purrr::map(setdiff(names(layer_modules), all_layers()), ~ { layer_modules[[.]] <- NULL })
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
    # See: https://aryoda.github.io/tutorials/tryCatchLog/tryCatchLog-intro-slides.html#/code-snippet-for-better-error-handling
    tryCatch(
      withCallingHandlers(
        withRestarts(
          print(ggobj()),
          muffleError = function() {
            failure <<- TRUE
            NULL
          }
        ),
        warning = function(w) {
          isolate(ggplot2_log(paste0("[", format(Sys.time(), "%X"), "] <span style='color:#C4A000'>**Warning**</span>: ", w$message, "<br/>", ggplot2_log())))
          invokeRestart("muffleWarning")
        },
        message = function(m) {
          isolate(ggplot2_log(paste0("[", format(Sys.time(), "%X"), "] <span style='color:#3d77c2'>**Message**</span>: ",  m$message, "<br/>", ggplot2_log())))
          invokeRestart("muffleMessage")
        },
        error = function(e) {
          if (nchar(e$message)) {
            isolate(ggplot2_log(paste0("[", format(Sys.time(), "%X"), "] <span style='color:#CC0000'>**Error**</span>: ", e$message, "<br/>", ggplot2_log())))
            shinyjs::show(id = "help-pane", anim = FALSE)
            shinyjs::html(id = "help-pane", html = e$message)
          }
          invokeRestart("muffleError")
        }),
      finally = {
        if (!failure) shinyjs::hide(id = "help-pane", anim = FALSE)
      }
    )
  })

  # _ Code ====
  output$code <- renderUI({
    req(ggcode())
    lines <- fansi::sgr_to_html(prettycode::highlight(ggcode(),
                                                      style = terminal_dark_theme()))
    HTML(
      paste0(
        purrr::map2(
          lines,
          purrr::map(gregexpr("^\\s+", lines), ~ attr(., "match.length")),
          ~ ifelse(.y > 0, stringr::str_replace(.x, "^\\s+", paste0(rep("&nbsp;", .y), collapse = "")), .x)
        ),
        collapse = "<br/>")
    )
  })
  outputOptions(output, "code", suspendWhenHidden = FALSE)  # Look into Shiny way of handling tabs

  output$log <- renderUI({
    req(ggplot2_log())
    HTML(markdown::markdownToHTML(text = ggplot2_log(), fragment.only = TRUE))
  })
  outputOptions(output, "log", suspendWhenHidden = FALSE)

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

    if (isTruthy(labs_code())) {
      code <- paste(code,
                    "+\n",
                    labs_code()
      )
    }

    return(styler::style_text(code))
  })

  ggobj <- reactive({
    req(ggcode())
    eval(parse(text=gsub(attributes(dataset)$df_name, "dataset", ggcode())))
  })

  # BEGIN: Labels module ----

  # _ label reactives ----
  xlabel <- reactive({
    req(ggobj())
    ggobj()$labels$x
  })

  ylabel <- reactive({
    req(ggobj())
    ggobj()$labels$y
  })

  labs_code <- callModule(module = labelsServer,
                          id = "labels",
                          xlabel = xlabel,
                          ylabel = ylabel)

  # END: Labels module ----

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

geoms <- c("geom-bar", "geom-histogram", "geom-point", "geom-line", "geom-dotplot", "geom-boxplot", "geom-violin", "geom-rug", "geom-smooth")
plot_names <- list(
  "geom-bar" = "Bar Plot",
  "geom-histogram" = "Histogram",
  "geom-point" = "Scatter Plot",
  "geom-line" = "Line Plot",
  "geom-dotplot" = "Dot Plot",
  "geom-boxplot" = "Box Plot",
  "geom-violin" = "Violin Plot",
  "geom-rug" = "Rug Plot",
  "geom-smooth" = "Smoother"
)

NA_defaults <- list(
  fill = "#000000",
  alpha = 1
)

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
  "geom-bar" = ggplot2::GeomBar$aesthetics(),
  "geom-histogram" = ggplot2::GeomBar$aesthetics(),
  "geom-point" = ggplot2::GeomPoint$aesthetics(),
  "geom-line" = ggplot2::GeomLine$aesthetics(),
  "geom-dotplot" = ggplot2::GeomDotplot$aesthetics(),
  "geom-boxplot" = c(ggplot2::GeomBoxplot$aesthetics(), "y"),
  "geom-violin" = ggplot2::GeomViolin$aesthetics(),
  "geom-rug" = ggplot2::GeomRug$aesthetics(),
  "geom-smooth" = ggplot2::GeomSmooth$aesthetics()
)
ordering <- unique(unlist(gg_aesthetics))
gg_aesthetics <- purrr::map(gg_aesthetics, ~ ordering[ordering %in% .])

idsToGeoms <- function(id) {
  switch(!is.null(id),
         sapply(id, FUN = function(x) { paste(stringr::str_split(x, '-')[[1]][1:2], collapse="-") }, simplify = "array"),
         NULL)
}

revList <- function(x) {
  tmp <- names(x)
  names(tmp) <- unlist(x)
  as.list(tmp)
}

terminal_dark_theme <- function() {
  mystyle <- prettycode::default_style()
  mystyle$call <- crayon::make_style("#06989A")
  mystyle$number <- crayon::make_style("#3465A4")
  mystyle$null <- crayon::make_style("#CC0000")
  mystyle$operator <- crayon::make_style("#4E9A06")
  return(mystyle)
}
