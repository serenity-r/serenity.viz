library(shiny)
library(reactlog)
library(serenity.viz)

`%||%` <- function(a, b) if (!is.null(a)) a else b

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

ui <- function() {
  fluidPage(
    shinyjs::useShinyjs(),
    tags$head(includeCSS(file.path(system.file("www", package = "serenity.viz"), "css", "app.css"))),
    h1("Example: Bar plot aesthetic"),
    checkboxInput("inherit",
                  "Inherit from base?",
                  value = TRUE),
    selectInput("layer",
                "Layer",
                choices = c("Base" = "base", "Bar" = "bar"),
                selected = "base"),
    hr(),
    uiOutput("layer")
  )
}

server <- function(input, output, session) {
  # Use geom_bar as an example
  computed_vars <- c("count", "density", "ncount", "ndensity")
  aesthetics <- ggplot2::GeomBar$aesthetics()

  aesthetic <- "colour"

  triggerAesUpdate <- makeReactiveTrigger()

  layers <- reactiveValues()

  output$layer <- renderUI({
    layer <- input$layer %||% "base"
    triggerAesUpdate$trigger()

    isolate({
      tagList(
        h2(layer),
        h3("Mapping"),
        div(class = "layer-aesthetics", layerAesUI(layer)),
        h3("Code"),
        verbatimTextOutput(paste0(layer, '_code'))
      )
    })
  })

  # Preps geom_blank inputs for layer modules
  base_layer_stages <- list(
    start = list(
      mapping = reactive({ input[['base-mapping-start-mapping']] }),
      custom_mapping = reactive({ input[['base-mapping-start-custom_mapping-custom_text']] %||% '' }),
      custom_toggle = reactive({ input[['base-mapping-start-custom_toggle']] %||% FALSE })
    ),
    after_scale = list(
      mapping = reactive({ input[['base-mapping-after_scale-mapping']] %||% aesthetic }),
      custom_mapping = reactive({ input[['base-mapping-after_scale-custom_mapping-custom_text']] %||% aesthetic }),
      custom_toggle = reactive({ input[['base-mapping-after_scale-custom_toggle']] %||% FALSE })
    )
  )

  observeEvent(input$layer, {
    if (!(input$layer %in% names(layers))) {
      layers[[input$layer]] <<- layerAesServer(
        input$layer,
        geom = ifelse(input$layer == "base", "geom-blank", "geom-bar"),
        aesthetic = aesthetic,
        base_layer_stages = base_layer_stages,
        inherit_aes = reactive({ switch(as.character(isolate(input$layer) == "base"), "TRUE" = FALSE, "FALSE" = input$inherit) }),
        default_geom_aes = switch(input$layer == "bar", geom_bar()$geom$default_aes[[aesthetic]]),
        default_stat_aes = reactive({ switch(isolate(input$layer) == "bar", StatCount$default_aes[[aesthetic]]) }),
        required = TRUE,
        dataset = iris,
        computed_vars = reactive({ computed_vars }),
        aesthetics = reactive({ aesthetics }),
        aesUpdateDependency = reactive({ triggerAesUpdate$depend() })
      )

      output[[paste0(input$layer, '_code')]] <<- renderText({
        req(input$layer %in% names(layers))
        paste0("Mapping: ", layers[[input$layer]]$code()$mapping, "\n",
               "Value: ", layers[[input$layer]]$code()$value)
      })
    }
  }, ignoreNULL = TRUE)
}

shinyApp(ui, server)
