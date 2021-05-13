library(shiny)
library(reactlog)
library(serenity.viz)

ui <- function() {
  fluidPage(
    shinyjs::useShinyjs(),
    tags$head(includeCSS(file.path(system.file("www", package = "serenity.viz"), "css", "app.css"))),
    h1("Example: Bar plot aesthetic"),
    checkboxInput("inherit",
                  "Inherit from base?",
                  value = FALSE),
    hr(),
    fluidRow(
      column(6, h2("Base Layer"),
             actionButton("add_base", "Add Base Plot"),
             h3("Mapping"),
             div(class = "layer-aesthetics", layerAesUI("base")),
             h3("Code"),
             verbatimTextOutput("base_code")),
      column(6, h2("Bar Plot"),
             actionButton("add_bar", "Add Bar Plot"),
             h3("Mapping"),
             div(class = "layer-aesthetics", layerAesUI("bar_layer")),
             h3("Code"),
             verbatimTextOutput("layer_code"))
    )
    # ,
    # reactlog_module_ui()
  )
}

server <- function(input, output, session) {
  # Use geom_bar as an example
  computed_vars <- c("count", "density", "ncount", "ndensity")
  aesthetics <- ggplot2::GeomBar$aesthetics()

  aesthetic <- "colour"

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

  observeEvent(input$add_base, {
    base_layer <<- layerAesServer(
      "base",
      geom = "geom-blank",
      aesthetic = aesthetic,
      base_layer_stages = base_layer_stages,
      inherit_aes = reactive({ FALSE }),
      default_geom_aes = NULL,
      default_stat_aes = reactive({ NULL }),
      required = TRUE,
      dataset = iris,
      computed_vars = reactive({ computed_vars }),
      aesthetics = reactive({ aesthetics })
    )

    output$base_code <<- renderText({
      paste0("Mapping: ", base_layer$code()$mapping, "\n",
             "Value: ", base_layer$code()$value)
    })

    # Deactivate button
    shinyjs::disable("add_base")
  }, ignoreInit = TRUE, ignoreNULL = TRUE, once = TRUE)

  observeEvent(input$add_bar, {
    bar_layer <<- layerAesServer(
      "bar_layer",
      geom = "geom-bar",
      aesthetic = aesthetic,
      base_layer_stages = base_layer_stages,
      inherit_aes = reactive({ input$inherit }),
      default_geom_aes = geom_bar()$geom$default_aes[[aesthetic]],
      default_stat_aes = reactive({ StatCount$default_aes[[aesthetic]] }),
      required = TRUE,
      dataset = iris,
      computed_vars = reactive({ computed_vars }),
      aesthetics = reactive({ aesthetics })
    )

    output$layer_code <<- renderText({
      paste0("Mapping: ", bar_layer$code()$mapping, "\n",
             "Value: ", bar_layer$code()$value)
    })

    # Deactivate button
    shinyjs::disable("add_bar")
  }, ignoreInit = TRUE, ignoreNULL = TRUE, once = TRUE)
}

shinyApp(ui, server)
