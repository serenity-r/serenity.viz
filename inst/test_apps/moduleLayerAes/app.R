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
    actionButton("add_layer", "Add Bar Plot"),
    hr(),
    fluidRow(
      column(6, h2("Base Layer"),
             h3("Mapping"),
             div(class = "layer-aesthetics", layerAesUI("base")),
             h3("Code"),
             verbatimTextOutput("base_code")),
      column(6, h2("Bar Plot"),
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

  base_layer <- layerAesServer(
    "base",
    geom = "geom-blank",
    aesthetic = aesthetic,
    base_layer_stages = NULL,
    inherit_aes = reactive({ FALSE }),
    default_geom_aes = NULL,
    default_stat_aes = reactive({ NULL }),
    required = TRUE,
    dataset = iris,
    computed_vars = reactive({ computed_vars }),
    aesthetics = reactive({ aesthetics })
  )

  observeEvent(input$add_layer, {
    bar_layer <<- layerAesServer(
      "bar_layer",
      geom = "geom-bar",
      aesthetic = aesthetic,
      base_layer_stages = base_layer$stages,
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
    shinyjs::disable("add_layer")
  }, ignoreInit = TRUE, ignoreNULL = TRUE, once = TRUE)

  output$base_code <- renderText({
    paste0("Mapping: ", base_layer$code()$mapping, "\n",
          "Value: ", base_layer$code()$value)
  })

  # reactlog_module_server()
}

shinyApp(ui, server)
