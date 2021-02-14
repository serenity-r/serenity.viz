library(shiny)
library(reactlog)
library(serenity.viz)

ui <- function() {
  fluidPage(
    shinyjs::useShinyjs(),
    tags$head(includeCSS(file.path(system.file("www", package = "serenity.viz"), "css", "app.css"))),
    h1("Example: Bar plot y aesthetic"),
    checkboxInput("inherit",
                  "Inherit from base?",
                  value = FALSE),
    actionButton("add_layer", "Add Bar Plot"),
    selectInput("stage", "Choose stage", c("start", "after_stat", "after_scale"), "start"),
    hr(),
    fluidRow(
      column(6, h2("Base Layer"),
             h3("Mapping"),
             div(class = "aesthetic", layerAesMappingUI("base")),
             h3("Code"),
             verbatimTextOutput("base_code")),
      column(6, h2("Bar Plot"),
             h3("Mapping"),
             div(class = "aesthetic", layerAesMappingUI("layer")),
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

  base <- layerAesMappingServer(
    "base",
    stage = reactive({ input$stage }),
    inheritable = list(
      from_base = reactive({ FALSE }),
      from_stat = reactive({ FALSE })
    ),
    base = reactive({ NULL }),
    aesthetic = "y",
    aesthetics = reactive({ aesthetics }),
    default_stat_aes = reactive({ NULL }),
    dataset = iris,
    computed_vars = reactive({ computed_vars })
  )

  # Trigger linked if inherit turned on
  observeEvent(input$inherit, {
    updateCheckboxInput(session, "linked", value = input$inherit)
  }, ignoreInit = TRUE)

  observeEvent(input$add_layer, {
    layer <<- layerAesMappingServer(
      "layer",
      stage = reactive({ input$stage }),
      inheritable = list(
        from_base = reactive({ input$inherit }),
        from_stat = reactive({ TRUE })
      ),
      base = base$stages,
      aesthetic = "y",
      aesthetics = reactive({ aesthetics }),
      default_stat_aes = reactive({ "count" }),
      dataset = iris,
      computed_vars = reactive({ computed_vars })
    )

    output$layer_code <<- renderText({
      layer$code()
    })

    # Deactivate button
    shinyjs::disable("add_layer")
  }, ignoreInit = TRUE, ignoreNULL = TRUE, once = TRUE)

  output$base_code <- renderText({
    base$code()
  })

  # reactlog_module_server()
}

shinyApp(ui, server)
