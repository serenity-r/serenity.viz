library(shiny)
library(ggplot2)
library(serenity.viz)

geom <- "geom-smooth"
geom_proto <- eval(parse(text=paste0(stringr::str_replace(geom, "-", "_"), "()")))

ui <- function() {
  fluidPage(
    shinyjs::useShinyjs(),
    tags$head(includeCSS(file.path(system.file("www", package = "serenity.viz"), "css", "app.css"))),
    checkboxInput("inherit",
                  "Inherit from base?",
                  value = FALSE),
    actionButton("add_layer", "Add Layer"),
    hr(),
    fluidRow(
      column(6, h2("Base Layer"),
             h3("Aesthetics"),
             layerAestheticsUI(id = "geom-blank", name = 'Base Layer'),
             h3("Code"),
             verbatimTextOutput("base_code")),
      column(6, h2("New Layer"),
             h3("Aesthetics"),
             layerAestheticsUI(id = geom, name = 'New Layer'),
             h3("Code"),
             verbatimTextOutput("layer_code"))
    )
  )
}

server <- function(input, output, session) {
  # Creates list of inputs for all base layer aesthetic mapping stages
  # Needed for layer inheritance
  get_base_layer_aesthetics <- function(prefix = 'geom-blank-ds-1-aesthetics') {
    geom_blank_inputs <- as.list(paste0(prefix, '-', gg_aesthetics[["geom-blank"]]))
    names(geom_blank_inputs) <- gg_aesthetics[["geom-blank"]]
    return(
      geom_blank_inputs %>%
        purrr::map(~ list(
          start = list(
            mapping = reactive({ input[[paste0(.,'-mapping-start-mapping')]] }),
            custom_mapping = reactive({ input[[paste0(.,'-mapping-start-custom_mapping-custom_text')]] %||% '' }),
            custom_toggle = reactive({ input[[paste0(.,'-mapping-start-custom_toggle')]] %||% FALSE })
          ),
          after_scale = list(
            mapping = reactive({ input[[paste0(.,'-mapping-after_scale-mapping')]] }),
            custom_mapping = reactive({ input[[paste0(.,'-mapping-after_scale-custom_mapping-custom_text')]] %||% '' }),
            custom_toggle = reactive({ input[[paste0(.,'-mapping-after_scale-custom_toggle')]] %||% FALSE })
          )
        )))
  }


  base_layer <- layerAestheticsServer(
    id = "geom-blank",
    geom = "geom-blank",
    layer_selected = reactive({ "geom-blank" }),
    base_layer_aesthetics = NULL,
    dataset = iris,
    inherit_aes = reactive({ FALSE }),
    layer_stat = reactive({ "identity" })
  )

  observeEvent(input$add_layer, {
    geom_layer <<- layerAestheticsServer(
      id = geom,
      geom = geom,
      layer_selected = reactive({ geom }),
      base_layer_aesthetics = get_base_layer_aesthetics("geom-blank"),
      dataset = iris,
      inherit_aes = reactive({ input$inherit }),
      layer_stat = reactive({ serenity.viz:::camelToSnake(stringr::str_remove(class(geom_proto$stat)[1], "Stat")) })
    )

    output$layer_code <<- renderText({
      geom_layer$code()
    })

    # Deactivate button
    shinyjs::disable("add_layer")
  }, ignoreInit = TRUE, ignoreNULL = TRUE, once = TRUE)

  output$base_code <- renderText({
    base_layer$code()
  })
}

shinyApp(ui, server)
