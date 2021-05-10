library(shiny)
library(serenity.viz)

ui <- function() {
  fluidPage(
    shinyjs::useShinyjs(),
    tags$head(includeCSS(file.path(system.file("www", package = "serenity.viz"), "css", "app.css"))),
    h1("Example: Bar plot y aesthetic"),
    checkboxInput("inherit",
                  "Inherit from base?",
                  value = FALSE),
    actionButton("add_stage", "Add Start Stage"),
    conditionalPanel("input.inherit == true",
                     checkboxInput("linked",
                                   "Link stages?",
                                   value = FALSE)
    ),
    hr(),
    fluidRow(
      column(6, h2("Stage for Base Layer"),
             h4("start"),
             fluidRow(
               column(6, div(class = "aesthetic", layerAesMappingStageUI("base_start"))),
               column(6, verbatimTextOutput("base_start_output")))),
      column(6, h2("Stage for Bar Plot"),
             uiOutput("stage"))
    )
  )
}

server <- function(input, output, session) {
  # Use geom_bar as an example
  output$stage <- renderUI({
    input$add_stage

    tagList(
      h4("start"),
      fluidRow(
        column(6, div(class = "aesthetic", layerAesMappingStageUI("start"))),
        column(6, verbatimTextOutput("start_output"))
      )
    )
  })

  base <- list(
    start = layerAesMappingStageServer(
      id = "base_start",
      stage = "start",
      choices = reactive({
        isolate({ serenity.viz:::dataInputChoices(iris, zone="aeszone") })
      }),
      inherit = reactive({ FALSE }),
      linked = reactive({ FALSE })
    )
  )

  output$base_start_output <- renderText({
    base$start$code()
  })

  # Trigger linked if inherit turned on
  observeEvent(input$inherit, {
    cat(paste("Toggled inherit! Changing linked to", input$inherit, "\n"))
    updateCheckboxInput(session, "linked", value = input$inherit)
  }, ignoreInit = TRUE)

  observeEvent(input$add_stage, {
    stages <<- list(
      # Call start mapping module ----
      start = layerAesMappingStageServer(
        id = "start",
        stage = "start",
        choices = reactive({
          serenity.viz:::dataInputChoices(iris, zone="aeszone", default = switch(input$inherit, base$start$mapping()))
        }),
        default = list(
          mapping = reactive({ switch(input$inherit, base$start$mapping()) }),
          custom_mapping = reactive({ switch(input$inherit, base$start$custom_mapping()) }),
          custom_toggle = base$start$custom_toggle
        ),
        inherit = reactive({ input$inherit }),
        linked = reactive({ input$linked })
      )
    )

    # Turn off linking if stage changed
    observeEvent(stages$start$changed(), {
      req(input$linked)
      cat(paste("Triggered! Changing linked to FALSE\n"))
      updateCheckboxInput(session, "linked", value = FALSE)
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    output$start_output <<- renderText({
      stages$start$code()
    })

    # Deactivate button
    shinyjs::disable("add_stage")
  }, ignoreInit = TRUE, ignoreNULL = TRUE, once = TRUE)
}

shinyApp(ui, server)
