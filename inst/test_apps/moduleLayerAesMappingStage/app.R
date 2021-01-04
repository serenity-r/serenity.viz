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
    actionButton("add_stages", "Add Stages"),
    conditionalPanel("input.inherit == true",
                     checkboxInput("linked",
                                   "Link stages?",
                                   value = TRUE)
    ),
    hr(),
    fluidRow(
      column(6, h2("Stages for Base Layer"),
             h4("start"),
             fluidRow(
               column(6, div(class = "aesthetic", layerAesMappingStageUI("base_start"))),
               column(6, verbatimTextOutput("base_start_output"))),
             h4("after_stat"),
             shinyjs::disabled(div(class = "aesthetic", layerAesMappingStageUI("base_after_stat"))),
             h4("after_scale"),
             fluidRow(
               column(6, div(class = "aesthetic", layerAesMappingStageUI("base_after_scale"))),
               column(6, verbatimTextOutput("base_after_scale_output")))
             ),
      column(6, h2("Stages for Bar Plot"),
             uiOutput("stages"))
    ),
    verbatimTextOutput("input"),
    reactlog_module_ui()
  )
}

server <- function(input, output, session) {
  # Use geom_bar as an example
  computed_vars <- c("count", "density", "ncount", "ndensity")
  aesthetics <- ggplot2::GeomBar$aesthetics()

  output$stages <- renderUI({
    input$add_stages

    tagList(
      h4("start"),
      fluidRow(
        column(6, div(class = "aesthetic", layerAesMappingStageUI("start"))),
        column(6, verbatimTextOutput("start_output"))
      ),
      h4("after_stat"),
      fluidRow(
        column(6, div(class = "aesthetic", layerAesMappingStageUI("after_stat"))),
        column(6, verbatimTextOutput("after_stat_output"))
      ),
      h4("after_scale"),
      fluidRow(
        column(6, div(class = "aesthetic", layerAesMappingStageUI("after_scale"))),
        column(6, verbatimTextOutput("after_scale_output"))
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
    ),
    after_scale = layerAesMappingStageServer(
      id = "base_after_scale",
      stage = "after_scale",
      choices = reactive({
        isolate({ serenity.viz:::dataInputChoices(aesthetics, zone="aeszone", type = "aesthetics", default = "y") })
      }),
      default = list(
        mapping = reactive({ "y" }),
        custom_mapping = reactive({ "y" })
      ),
      inherit = reactive({ FALSE }),
      linked = reactive({ FALSE })
    )
  )

  output$base_start_output <- renderText({
    base$start$code()
  })

  output$base_after_scale_output <- renderText({
    base$after_scale$code()
  })

  # Trigger linked if inherit turned on
  observeEvent(input$inherit, {
    cat(paste("Toggled inherit! Changing linked to", input$inherit, "\n"))
    updateCheckboxInput(session, "linked", value = input$inherit)
  }, ignoreInit = TRUE)

  observeEvent(input$add_stages, {
    after_scale_default <<- reactive({ ifelse(input$inherit, base$after_scale$mapping(), "y") })
    after_scale_custom_mapping <<- reactive({ ifelse(input$inherit, base$after_scale$custom_mapping(), "y") })
    stages <<- list(
      # Call start mapping module ----
      start = layerAesMappingStageServer(
        id = "start",
        stage = "start",
        choices = reactive({
          serenity.viz:::dataInputChoices(iris, zone="aeszone", default = base$start$mapping())
        }),
        default = list(
          mapping = reactive({ switch(input$inherit, base$start$mapping()) }),
          custom_mapping = reactive({ ifelse(input$inherit, base$start$custom_mapping(), character(0)) }),
          custom_toggle = base$start$custom_toggle
        ),
        inherit = reactive({ input$inherit }),
        linked = reactive({ input$linked })
      ),
      # Call after_stat mapping module ----
      after_stat = layerAesMappingStageServer(
        id = "after_stat",
        stage = "after_stat",
        choices = reactive({
          serenity.viz:::dataInputChoices(computed_vars, zone="aeszone", type = "computed", default = "count")
        }),
        default = list(
          mapping = reactive({ "count" }),
          custom_mapping = reactive({ "count" }) # Why do i need both??
        ),
        inherit = reactive({ FALSE }),
        linked = reactive({ FALSE })
      ),
      # Call after_scale mapping module ----
      after_scale = layerAesMappingStageServer(
        id = "after_scale",
        stage = "after_scale",
        choices = reactive({
          serenity.viz:::dataInputChoices(aesthetics, zone="aeszone", type = "aesthetics", default = after_scale_default())
        }),
        default = list(
          mapping = after_scale_default,
          custom_mapping = after_scale_custom_mapping,
          custom_toggle = base$after_scale$custom_toggle
        ),
        inherit = reactive({ input$inherit }),
        linked = reactive({ input$linked })
      )
    )

    # Turn of linking if stage changed
    observeEvent(c(
      stages$start$changed(),
      stages$after_scale$changed()
    ), {
      req(input$linked)
      cat(paste("Triggered! Changing linked to FALSE\n"))
      updateCheckboxInput(session, "linked", value = FALSE)
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    output$start_output <<- renderText({
      stages$start$code()
    })

    output$after_stat_output <<- renderText({
      stages$after_stat$code()
    })

    output$after_scale_output <<- renderText({
      stages$after_scale$code()
    })

    # Deactivate button
    shinyjs::disable("add_stages")
  }, ignoreInit = TRUE, ignoreNULL = TRUE, once = TRUE)

  reactlog_module_server()
}

shinyApp(ui, server)
