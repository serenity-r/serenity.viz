#' UI for layer aesthetic submodule
#'
#' @param id  ID of layer aesthetic
#'
#' @return UI for layer aesthetic
layerAesUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  div(
    class = "aesthetic",
    uiOutput(ns('aes_header_ui'),
             container = tags$header,
             class = "aes-header"),
    tagList(
      # Mappings
      conditionalPanel(condition = "input.edit_value == null || input.edit_value == false",
                       ns = ns,
                       layerAesMappingUI(ns('mapping'))
      ),
      # Values
      conditionalPanel(condition = "input.edit_value == true",
                       ns = ns,
                       layerAesValueUI(ns('value'))
      )
    )
  )
}

#' Server for layer aesthetic submodule
#'
#' @param id ID of layer aesthetic submodule
#' @param geom Geom
#' @param aesthetic Aesthetic
#' @param base_layer_stages  Need base layer aesthetic mapping to use for inheritance
#' @param inherit_aes Reactive: Is this aesthetic inheritable?
#' @param default_geom_aes Default value for geom aesthetic
#' @param default_stat_aes Reactive value of default stat aesthetic - might be mapping!
#' @param required Reactive value of required status for aesthetic
#' @param dataset Dataset
#' @param computed_vars Reactive value of stat computed variables
#' @param aesthetics Reactive value of aesthetics (combines layer and stat aesthetics)
#'
#' @importFrom magrittr %>%
#' @import shiny ggplot2
#'
layerAesServer <- function(id, geom, aesthetic, base_layer_stages, inherit_aes, default_geom_aes,
                           default_stat_aes, required, dataset, computed_vars,
                           aesthetics) {
  moduleServer(
    id,
    function(input, output, session) {
      output$aes_header_ui <- renderUI({
        isolate({
          tagList(
            span(class = "aes-name", aesthetic),
            shinyWidgets::radioGroupButtons(
              session$ns("stage"),
              choices = c(`<i class='fa fa-database'></i>` = "start",
                          `<i class='fa fa-calculator'></i>` = "after_stat",
                          `<i class='fa fa-paint-brush'></i>` = "after_scale")
            ) %>% {
              .$attribs$class <- paste(.$attribs$class, "stageszone")
              .
            },
            div(
              class = paste(c("aes-select", switch((geom == "geom-blank") || is.null(default_geom_aes), "hidden")), collapse = " "),
              shinyWidgets::prettySwitch(
                inputId = session$ns("edit_value"),
                label = '',
                value = isTruthy(input$edit_value),
                inline = TRUE
              ),
              icon("sliders-h", class = ifelse(!isTruthy(input$edit_value), 'inactive', '')) %>%
                {
                  .$attribs$id <- session$ns("sliders-h")
                  .
                }
            ),
            div(
              class = "header-icons",
              prettyToggle(
                inputId = session$ns("scale"),
                label_on = "",
                label_off = "",
                status_on = "default",
                status_off = "default",
                outline = TRUE,
                plain = TRUE,
                icon_on = icon("times"),
                icon_off = icon("ruler"),
                inline = TRUE
              ) %>% {
                .$attribs$class <- paste(c(.$attribs$class, 'hidden', 'disabled'), collapse = " ")
                .
              }
            )
          )
        })
      })
      outputOptions(output, "aes_header_ui", suspendWhenHidden = FALSE)

      # Input checking for default_geom_aes ----

      # https://github.com/tidyverse/ggplot2/issues/4279
      # Remove when available on CRAN
      if (geom == "geom-polygon" && aesthetic == "colour") {
        default_geom_aes <- NA
      }

      # Call value module ----
      value <- layerAesValueServer(
        "value",
        aesthetic = aesthetic,
        initial = default_geom_aes
      )

      # Call mapping module ----
      mapping <- layerAesMappingServer(
        "mapping",
        stage = reactive({ input$stage }),
        aesthetic = aesthetic,
        inheritable = list(
          from_base = reactive({ (geom != "geom-blank") && inherit_aes() }),
          from_stat = reactive({ (geom != "geom-blank") && rlang::is_quosure(default_stat_aes()) })
        ),
        base_layer_stages = base_layer_stages,
        aesthetics = aesthetics,
        default_stat_aes = default_stat_aes,
        dataset = dataset,
        computed_vars = computed_vars
      )

      observeEvent(input$edit_value, {
        # REFACTOR: Change to shinyjs::toggleClass
        shinyjs::toggleClass("inactive",
                             selector = paste(paste0('#', session$ns('aes_header_ui')), '.aes-select', '.fa-database'))
        # if (input$edit_value) {
        #   shinyjs::js$addClass('inactive', paste(paste0('#', session$ns('aes_header_ui')), '.aes-select', '.fa-database'))
        #   shinyjs::js$removeClass('inactive', paste(paste0('#', session$ns('aes_header_ui')), '.aes-select', '.fa-sliders-h'))
        # } else {
        #   shinyjs::js$removeClass('inactive', paste(paste0('#', session$ns('aes_header_ui')), '.aes-select', '.fa-database'))
        #   shinyjs::js$addClass('inactive', paste(paste0('#', session$ns('aes_header_ui')), '.aes-select', '.fa-sliders-h'))
        # }
      }, ignoreInit = TRUE)

      # _ Aesthetic to code ====
      aesToCode <- reactive({
        req(!is.null(input$edit_value))

        list(
          mapping = switch(!input$edit_value && isTruthy(mapping$code()),
                           paste(aesthetic, "=", mapping$code())),
          value = switch(input$edit_value && isTruthy(value()),
                         paste(aesthetic, "=", value()))
        )
      })

      return(
        list(
          stages = mapping$stages,
          code = aesToCode
        )
      )
    }
  )
}
