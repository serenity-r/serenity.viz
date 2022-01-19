#' UI for layer aesthetic stage submodule
#'
#' @param id  ID of layer aesthetic stage submodule
#'
#' @return UI for layer aesthetic stage submodule
#' @export
layerAesMappingStageUI <- function(id) {
  ns <- NS(id)

  uiOutput(ns('stage_ui'))
}

#' Server for layer aesthetic stage submodule
#'
#' @param id  ID of layer aesthetic stage submodule
#' @param stage Which stage? Possible values include \code{start}, \code{after_stat}, and \code{after_scale}.
#' @param choices Reactive: Stage choices (given by \code{dataInputChoices})
#' @param default Reactive or list of reactive default values (\code{mapping},
#'   \code{custom_mapping}, and \code{custom_toggle}). Defaults to NULL for all three. If
#'   a single reactive, then understood as specifying mapping only.
#' @param inherit Is this layer allowing aesthetic inheritance?
#' @param linked Is this aesthetic inheriting from base?
#' @param aesUpdateDependency Trigger update on layer change
#'
#' @return List containing layer aesthetic mapping stage module state.
#' \describe{
#'   \item{mapping}{Reactive expression of stage mapping state}
#'   \item{custom_mapping}{Reactive expression of stage custom mapping state (used for initialization of \code{default})}
#'   \item{custom_toggle}{Reactive expression of stage custom toggle state (used for initialization of \code{default})}
#'   \item{changed}{Reactive expression triggered when mapping changes (used for unlinking to base layer)}
#'   \item{code}{Reactive expression of aesthetic mapping stage code (string)}
#' }
#'
#' @export
layerAesMappingStageServer <- function(id, stage, choices,
                                       default = list(),
                                       inherit = reactive({ TRUE }),
                                       linked = reactive({ TRUE }),
                                       aesUpdateDependency = reactive({ NULL })) {
  moduleServer(
    id,
    function (input, output, session) {
      # Initialization ----

      # Parse defaults
      if (!is.list(default)) {
        default = list(mapping = default)
      }
      default <- do.call(function(...) {
        purrr::list_modify(list(
          mapping = reactive({ NULL }),
          custom_mapping = reactive({ character(0) }),
          custom_toggle = reactive({ FALSE })
        ), ...) }, default)

      # Stage UI ----
      ## _ renderUI ----
      output$stage_ui <- renderUI({
        aesUpdateDependency()

        isolate({
          icons <- tagList(
            div(
              class = "aes-content-icons",
              prettyToggle(
                inputId = session$ns("custom_toggle"),
                value = input$custom_toggle %||% default$custom_toggle() %||% FALSE,
                label_on = "",
                label_off = "",
                status_on = "default",
                status_off = "default",
                outline = TRUE,
                plain = TRUE,
                icon_on = icon("times"),
                icon_off = icon("pencil-alt"),
                inline = TRUE
              ) %>% {
                .$attribs$class <- paste(c(.$attribs$class, "custom-toggle"), collapse = " ")
                .
              }
            )
          )

          content <- tagList(
            dndselectr::dropZoneInput(session$ns("mapping"),
                                      class = "ds-dropzone-wrap",
                                      choices = choices(),
                                      presets = input$mapping %||% default$mapping(),
                                      placeholder = paste("Drag or select",
                                                          switch(stage, "start" = "variable", "after_stat" = "computed", "after_scale" = "aesthetic")),
                                      maxInput = 1,
                                      replaceOnDrop = TRUE),
            shinyWidgets::pickerInput(
              inputId = session$ns("choose_mapping"),
              label = NULL,
              selected = input$mapping %||% default$mapping(),
              choices = dropNulls(list(" " = "",
                                       "Variables" = switch(stage, "start" = names(choices())),
                                       "Computed" = switch(stage, "after_stat" = names(choices())),
                                       "Aesthetics" = switch(stage, "after_scale" = names(choices())))),
              choicesOpt = list(
                content = c(htmltools::doRenderTags(em("Clear variable")),
                            sapply(choices(), function(x) { htmltools::doRenderTags(x) })
                )
              ),
              options = list(
                title = "Nothing selected",
                size = 6,
                `live-search` = ifelse(length(choices()) > 6, TRUE, FALSE),
                `dropup-auto` = FALSE
              )
            ) %>% {
              .$attribs$class <- paste(.$attribs$class, "ds-chooser") # Change aes-choose-data to better class name
              .
            }
          )

          tags$section(
            class = "stage-section",
            icons,
            conditionalPanel(condition = "input.custom_toggle == null || input.custom_toggle == false",
                             ns = session$ns,
                             class = "aes-content",
                             content),
            conditionalPanel(condition = "input.custom_toggle == true",
                             ns = session$ns,
                             layerAesCustomUI(session$ns("custom_mapping")))
          )
        })
      })
      outputOptions(output, "stage_ui", suspendWhenHidden = FALSE)

      # Set up custom mapping module ----
      triggerCustomUpdate <- makeReactiveTrigger()
      # _ layerAesCustomServer ----
      custom_mapping <- layerAesCustomServer(
        "custom_mapping",
        custom_for = reactive({ input$mapping }),
        custom_value = reactive({
          triggerCustomUpdate$depend()
          isolate(default$custom_mapping()) # Avoid updating on default change (only when linked() changes)
        }),
        waitforit = 1,
        aesUpdateDependency = aesUpdateDependency
      )

      # Entangle aesthetic picker and dropzone ----
      dndselectr::entangleInputs(session, mapping = "DropZone", choose_mapping = "Picker")

      # If linking turned on, set mapping, custom and custom_toggle to default ----
      ## _ observeEvent ----
      observeEvent(linked(), {
        if (linked()) {
          shinyWidgets::updatePrettyToggle(session, 'custom_toggle', value = default$custom_toggle() %T||% FALSE)
          dndselectr::updateDropZoneInput(session, 'mapping', presets = default$mapping() %T||% character(0))
          triggerCustomUpdate$trigger()
        }
      }, ignoreInit = TRUE)

      # Deactivate linking if mapping, custom_mapping, or custom_toggle change ----
      triggerChange <- makeReactiveTrigger()

      ## _ observeEvent: If mapping changed, deactivate if different from default ----
      observeEvent(input$mapping, {
        req(linked() && !identical(input$mapping, default$mapping()))

        triggerChange$trigger()
      }, ignoreNULL = FALSE, ignoreInit = TRUE)

      ## _ observeEvent: If custom changed, deactivate if different from default ----
      observeEvent(custom_mapping(), {
        req(linked() && !identical(custom_mapping(), default$custom_mapping()))

        triggerChange$trigger()
      }, ignoreInit = TRUE)

      ## _ observeEvent: If custom_toggle changed, deactivate if different from default ----
      observeEvent(input$custom_toggle, {
        req(linked() && !identical(input$custom_toggle, default$custom_toggle()))

        triggerChange$trigger()
      }, ignoreInit = TRUE)

      # Transfer default changes to inputs (if linking on) ----

      ## _ observeEvent: Pass default mapping changes to mapping ----
      observeEvent(default$mapping(), {
        req(inherit() && linked())

        dndselectr::updateDropZoneInput(session, 'mapping',
                                        choices = choices(),
                                        presets = default$mapping() %||% character(0))
        shinyWidgets::updatePickerInput(session, 'choose_mapping',
                                        choices = dropNulls(list(" " = "",
                                                                 "Variables" = switch(stage, "start" = names(choices())),
                                                                 "Computed" = switch(stage, "after_stat" = names(choices())),
                                                                 "Aesthetics" = switch(stage, "after_scale" = names(choices())))),
                                        choicesOpt = list(
                                          content = c(htmltools::doRenderTags(em("Clear variable")),
                                                      sapply(choices(), function(x) { htmltools::doRenderTags(x) })
                                          )
                                        ),
                                        selected = default$mapping() %||% ""
        )
      }, ignoreNULL = FALSE, ignoreInit = TRUE)

      ## _ observeEvent: Pass default custom changes to custom module ----
      observeEvent(default$custom_mapping(), {
        req(inherit() && linked())

        triggerCustomUpdate$trigger()
      }, ignoreNULL = FALSE, ignoreInit = TRUE)

      ## _ observeEvent: Pass default custom toggle changes to custom toggle ----
      observeEvent(default$custom_toggle(), {
        req(inherit() && linked())

        shinyWidgets::updatePrettyToggle(session, 'custom_toggle', value = default$custom_toggle() %T||% FALSE)
      }, ignoreNULL = FALSE, ignoreInit = TRUE)

      # Update choices() change in dropzone and pickerinput ----
      ## _ observeEvent ----
      observeEvent(choices(), {
        dndselectr::updateDropZoneInput(session, 'mapping',
                                        choices = choices(),
                                        presets = input$mapping %||% character(0))
        shinyWidgets::updatePickerInput(session, 'choose_mapping',
                                        choices = dropNulls(list(" " = "",
                                                                 "Variables" = switch(stage, "start" = names(choices())),
                                                                 "Computed" = switch(stage, "after_stat" = names(choices())),
                                                                 "Aesthetics" = switch(stage, "after_scale" = names(choices())))),
                                        choicesOpt = list(
                                          content = c(htmltools::doRenderTags(em("Clear variable")),
                                                      sapply(choices(), function(x) { htmltools::doRenderTags(x) })
                                          )
                                        ),
                                        selected = input$mapping %||% ""
        )
      }, ignoreNULL = TRUE, ignoreInit = TRUE)

      # Reactive: State of stage module to code ----
      stage_to_code <- reactive({
        req(!is.null(input$custom_toggle))

        mapping <- NULL
        if (input$custom_toggle) {
          # Custom override
          if (!identical(custom_mapping(), default$custom_mapping())) {
            if (identical(custom_mapping(), character(0))) {
              # Only show NULL when default is not empty
              mapping <- "NULL"
            } else {
              mapping <- custom_mapping()
            }
          }
        } else {
          # Mapping
          if (!identical(input$mapping, default$mapping())) {
            if (is.null(input$mapping)) {
              # Only show NULL when default is not NULL
              mapping <- "NULL"
            } else {
              mapping <- ifelse(make.names(input$mapping) == input$mapping,
                                input$mapping,
                                paste0("`", input$mapping, "`"))
            }
          }
        }

        mapping
      })

      # Return: Module outputs ----
      #   - mapping, custom_mapping, and custom_toggle needed for base layer
      #     as defaults
      #   - changed trigger used to set linked() to FALSE for all stages
      #   - code used for plotting
      return(
        list(
          mapping = reactive({
            req(!is.null(input$custom_toggle))
            input$mapping
          }),
          custom_mapping = reactive({
            req(!is.null(input$custom_toggle))
            custom_mapping()
          }),
          custom_toggle = reactive({
            req(!is.null(input$custom_toggle))
            input$custom_toggle
          }),
          changed = reactive({
            triggerChange$depend()
          }),
          code = stage_to_code
        )
      )
    }
  )
}
