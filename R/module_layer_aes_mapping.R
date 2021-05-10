#' UI for layer aesthetic mapping submodule
#'
#' @param id  ID of layer aesthetic mapping submodule
#'
#' @return UI for layer aesthetic mapping submodule
#' @export
layerAesMappingUI <- function(id) {
  ns <- NS(id)

  uiOutput(
    ns("mapping_ui"),
    container = tags$section,
    class = "mapping-section"
  )
}

#' Server for layer aesthetic mapping submodule
#'
#' @param id ID of layer aesthetic mapping submodule
#' @param stage Reactive of currently selected stage. Stage is one of
#'   \code{start}, \code{after_stat}, or \code{after_scale}.
#' @param aesthetic Aesthetic for this mapping.
#' @param inheritable List of two logical reactives: \code{from_base} and
#'   \code{from_stat}. \code{from_base} is TRUE if mapping can inherit from
#'   the base layer. \code{from_stat} is TRUE if a non-identity stat is set.
#' @param base_layer_stages Returned list of reactives from the
#'   \code{\link{layerAesMappingStageServer}} that stores the state of the
#'   base layer stages. Used for inheritance and linking.
#' @param aesthetics Reactive of aesthetics for \code{after_scale} stage.
#' @param default_stat_aes Reactive value of default stat aesthetic - might be mapping!
#' @param dataset Dataset
#' @param computed_vars Reactive of stat computed variables for \code{after_stat}
#'   stage.
#'
#' @return
#' @export
layerAesMappingServer <- function(id, stage, aesthetic, inheritable, base_layer_stages,
                                  aesthetics, default_stat_aes, dataset, computed_vars) {
  moduleServer(
    id,
    function (input, output, session) {
      output$stage <- reactive({
        stage()
      })
      outputOptions(output, "stage", suspendWhenHidden = FALSE)

      # Mapping UI ----
      output$mapping_ui <- renderUI({
        isolate({
          tagList(
            prettyToggle(
              inputId = session$ns("linked"),
              value = input$linked %T||% TRUE,
              label_on = "",
              label_off = "",
              status_on = "default",
              status_off = "default",
              outline = TRUE,
              plain = TRUE,
              icon_on = icon("link"),
              icon_off = icon("unlink"),
              inline = TRUE
            ) %>% {
              .$attribs$id <- paste0(session$ns("linked"), '-icon')
              .$attribs$class <- paste(c(.$attribs$class,
                                         "link-toggle",
                                         switch(!inheritable$from_base(), "hidden")), collapse = " ")
              .
            },
            conditionalPanel(
              condition = "output.stage == null || output.stage == 'start'",
              ns = session$ns,
              layerAesMappingStageUI(session$ns("start"))
            ),
            conditionalPanel(
              condition = "output.stage == 'after_stat'",
              ns = session$ns,
              layerAesMappingStageUI(session$ns("after_stat"))
            ),
            conditionalPanel(
              condition = "output.stage == 'after_scale'",
              ns = session$ns,
              layerAesMappingStageUI(session$ns("after_scale"))
            )
          )
        })
      })
      outputOptions(output, "mapping_ui", suspendWhenHidden = FALSE)

      after_scale_default <- reactive({ ifelse(inheritable$from_base(), base_layer_stages$after_scale$mapping(), aesthetic) })
      after_scale_custom_mapping <- reactive({ ifelse(inheritable$from_base(), base_layer_stages$after_scale$custom_mapping(), aesthetic) })
      stages <- list(
        # callModule: Call start mapping module ----
        start = layerAesMappingStageServer(
          id = "start",
          stage = "start",
          choices = reactive({
            dataInputChoices(dataset, zone="aeszone", default = switch(inheritable$from_base(), base_layer_stages$start$mapping()))
          }),
          default = list(
            mapping = reactive({ switch(inheritable$from_base(), base_layer_stages$start$mapping()) }),
            custom_mapping = reactive({ ifelse(inheritable$from_base(), base_layer_stages$start$custom_mapping(), character(0)) }),
            custom_toggle = reactive({ ifelse(inheritable$from_base(), base_layer_stages$start$custom_toggle(), FALSE) })
          ),
          inherit = reactive({ inheritable$from_base() }),
          linked = reactive({ input$linked })
        ),
        # callModule: Call after_stat mapping module ----
        after_stat = layerAesMappingStageServer(
          id = "after_stat",
          stage = "after_stat",
          choices = reactive({
            dataInputChoices(computed_vars(), zone="aeszone", type = "computed", default = switch(inheritable$from_stat(), strsplit(rlang::quo_name(default_stat_aes()), "[()]")[[1]][2]))
          }),
          default = list(
            mapping = reactive({ switch(inheritable$from_stat(), strsplit(rlang::quo_name(default_stat_aes()), "[()]")[[1]][2]) }),
            custom_mapping = reactive({ ifelse(inheritable$from_stat(), strsplit(rlang::quo_name(default_stat_aes()), "[()]")[[1]][2], character(0)) }),
            custom_toggle = reactive({ FALSE })
          ),
          inherit = reactive({ FALSE }),
          linked = reactive({ FALSE })
        ),
        # callModule: Call after_scale mapping module ----
        after_scale = layerAesMappingStageServer(
          id = "after_scale",
          stage = "after_scale",
          choices = reactive({
            dataInputChoices(aesthetics(), zone="aeszone", type = "aesthetics", default = after_scale_default())
          }),
          default = list(
            mapping = after_scale_default,
            custom_mapping = after_scale_custom_mapping,
            custom_toggle = reactive({ ifelse(inheritable$from_base(), base_layer_stages$after_scale$custom_toggle(), FALSE) })
          ),
          inherit = reactive({ inheritable$from_base() }),
          linked = reactive({ input$linked })
        )
      )

      # Turn of linking if stage changed
      observeEvent(c(
        stages$start$changed(),
        stages$after_scale$changed()
      ), {
        req(input$linked)
        updateCheckboxInput(session, "linked", value = FALSE)
      }, ignoreNULL = FALSE, ignoreInit = TRUE)

      # Link/unlink on inheritability status change
      observeEvent(inheritable$from_base(), {
        updateCheckboxInput(session, "linked", value = inheritable$from_base())
        shinyjs::toggleClass("linked-icon", "hidden", !inheritable$from_base())
      }, ignoreInit = TRUE)

      # Reactive: State of stage module to code ----
      mapping_to_code <- reactive({
        mapping <- NULL
        mapping <- dropNulls(
          list(
            start = stages$start$code(),
            after_stat = stages$after_stat$code(),
            after_scale = stages$after_scale$code()
          )
        )
        if (length(mapping) == 0) {
          mapping <- NULL
        } else
        if (length(mapping) == 1) {
          mapping <- paste0(
            switch(names(mapping),
                   "after_stat" = "after_stat(",
                   "after_scale" = "after_scale(",
                   ""),
            mapping[[1]],
            switch(names(mapping),
                   "after_stat" = ,
                   "after_scale" = ")",
                   "")
          )
        } else {
          mapping <- paste0(
            "stage(",
            paste(
              mapply(
                function(key, value) { paste(key, "=", value) },
                names(mapping), mapping
              ),
              collapse = ", "),
            ")"
          )
        }

        mapping
      })

      return(
        list(
          stages = stages,
          code = mapping_to_code
        )
      )
    }
  )
}
