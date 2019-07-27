#' UI for layer module
#'
#' @param id  Layer ID
#'
#' @return UI for layer
#'
layerUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  geom_type <- paste(stringr::str_split(ns(''), '-')[[1]][1:2], collapse="-")

  if (geom_type == "geom-blank") {
    geom_name <- tagList("Base", br(), "Layer")
  } else {
    geom_name <- plot_names[[geom_type]]
  }

  div(
    class = "layer-wrap layer",
    tagList(
      div(
        class = "layer-title",
        tagList(
          div(
            class = "geom-layer-title",
            div(class = paste("geom-icon", geom_type)),
            span(class = "geom-name", geom_name)
          ),
          switch(geom_type != "geom-blank", span("Other stuff", class="hidden"), NULL)
        )
      ),
      wellPanel(
        class = "layer-settings",
        "Stuff to say"
      ),
      div(
        class = "layer-icons",
        tagList(
          switch(geom_type != "geom-blank",
                 div(icon("cog")),
                 NULL),
          switch(geom_type != "geom-blank",
                 div(class = "ds-toggle-visible", icon("eye")),
                 NULL),
          div(icon("question"))
        )
      )
    )
  )
}

#' UI for layer choice
#'
#' @param geom  Layer geom
#'
#' @return UI for layer chioce
#'
layerChoiceUI <- function(geom) {
  div(
    class = "layer-wrap choice",
    tagList(
      div(
        class = "layer-title",
        tagList(
          div(class = paste("geom-icon", geom)),
          div(plot_names[geom], class = "plot-name")
        )
      ),
      div(
        class = "layer-icons",
        tagList(
          icon("question"),
          icon("check")
        )
      )
    )
  )
}

#' Server for layers module
#'
#' @param input   Shiny inputs
#' @param output  Shiny outputs
#' @param session Shiny user session
#'
#' @importFrom magrittr %>%
#' @import shiny ggplot2
#'
layerServer <- function(input, output, session) {
}
