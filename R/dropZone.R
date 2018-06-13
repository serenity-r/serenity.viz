#' Create a drop zone for drag-and-drop
#'
#' Create an input control for drag-and-drop behavior
#'
#' @param inputId The \code{input} slot that will be used to access the value.
#' @param label Display label for the control, or `\code{NULL} for no label.
#' @import shiny
#' @export
dropZoneInput <- function(inputId,..., class = "", style = NULL) {
  # declare dependencies
  shiny::addResourcePath("dropzone-binding",
                         system.file("www", package = "serenity.viz"))
  shiny::addResourcePath("underscore-lib",
                         system.file("www", "shared", "underscore", package = "serenity.viz"))
  deps <- list(
    htmltools::htmlDependency(
      "dropzone-binding", "0.1.0", c(href = "dropzone-binding"),
      script = "input-binding-dropzone.js"
    ),
    htmltools::htmlDependency("jqueryui", "1.11.4", c(href="shared/jqueryui"),
                   script = "jquery-ui.min.js"), # For some reason can't add resource path above for jqueryui
    htmltools::htmlDependency("underscore", "1.9.1", c(href="underscore-lib"),
                              script = "underscore-min.js")
  )

  inputTag <- div(
      id = inputId,
      class = paste(class, "dropzone"),
      style = style,
      list(...)
  )

  htmltools::attachDependencies(inputTag, deps)
}
