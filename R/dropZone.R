#' Create a drop zone for drag-and-drop
#'
#' Create an input control for drag-and-drop behavior
#'
#' @param inputId The \code{input} slot that will be used to access the value.
#' @param label Display label for the control, or `\code{NULL} for no label.
#' @import shiny
#' @export
dropZoneInput <- function(inputId,..., class = "dropzone", style = NULL) {
  # declare dependencies
  shiny::addResourcePath("dropzone-binding",
                         system.file("www", package = "serenity.viz"))
  deps <- list(
    htmltools::htmlDependency(
      "dropzone-binding", "0.1.0", c(href = "dropzone-binding"),
      script = "input-binding-dropzone.js"
    )
  )

  inputTag <- div(
      id = inputId,
      class = class,
      style = style,
      list(...)
  )

  htmltools::attachDependencies(inputTag, deps)
}
