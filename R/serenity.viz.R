## Package documentation ----
# See http://r-pkgs.had.co.nz/man.html#man-packages
#
#' Serenity Viz
#'
#' @name serenity.viz
#' @docType package
#' @import shiny miniUI
NULL

## Launch function ----
# See https://github.com/serenity-r/serenity/wiki/Code-Notes#how-does-a-shiny-app-get-bundled-in-an-r-package
#
#' Launch Serenity Viz in the default browser.
#'
#' @examples
#' \dontrun{
#' serenity.viz::serenity.viz()
#' }
#' @import shiny miniUI
#' @export
serenity.viz <- function() {
  message("Starting Serenity Viz...")
  if (!"package:serenity.viz" %in% search()) {
    if (!suppressMessages(require(serenity.viz)))
      stop("Calling serenity.viz start function but serenity.viz is not installed.")
  }

  viewer <- shiny::dialogViewer("Serenity Viz", width = 900, height = 700)
  shiny::runGadget(shiny::shinyApp(ui = ui(), server), viewer = viewer,
                   stopOnCancel = TRUE)
}
