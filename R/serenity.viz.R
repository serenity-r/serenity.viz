## Package documentation ----
# See http://r-pkgs.had.co.nz/man.html#man-packages
#
#' Serenity Viz
#'
#' @name serenity.viz
#' @docType package
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
#' @export
serenity.viz <- function() {
  message("Starting Serenity Viz...")
  if (!"package:serenity.viz" %in% search()) {
    if (!suppressMessages(require(serenity.viz)))
      stop("Calling serenity.viz start function but serenity.viz is not installed.")
  }

  # resourcePath <- system.file("gadgets", "serenity.viz",
  #                             package = "serenity.viz")

  # viewer <- shiny::dialogViewer("Serenity Viz", width = 1000, height = 600)
  viewer <- shiny::browserViewer()
  shiny::runGadget(shiny::shinyApp(ui = ui(), server), viewer = viewer,
                   stopOnCancel = TRUE)
}
