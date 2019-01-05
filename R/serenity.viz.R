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
#' @import shiny
#' @export
serenity.viz <- function(data = NULL) {
  # Process incoming data
  # Make sure assignment to serenity.viz.data occurs in global namespace
  if (!is.null(data)) {
    serenity.viz.data <<- data
    attr(serenity.viz.data, "df_name") <<- deparse(substitute(data))
  } else {
    serenity.viz.data <<- iris
    attr(serenity.viz.data, "df_name") <<- "iris"
  }

  message("Starting Serenity Viz...")
  if (!"package:serenity.viz" %in% search()) {
    if (!suppressMessages(require(serenity.viz)))
      stop("Calling serenity.viz start function but serenity.viz is not installed.")
  }

  runApp(system.file("serenity.viz", package = "serenity.viz"), launch.browser = TRUE)
}
