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
#' @param dataset Passed in dataset for visualization
#' @examples
#' \dontrun{
#' serenity.viz::serenity.viz()
#' }
#' @import shiny
#' @export
serenity.viz <- function(dataset = NULL) {
  # Process incoming data
  if (!is.null(dataset)) {
    attr(dataset, "df_name") <- deparse(substitute(dataset))
  } else {
    dataset <- iris
    attr(dataset, "df_name") <- "iris"
  }

  message("Starting Serenity Viz...")
  if (!"package:serenity.viz" %in% search()) {
    if (!suppressMessages(require(serenity.viz)))
      stop("Calling serenity.viz start function but serenity.viz is not installed.")
  }

  # Gotta be a better way: https://community.rstudio.com/t/pass-variables-to-shiny-app/1950
  serenity.viz.app <- shinyApp(ui = serenityVizUI("serenityVizApp", dataset, titlebar = TRUE),
                               server = function(input, output, session) { serenityVizAppServer(input, output, session, dataset) }
  )
  runApp(serenity.viz.app, launch.browser = TRUE)
}

serenityVizAppServer <- function(input, output, session, dataset) {
  # Stop app on close of browser tab
  # https://github.com/daattali/advanced-shiny/tree/master/auto-kill-app
  session$onSessionEnded(stopApp)

  # Call module
  callModule(module = serenityVizServer,
             id = "serenityVizApp",
             dataset = dataset)
}

