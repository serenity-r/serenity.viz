source("init_all_sessions.R", local=TRUE)

server <- function(input, output, session) {
  # Stop app on close of browser tab
  # https://github.com/daattali/advanced-shiny/tree/master/auto-kill-app
  session$onSessionEnded(stopApp)

  # "Globals" for server
  var_names <- names(serenity.viz.data)

  # Main
  source("serenity.viz.R", local=TRUE)

  # UI to code
  # source("ggcode.R", local=TRUE)
}
