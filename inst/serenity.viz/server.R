library(ggplot2)

source("init_all_sessions.R", local=TRUE)

server <- function(input, output, session) {
  # Stop app on close of browser tab
  # https://github.com/daattali/advanced-shiny/tree/master/auto-kill-app
  session$onSessionEnded(stopApp)

  source("serenity.viz.R", local=TRUE)
}
