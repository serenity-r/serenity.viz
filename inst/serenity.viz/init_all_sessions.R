## Available for all R processes (but NOT UI)

# This piece of code creates a reactive trigger so we can force a reactive to execute
#   Our use case is to sometimes force a replot
#   @TODO make sure this is really necessary.  I feel like this might just be some
#     not-so-smart reactive programming.
# https://github.com/daattali/advanced-shiny/tree/master/reactive-trigger
makeReactiveTrigger <- function() {
  rv <- reactiveValues(a = 0)
  list(
    depend = function() {
      rv$a
      invisible()
    },
    trigger = function() {
      rv$a <- isolate(rv$a + 1)
    }
  )
}
