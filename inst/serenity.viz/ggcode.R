ggcode <- reactive({
  code <- paste(
    "ggplot(data = iris)"
  )
  code <- do.call('paste', c(code, as.list(input$`selected-layers-row`), sep = '+'))
  # code <- paste(code, input$`selected-layers-row`, sep='+')

  return(code)
})
