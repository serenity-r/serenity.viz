ggcode <- reactive({
  code <- attributes(serenity.viz.data)$df_name
  if (isTruthy(subsetted_data())) {
    code <- paste(code,
                  "%>%\n",
                  subsetted_data()
    )
  }

  code <- paste(code,
                "%>%\n",
                "ggplot()"
                )

  # code <- do.call('paste', c(code, as.list(input$`selected-layers-row`), sep = '+'))
  # code <- paste(code, input$`selected-layers-row`, sep='+')

  return(styler::style_text(code))
})
