ggcode <- reactive({
  code <- attributes(serenity.viz.data)$df_name
  if (isTruthy(subsetted_data())) {
    code <- paste(code,
                  "%>%\n",
                  subsetted_data()
    )
  }

  tmp <- layer_code()
  # if (isTruthy(layer_code())) {
  #   code <- paste(code,
  #                 "%>%\n",
  #                 layer_code()
  #   )
  # }

  return(styler::style_text(code))
})
