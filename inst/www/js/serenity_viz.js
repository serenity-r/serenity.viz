Shiny.addCustomMessageHandler('nullify', function(variableName) {
  Shiny.onInputChange(variableName, null);
});
