Shiny.addCustomMessageHandler('nullify', function(variableName) {
  Shiny.onInputChange(variableName, null);
});

// Stage color matching state needs updating AFTER UI rendered
$(document).on('shiny:value', function(event) {
  if ($(event.target).hasClass('layer-aesthetics')) {
    Shiny.onInputChange("serenityVizApp-layers-post_aesthetics_render_update", Math.random());
  }
});
