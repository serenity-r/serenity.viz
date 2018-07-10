shinyjs.init = function() {
  // Trigger geom click
  $("#selected-geoms-row").on("click", ".col", function(event) {
    var colnum = $(event.target).data("colnum");
    Shiny.onInputChange("jsGeomNum", [colnum, Math.random()]);
  });

  // Trigger layer click
  $("#selected-layers-row").on("click", ".col", function(event) {
    var $layer = $(event.target).closest('.col');
    var colId = $layer.attr('id');

    // Toggle selected class - this is handled through Shiny with the geoms
    var $selected = $("#selected-layers-row").children(".selected");
    if ($selected.attr('id') != colId) {
      $selected.removeClass("selected");
      $layer.addClass("selected");

      Shiny.onInputChange("jsLayerId", [colId, Math.random()]);
    }
  });

  // Trigger layer visibility
  $("#selected-layers-row").on("click", ".visible > .svg-inline--fa", function(event) {
    event.stopPropagation(); // Don't trigger a layer selection

    $(this).toggleClass("fa-eye fa-eye-slash");
    $(this).closest(".col").toggleClass("noshow");
  });
};

shinyjs.closeWindow = function() {
  window.close();
};
