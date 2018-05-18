shinyjs.init = function() {
  $("#selected-cols-row").on("click", ".col", function(event) {
    var colnum = $(event.target).data("colnum");
    Shiny.onInputChange("jsColNum", [colnum, Math.random()]);
  });
};

shinyjs.closeWindow = function() {
  window.close();
};
