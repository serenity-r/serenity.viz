
shinyjs.close_window = function() {
  window.close();
};

shinyjs.addClass = function(params) {
  $(params[1]).addClass(params[0]);
};

shinyjs.removeClass = function(params) {
  $(params[1]).removeClass(params[0]);
};

shinyjs.toggleClass = function(params) {
  $(params[1]).toggleClass(params[0]);
};
