// need to bind on inserted to work with insertUI; $(document).ready doesn't work!
$(document).bind('DOMNodeInserted', function() {
  $(".dropzone").on("dragover", function(ev) {
    ev.preventDefault();
  });

  $(".col.geom").on("dragstart", function(ev) {
    ev.originalEvent.dataTransfer.dropEffect = "copy";
    ev.originalEvent.dataTransfer.setData("text/plain", ev.target.id);
  });

  $(".dropzone").on("drop", function(ev) {
    ev.preventDefault();
    var data = ev.originalEvent.dataTransfer.getData("Text");

    // prevent images from stacking on top of each other
    ev.target.appendChild(document.getElementById(data));
    var el = $(ev.target);
    el.trigger("change");
  });
});

var dropZoneBinding = new Shiny.InputBinding();

$.extend(dropZoneBinding, {
  find: function(scope) {
    return $(scope).find('.dropzone');
  },
  getValue: function(el) {
    return $(el).text();
  },
  setValue: function(el, value) {
    $(el).text();
  },
  subscribe: function(el, callback) {
    $(el).on("change.dropZoneBinding", function(e) {
      callback();
    });
  },
  unsubscribe: function(el) {
    $(el).off(".dropZoneBinding");
  },
  getRatePolicy : function() {
    return {
      policy: 'debounce',
      delay: 250
    };
  }
});

Shiny.inputBindings.register(dropZoneBinding);
