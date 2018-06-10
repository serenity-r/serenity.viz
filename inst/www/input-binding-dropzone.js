// need to bind on inserted to work with insertUI; $(document).ready doesn't work!
$(document).bind('DOMNodeInserted', function() {
  $(".dropzone").on("dragover", function(ev) {
    ev.preventDefault();
  });

  $(".col.geom").on("dragstart", function(ev) {
    ev.originalEvent.dataTransfer.dropEffect = "copy";
    var i = document.getElementById("selected-layers-row").childElementCount;
    ev.originalEvent.dataTransfer.setData("text/plain", ev.target.id + '-' + i);
  });

  $(".dropzone").on("drop", function(ev) {
    ev.preventDefault();

    var data = ev.originalEvent.dataTransfer.getData("Text");
    geomid = data.split('-',2).join('-');
    layernum = data.split('-')[2];
    layerid = geomid + '-layer-' + layernum;
    if (!document.getElementById(layerid)) { // Likes to add a bazillion elements, probably due to it being a shiny input and triggering based on rate policy
      // Drag-and-copy:  https://stackoverflow.com/questions/13007582/html5-drag-and-copy
      var nodeCopy = document.getElementById(geomid).cloneNode(true);

      // Change attributes from geom to layer
      nodeCopy.classList.remove('geom');
      nodeCopy.classList.add('layer');
      nodeCopy.id = layerid;

      // Add to layers div (child of target - this is due to spec)
      document.getElementById('selected-layers-row').appendChild(nodeCopy);
      var el = $(ev.target);
      el.trigger("change");
    }
  });
});

var dropZoneBinding = new Shiny.InputBinding();

$.extend(dropZoneBinding, {
  find: function(scope) {
    return $(scope).find('.dropzone');
  },
  getValue: function(el) {
    return $(el).children().map(function () {
      return this.classList[1];
    }).get();
  },
  setValue: function(el, value) {
    $(el).text(value);
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
