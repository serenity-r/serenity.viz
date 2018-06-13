// need to bind on inserted to work with insertUI; $(document).ready doesn't work!
$(document).bind('DOMNodeInserted', function() {
  // ***
  // Drag-and-drop draggable elements
  // ***

  // Geoms -> Layers
  $(".col.geom").on("dragstart", function(ev) {
    ev.originalEvent.dataTransfer.dropEffect = "copy";
    var i = document.getElementById("selected-layers-row").childElementCount;
    ev.originalEvent.dataTransfer.setData("text/plain", ev.target.id + '-' + i);
    ev.originalEvent.dataTransfer.setData("geom", '');
  });

  // Variables -> Aesthetics
  $(".grid.var").on("dragstart", function(ev) {
    ev.originalEvent.dataTransfer.dropEffect = "link";
    ev.originalEvent.dataTransfer.setData("text/plain", $(ev.target)[0].children[0].id); // id of child
    ev.originalEvent.dataTransfer.setData("var", '');
  });

  // ***
  // Dropzones
  // ***

  // Signify allowable drop
  // Multiple dropzones:  https://stackoverflow.com/questions/11065803/determine-what-is-being-dragged-from-dragenter-dragover-events/11089592#11089592
  $(".dropzone").on("dragover", function(ev) {
    if (((ev.target.id == "selected-layers-row") && (ev.originalEvent.dataTransfer.types[1] == "geom")) ||
        ((ev.target.closest('#acc') && (ev.target.closest('#acc').parentElement.id == "aesthetics") && (ev.originalEvent.dataTransfer.types[1] == "var"))))
         {
      ev.preventDefault();
    }
  });

  $(".dropzone").on("drop", function(ev) {
    ev.preventDefault();

    var data = ev.originalEvent.dataTransfer.getData("Text");
    var dropid = ev.target.id;
    if (dropid === "selected-layers-row") { // Geom -> Layer
      // data is geom information
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
      }
    } else { // Variable -> Aesthetic
      console.log(data); // Variable
      console.log(dropid); // Aesthetic
    }

    // Trigger change in Shiny input
    var el = $(ev.target);
    el.trigger("change");
  });

  // ***
  // Sortable layers
  // ***

  // We want layers to be sortable (except for main blank layer, which has unsortable class)
  $("#selected-layers-row").sortable({
     items: ".col:not(.unsortable)"
  });
  $("#selected-layers-row").disableSelection();

  // Make sure to trigger a change in the Shiny dropzone input when sorting occurs
  $(".dropzone").on("sortupdate", function(ev) {
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
