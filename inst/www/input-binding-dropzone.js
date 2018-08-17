// need to bind on inserted to work with insertUI; $(document).ready doesn't work!
$(document).bind('DOMNodeInserted', function() {

  // ***
  // Drag-and-drop draggable elements
  // http://apress.jensimmons.com/v5/pro-html5-programming/ch9.html
  // ***

  // Geoms -> Layers
  $(".col.geom").on("dragstart", function(ev) {
    // This is a copy event
    ev.originalEvent.dataTransfer.dropEffect = "copy";

    // Set the id of the target layer
    geom_id = ev.target.id;
    var m = 0;
    $('#selected-layers-row .' + geom_id).each(function() { m = Math.max(m, this.id.split('-')[3]); });
    layer_num = m + 1;
    layer_id = geom_id + '-layer-' + layer_num;
    ev.originalEvent.dataTransfer.setData("text/plain", layer_id);

    // Set geom so layer dropzone can identify proper drop
    ev.originalEvent.dataTransfer.setData("geom", '');
  });

  // Variables -> Aesthetics
  $(".grid.var").on("dragstart", function(ev) {
    // This is a copy event
    ev.originalEvent.dataTransfer.dropEffect = "copy";

    // Set the id of the target mapping
    varid = ev.target.id;
    var m = 0;
    $('#aesthetics .' + varid.split('.').join('-')).each(function() { m = Math.max(m, this.id.split('-')[2]); });
    mapnum = m + 1;
    mapid = varid + '-map-' + mapnum;

    ev.originalEvent.dataTransfer.setData("text/plain", mapid); // id of child

    // Set var so aesthetic dropzone can identify proper drop
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
      geom_id = data.split('-',2).join('-');
      if (!document.getElementById(data)) { // Likes to add a bazillion elements, probably due to it being a shiny input and triggering based on rate policy
        // Toggle selected class - this is handled through Shiny with the geoms
        var $selected = $("#selected-layers-row").children(".selected");
        $selected.removeClass("selected");

        // Drag-and-copy:  https://stackoverflow.com/questions/13007582/html5-drag-and-copy
        var nodeCopy = document.getElementById(geom_id).cloneNode(true);

        // Change attributes from geom to layer
        nodeCopy.classList.remove('geom');
        nodeCopy.classList.add('layer');
        nodeCopy.classList.add('selected');
        nodeCopy.children[0].classList.remove('selected-geom-inner');
        nodeCopy.children[0].classList.add('layer-inner');
        Shiny.onInputChange("js_layer_id", [data, Math.random()]); // Trigger update of attributes
        nodeCopy.id = data;

        // Add visible icon
        var visel = document.createElement("div");
        visel.classList.add("visible");

        var eyecon = document.createElement("i");
        eyecon.classList.add("fa");
        eyecon.classList.add("fa-eye");

        visel.appendChild(eyecon);
        nodeCopy.children[0].appendChild(visel);

        // Add to layers div (child of target - this is due to spec)
        document.getElementById('selected-layers-row').appendChild(nodeCopy);
      }
    } else { // Variable -> Aesthetic
      varid = data.split('-',1)[0];

      // Get selector for element with possible Shiny inputs (or) mapping variables
      var bs_id = '#' + $('#'+dropid).closest('.panel').attr('id');
      if (!document.getElementById(data) && // Likes to add a bazillion elements, probably due to it being a shiny input and triggering based on rate policy
          $(bs_id + ' .panel-body').find('.' + varid.split('.').join('-')).length === 0) {

        // Unbind all inputs in container then remove
        Shiny.unbindAll(bs_id + ' .panel-body');
        // $(bs_id + ' .panel-body' + ' .shiny-input-container').remove();
        $(bs_id + ' .panel-body').empty();  // Remove everything now - change later to allow for facetting

        // Add mapping element
        var varCopy = document.getElementById(varid).cloneNode(true);

        // Change attributes from var to map
        varCopy.classList.remove('var');
        varCopy.classList.add('map');
        varCopy.id = data;

        // Add to aesthetics div (child of target - this is due to spec)
        document.querySelector(bs_id + ' .panel-body').appendChild(varCopy);
      }
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
    // Needs to update array of layer id's
    var el = $(ev.target);
    el.trigger("change");

    // Trigger layer update - needed due to observeEvent (not reactive right now!!)
    // var layer_id = $(ev.target).children('.selected').attr('id');
    // Shiny.onInputChange("js_layer_id", [layer_id, Math.random()]); // Trigger update of attributes
  });

  // ***
  // Sortable mapping variables
  // ***

  // We want mapping variables to be sortable (implementation of facet wrapping)
  $(".panel-body").sortable({
     items: ".map"
  });
  $(".panel-body").disableSelection();

  // Make sure to trigger a change in the Shiny dropzone input when sorting occurs
  $(".panel-body").on("sortupdate", function(ev) {
    // Needs to update array of mapping variable id's
    var el = $(ev.target).closest('.panel').find('.dropzone');
    el.trigger("change");
  });
});


// Update viz layers
// Shiny.onInputChange('viz_layers',
//   $(el).children().map(function () { if ($(this).is('.layer:not(.noshow)')) { return this.id } }).get());

var dropZoneBinding = new Shiny.InputBinding();

$.extend(dropZoneBinding, {
  find: function(scope) {
    return $(scope).find('.dropzone');
  },
  getValue: function(el) {
    if (el.id == "selected-layers-row") {
      // Return array of layer ids
      return $(el).children().map(function () {
        return this.id;
      }).get();
    } else {
      // Return array of mapping variables
      return $(el).closest('.panel').find('.panel-body').children('.map').map(function () {
        return this.id.split('-')[0];
      }).get();
    }
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
  receiveMessage: function(el, data) {
    if (data.hasOwnProperty('action')) {
      if (data.action == 'change_inherited_status') {
        $(el).closest('.panel').find('.panel-body').children('.aes-wrap').removeClass('inherited');
      } else if (data.action == 'default_off') {
        $(el).closest('.panel').find('.panel-body').children('.aes-wrap').removeClass('default');
      } else if (data.action == 'default_on') {
        $(el).closest('.panel').find('.panel-body').children('.aes-wrap').addClass('default');
      }
    }
  },
  getRatePolicy : function() {
    return {
      policy: 'debounce',
      delay: 250
    };
  }
});

Shiny.inputBindings.register(dropZoneBinding);
