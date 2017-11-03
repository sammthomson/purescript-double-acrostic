"use strict";


/** Creates an input for a single capital letter. */
exports.cUpperChar = function(nextInput) {
  return function (previousInput) {
      return function(idx) {
      return function(initial) {
        return function(send) {
          return function() {
            var uid = getUniqueID();
            var input = document.createElement("input");
            input.type = "text";
            input.value = initial;
            input.size = 1;
            input.id = uid;
            input.style = "border: 0;";

            var idxDiv = document.createElement("div");
            idxDiv.className += " idx";
            idxDiv.appendChild(document.createTextNode(idx));

            var letterDiv = document.createElement("div");
            letterDiv.className += " letter";
            letterDiv.appendChild(input);

            // select all by default when moving to a char input
            function focusAndSelectAll(target) {
              if (target != null) {
                target.focus();
                target.select();
              }
            }
            input.addEventListener("input", function(e) {
              var target = e.srcElement || e.target;
              var value = target.value;
              var length = target.value.length;
              if (length >= 1) {
                focusAndSelectAll(nextInput(target)());
              }
              // upper-case and truncate to the first char
              value = value.substring(value.length - 1, value.length).toUpperCase();
              e.target.value = value;
              send(value)();
            });
            // allow navigating to previous/next input using arrow keys
            input.addEventListener("keydown", function(e) {
              var target = e.srcElement || e.target;
              if (e.key == "ArrowRight") {
                focusAndSelectAll(nextInput(target)());
              } else if (e.key == "ArrowLeft") {
                focusAndSelectAll(previousInput(target)());
              }
            });
            // move to the previous input when the backspace key is *released*
            // (so only one char is deleted per keystroke, and current char is
            // deleted before moving to previous input.)
            input.addEventListener("keyup", function(e) {
              var target = e.srcElement || e.target;
              if (e.key == "Backspace") {
                focusAndSelectAll(previousInput(target)());
              }
            });

            return [idxDiv, letterDiv];
          };
        };
      };
    };
  };
};

/**
 * Makes a 1-row table from a 2-d array of elements.
 * Each table cell gets an array of elements put into it.
 */
exports.makeRow = function(cells) {
  return function() {
    var table = document.createElement("table");
    var tr = document.createElement("tr");
    for (var i = 0; i < cells.length; i++) {
      var td = document.createElement("td");
      for (var j = 0; j < cells[i].length; j++) {
        td.appendChild(cells[i][j]);
      }
      tr.appendChild(td);
    }
    table.appendChild(tr);
    return table;
  };
};


// CHEATING BECAUSE CONSTRUCTORS ARE NOT EXPORTED
function id(x) { return x; }
// newtype constructor
exports.mkUi = id;
// product type constructor
exports.mkFlare = (function () {
  function Flare(value0) {
    var flare = this;
    return function (value1) {
      flare.value0 = value0;
      flare.value1 = value1;
      return flare;
    };
  };
  return Flare;
})();

// COPIED FROM purescript-flare BECAUSE IT'S NOT EXPORTED

// This function maintains a global state `window.flareID` to generate unique
// DOM element IDs. It is only called from functions with a DOM effect.
function getUniqueID() {
  if (window.flareID === undefined) {
    window.flareID = 0;
  }
  window.flareID = window.flareID + 1;
  return "flare-component-" + window.flareID.toString();
}
