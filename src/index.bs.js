// Generated by BUCKLESCRIPT VERSION 3.0.0, PLEASE EDIT WITH CARE
'use strict';

var Canvas2dRe = require("bs-webapi/src/canvas/Canvas2dRe.js");
var Pervasives = require("bs-platform/lib/js/pervasives.js");

function load() {
  var match = document.getElementById("canvas");
  var canvas = (match == null) ? (console.log("cant find canvas canvas \n"), Pervasives.failwith("fail")) : match;
  var context = canvas.getContext("2d");
  Canvas2dRe.setFillStyle(context, /* String */0, "red");
  context.fillRect(0, 0, 100, 100);
  return /* () */0;
}

window.onload = load;

var canvasDimensions = /* record */[
  /* height */400,
  /* width */760
];

exports.canvasDimensions = canvasDimensions;
exports.load = load;
/*  Not a pure module */
