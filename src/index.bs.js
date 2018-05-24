// Generated by BUCKLESCRIPT VERSION 3.0.0, PLEASE EDIT WITH CARE
'use strict';

var Caml_int32 = require("bs-platform/lib/js/caml_int32.js");
var Canvas2dRe = require("bs-webapi/src/canvas/Canvas2dRe.js");
var Pervasives = require("bs-platform/lib/js/pervasives.js");
var Frogger_spritesPng = require("../assets/frogger_sprites.png");

var pressedKeys = /* record */[
  /* left */false,
  /* right */false,
  /* up */false,
  /* down */false,
  /* bbox */0
];

function keydown(evt) {
  var match = evt.keyCode;
  var exit = 0;
  if (match >= 41) {
    var switcher = match - 65 | 0;
    if (switcher > 22 || switcher < 0) {
      exit = 1;
    } else {
      switch (switcher) {
        case 0 : 
            pressedKeys[/* left */0] = true;
            return /* () */0;
        case 1 : 
            pressedKeys[/* bbox */4] = (pressedKeys[/* bbox */4] + 1 | 0) % 2;
            return /* () */0;
        case 3 : 
            pressedKeys[/* right */1] = true;
            return /* () */0;
        case 18 : 
            pressedKeys[/* down */3] = true;
            return /* () */0;
        case 2 : 
        case 4 : 
        case 5 : 
        case 6 : 
        case 7 : 
        case 8 : 
        case 9 : 
        case 10 : 
        case 11 : 
        case 12 : 
        case 13 : 
        case 14 : 
        case 15 : 
        case 16 : 
        case 17 : 
        case 19 : 
        case 20 : 
        case 21 : 
            exit = 1;
            break;
        case 22 : 
            pressedKeys[/* up */2] = true;
            return /* () */0;
        
      }
    }
  } else if (match >= 32) {
    switch (match - 32 | 0) {
      case 1 : 
      case 2 : 
      case 3 : 
      case 4 : 
          exit = 1;
          break;
      case 5 : 
          pressedKeys[/* left */0] = true;
          return /* () */0;
      case 0 : 
      case 6 : 
          pressedKeys[/* up */2] = true;
          return /* () */0;
      case 7 : 
          pressedKeys[/* right */1] = true;
          return /* () */0;
      case 8 : 
          pressedKeys[/* down */3] = true;
          return /* () */0;
      
    }
  } else {
    exit = 1;
  }
  if (exit === 1) {
    console.log("did not find nothing" + evt.keyCode);
    return /* () */0;
  }
  
}

window.addEventListener("keydown", keydown);

var img = new Image();

img.src = Frogger_spritesPng;

function makeSprite(xStart, yStart, frames, frameSpeed) {
  return /* record */[
          /* xStart */xStart,
          /* yStart */yStart,
          /* frames */frames,
          /* frameSpeed */frameSpeed
        ];
}

var startWorld_000 = /* frog : record */[
  /* x */175,
  /* y */387,
  /* width */35,
  /* height */30,
  /* frameIndex */0,
  /* currentSprite : record */[
    /* xStart */0,
    /* yStart */370,
    /* frames */2,
    /* frameSpeed */20
  ],
  /* leftSprite : record */[
    /* xStart */70,
    /* yStart */335,
    /* frames */2,
    /* frameSpeed */20
  ],
  /* rightSprite : record */[
    /* xStart */0,
    /* yStart */335,
    /* frames */2,
    /* frameSpeed */20
  ],
  /* upSprite : record */[
    /* xStart */0,
    /* yStart */370,
    /* frames */2,
    /* frameSpeed */20
  ],
  /* downSprite : record */[
    /* xStart */70,
    /* yStart */370,
    /* frames */2,
    /* frameSpeed */20
  ]
];

var startWorld = /* record */[
  startWorld_000,
  /* width */400,
  /* height */440,
  /* keys */pressedKeys
];

function drawSprite(ctx, sprite) {
  var frameCalc = sprite[/* frameIndex */4] / 1000 | 0;
  var frogFrame = frameCalc >= sprite[/* currentSprite */5][/* frames */2] ? 0 : frameCalc;
  var startX = sprite[/* currentSprite */5][/* xStart */0] + frogFrame * sprite[/* width */2];
  ctx.drawImage(img, startX, sprite[/* currentSprite */5][/* yStart */1], sprite[/* width */2], sprite[/* height */3], sprite[/* x */0], sprite[/* y */1], (sprite[/* width */2] << 0), (sprite[/* height */3] << 0));
  return /* () */0;
}

function drawGoal(ctx) {
  ctx.drawImage(img, 0, 62, 400, 45, 0, 0, 400, 50);
  return /* () */0;
}

function drawGrass(ctx, y) {
  ctx.drawImage(img, 0, 120, 400, 33, 0, y, 400, 33);
  return /* () */0;
}

function render(ctx, world) {
  Canvas2dRe.setFillStyle(ctx, /* String */0, "black");
  ctx.fillRect(0, 0, world[/* width */1], world[/* height */2]);
  drawGoal(ctx);
  drawGrass(ctx, world[/* height */2] - 60 | 0);
  drawGrass(ctx, (world[/* height */2] - 60 | 0) - 180 | 0);
  return drawSprite(ctx, world[/* frog */0]);
}

var lastTime = [Date.now()];

function update(ctx, world) {
  var now = Date.now();
  var dt = now - lastTime[0];
  render(ctx, world);
  lastTime[0] = Date.now();
  var init = world[/* frog */0];
  var frog_000 = /* x */world[/* frog */0][/* x */0] + Caml_int32.imul(42, pressedKeys[/* left */0] ? -1 : (
          pressedKeys[/* right */1] ? 1 : 0
        )) | 0;
  var frog_001 = /* y */world[/* frog */0][/* y */1] + Caml_int32.imul(30, pressedKeys[/* up */2] ? -1 : (
          pressedKeys[/* down */3] ? 1 : 0
        )) | 0;
  var frog_002 = /* width */init[/* width */2];
  var frog_003 = /* height */init[/* height */3];
  var frog_004 = /* frameIndex */pressedKeys[/* down */3] || pressedKeys[/* up */2] || pressedKeys[/* left */0] || pressedKeys[/* right */1] ? 0 : world[/* frog */0][/* frameIndex */4] + dt * world[/* frog */0][/* currentSprite */5][/* frameSpeed */3];
  var frog_005 = /* currentSprite */pressedKeys[/* up */2] ? world[/* frog */0][/* upSprite */8] : (
      pressedKeys[/* down */3] ? world[/* frog */0][/* downSprite */9] : (
          pressedKeys[/* left */0] ? world[/* frog */0][/* leftSprite */6] : (
              pressedKeys[/* right */1] ? world[/* frog */0][/* rightSprite */7] : world[/* frog */0][/* currentSprite */5]
            )
        )
    );
  var frog_006 = /* leftSprite */init[/* leftSprite */6];
  var frog_007 = /* rightSprite */init[/* rightSprite */7];
  var frog_008 = /* upSprite */init[/* upSprite */8];
  var frog_009 = /* downSprite */init[/* downSprite */9];
  var frog = /* record */[
    frog_000,
    frog_001,
    frog_002,
    frog_003,
    frog_004,
    frog_005,
    frog_006,
    frog_007,
    frog_008,
    frog_009
  ];
  pressedKeys[/* left */0] = false;
  pressedKeys[/* right */1] = false;
  pressedKeys[/* up */2] = false;
  pressedKeys[/* down */3] = false;
  var newWorld_001 = /* width */world[/* width */1];
  var newWorld_002 = /* height */world[/* height */2];
  var newWorld_003 = /* keys */world[/* keys */3];
  var newWorld = /* record */[
    /* frog */frog,
    newWorld_001,
    newWorld_002,
    newWorld_003
  ];
  console.log(world);
  requestAnimationFrame((function () {
          return update(ctx, newWorld);
        }));
  return /* () */0;
}

function load() {
  var match = document.getElementById("canvas");
  var canvas = (match == null) ? (console.log("cant find canvas canvas \n"), Pervasives.failwith("fail")) : match;
  canvas.setAttribute("height", String(440) + "px");
  canvas.setAttribute("width", String(400) + "px");
  var context = canvas.getContext("2d");
  return update(context, startWorld);
}

window.onload = load;

var rowheight = 30;

var colwidth = 42;

var worldHeight = 440;

var worldWidth = 400;

var magnification = 1;

exports.pressedKeys = pressedKeys;
exports.keydown = keydown;
exports.img = img;
exports.rowheight = rowheight;
exports.colwidth = colwidth;
exports.makeSprite = makeSprite;
exports.worldHeight = worldHeight;
exports.worldWidth = worldWidth;
exports.startWorld = startWorld;
exports.magnification = magnification;
exports.drawSprite = drawSprite;
exports.drawGoal = drawGoal;
exports.drawGrass = drawGrass;
exports.render = render;
exports.lastTime = lastTime;
exports.update = update;
exports.load = load;
/*  Not a pure module */
