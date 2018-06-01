open Webapi.Dom
open Types
open Utils
open State

external toUnsafe : 'a -> < .. > Js.t = "%identity"

let windowHeight = Window.innerHeight window;;
let windowWidth = Window.innerWidth window;;

(* let worldHeight = rows * tileSize;;
   let worldWidth = cols * tileSize;; *)
let magnification = 1.;; (* visual scaling multiplier *)

let drawImage ctx image sourceX sourceY sourceWidth sourceHeight dx dy dWidth dHeight =
  let unsafeCtx = (toUnsafe ctx) in
  ignore @@ unsafeCtx##drawImage image sourceX sourceY sourceWidth sourceHeight dx dy dWidth dHeight;;

let drawLaneObject ctx (sprite:laneObjectT) = 
  let frameCalc = floor (sprite.frameIndex /. 1000.) in
  let img = sprite.img in
  let startX = (float_of_int img.xStart) +. frameCalc *. (float_of_int img.width) in 
  (List.iter (fun i -> 
       drawImage ctx spriteSheet startX img.yStart img.width img.height ((sprite.rect.x +. (float_of_int (img.width * i)) ) *. magnification) sprite.rect.y (magnification *. (float_of_int img.width)) (magnification *. (float_of_int tileSize))
     ) (0<->(img.number-1)));
;;

let drawFrog ctx (frog:frogT) = 
  let img = match frog.direction with 
    | Up -> frogUp;
    | Down -> frogDown;
    | Left -> frogLeft;
    | Right -> frogRight in
  let startX = float_of_int (img.xStart + if frog.leftInJump = 0. then 0 else img.width + 5) in 
  drawImage ctx spriteSheet startX img.yStart img.width img.height ((frog.rect.x-.10.) *. magnification) frog.rect.y (magnification *. (float_of_int img.width)) (magnification *. (float_of_int img.height))

let drawStartScreen ctx = 
  Canvas2dRe.setFillStyle ctx Canvas2dRe.String "white";
  Canvas2dRe.fillRect ctx ~x:0. ~y:0. ~h: (float_of_int height) ~w:(float_of_int width);
  Canvas2dRe.setFillStyle ctx Canvas2dRe.String "black";
  Canvas2dRe.font ctx "60px/1 sans-serif";
  Canvas2dRe.fillText ~x:80. ~y: 200. "Frogger" ctx;
  Canvas2dRe.font ctx "20px/1 sans-serif";
  Canvas2dRe.fillText ~x:80. ~y: 280. "Press any key to start the game" ctx;
;;

let drawWinScreen ctx = 
  Canvas2dRe.setFillStyle ctx Canvas2dRe.String "white";
  Canvas2dRe.fillRect ctx ~x:0. ~y:0. ~h: (float_of_int height) ~w:(float_of_int width);
  Canvas2dRe.setFillStyle ctx Canvas2dRe.String "black";
  Canvas2dRe.font ctx "60px/1 sans-serif";
  Canvas2dRe.fillText ~x:80. ~y: 150. "You Win!" ctx;
  Canvas2dRe.font ctx "20px/1 sans-serif";
  Canvas2dRe.fillText ~x:80. ~y: 280. "Press any key to start another the game" ctx;
;;

let drawLoseScreen ctx = 
  Canvas2dRe.setFillStyle ctx Canvas2dRe.String "white";
  Canvas2dRe.fillRect ctx ~x:0. ~y:0. ~h: (float_of_int height) ~w:(float_of_int width);
  Canvas2dRe.setFillStyle ctx Canvas2dRe.String "black";
  Canvas2dRe.font ctx "60px/1 sans-serif";
  Canvas2dRe.fillText ~x:80. ~y: 200. "You Lose" ctx;
  Canvas2dRe.font ctx "20px/1 sans-serif";
  Canvas2dRe.fillText ~x:80. ~y: 280. "Press any key to start another game" ctx;
;;


let drawGoal ctx = 
  let y = getYForRow 15 in
  drawImage ctx spriteSheet 0 62 398 45 0 (y+halfTileSize) (magnification *. (float_of_int width)) (magnification *. (float_of_int (tileSize + halfTileSize)));;

let drawGrass ctx y = 
  drawImage ctx spriteSheet 0 120 398 33 0 y (magnification *. (float_of_int width)) (magnification *. (float_of_int tileSize));;

let rec drawCars ctx cars = 
  match cars with 
  | [] -> ();
  | hd::tl -> drawLaneObject ctx hd; (drawCars ctx tl)
;;

let drawLives ctx world =
  Canvas2dRe.setFillStyle ctx String("red");
  let livesText = "Lives: " ^ (string_of_int world.lives) in
  Canvas2dRe.fillText ~x:(float_of_int halfTileSize) ~y: ((float_of_int (getYForRow 1)) +. 20.) livesText ctx
;;

let drawTimer ctx world =
  Canvas2dRe.setFillStyle ctx String("red");
  let timerText = "Timer: " ^ (string_of_int (world.timer / 1000)) in
  Canvas2dRe.fillText ~x:(float_of_int (width - tileSize * 3)) ~y: ((float_of_int (getYForRow 1)) +. 20.) timerText ctx
;;

let drawCompletedEndzones ctx world =
  Canvas2dRe.setFillStyle ctx String("purple");
  (List.iter (fun (i, rect) ->
       if (List.assoc i world.endzone) then (
         let x = rect.x in
         Canvas2dRe.fillRect ~x ~y:(float_of_int (tileSize*2))
           ~w: (float_of_int tileSize) ~h: (float_of_int tileSize) ctx;
       ); 
     ) endzoneRects);
;;

let drawBoundingBoxes ctx world = 
  let frogBoxColor = ref "red" in
  (List.iter (fun { rect; } -> (
         let color = if (intersects world.frog.rect rect) then ( frogBoxColor := "blue"; "blue") else "red" in
         Canvas2dRe.setStrokeStyle ctx Canvas2dRe.String color;
         Canvas2dRe.strokeRect ~x: rect.x ~y: rect.y ~w: (float_of_int rect.width) ~h: (float_of_int rect.height) ctx;
       )) (world.objects) );
  let rect = world.frog.rect in
  Canvas2dRe.setStrokeStyle ctx Canvas2dRe.String !frogBoxColor;
  Canvas2dRe.strokeRect ~x: rect.x ~y: rect.y ~w: (float_of_int rect.width) ~h: (float_of_int rect.height) ctx;
;;

let drawGrid ctx = 
  Canvas2dRe.setStrokeStyle ctx Canvas2dRe.String "red";
  (* draw columns *)
  (List.iter (fun i -> (
         Canvas2dRe.beginPath ctx;
         Canvas2dRe.moveTo ~x: (float_of_int (i * tileSize)) ~y: 0. ctx;
         Canvas2dRe.lineTo ~x: (float_of_int (i * tileSize)) ~y: (float_of_int height) ctx;
         Canvas2dRe.stroke ctx;
       )) (0 <-> cols) );

  (* draw rows *)
  (List.iter (fun i -> (
         Canvas2dRe.beginPath ctx;
         Canvas2dRe.moveTo ~x: 0. ~y: (float_of_int (i * tileSize)) ctx;
         Canvas2dRe.lineTo ~x: (float_of_int width) ~y: (float_of_int (i * tileSize)) ctx;
         Canvas2dRe.stroke ctx;
       )) (0 <-> rows)
  );;

let render ctx (world:worldT) = 
  Canvas2dRe.setFillStyle ctx String "rgb(1,4,69)";
  Canvas2dRe.fillRect ctx ~x:0. ~y:0. ~h: (float_of_int height) ~w:(float_of_int width);
  Canvas2dRe.setFillStyle ctx String "black";
  Canvas2dRe.fillRect ctx ~x:0. ~y:(float_of_int (getYForRow 7)) ~h: (float_of_int height) ~w:(float_of_int width);
  if pressedKeys.grid then drawGrid ctx;
  if pressedKeys.bbox then drawBoundingBoxes ctx world;
  (drawGoal ctx);
  (drawGrass ctx (getYForRow 2));
  (drawGrass ctx (getYForRow 8));
  (drawLives ctx world);
  (drawTimer ctx world);
  (drawCars ctx world.objects);
  (drawFrog ctx world.frog);
  (drawCompletedEndzones ctx world);
;;
