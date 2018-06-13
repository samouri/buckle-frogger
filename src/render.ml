open Webapi.Dom
open Types
open Utils
open State

external toUnsafe : 'a -> < .. > Js.t = "%identity"

(* let worldHeight = rows * tileSize;;
   let worldWidth = cols * tileSize;; *)
let magnification = 1.;; (* visual scaling multiplier *)

let drawImage ctx image sourceX sourceY sourceWidth sourceHeight dx dy dWidth dHeight =
  let unsafeCtx = (toUnsafe ctx) in
  ignore @@ unsafeCtx##drawImage image sourceX sourceY sourceWidth sourceHeight dx dy dWidth dHeight;
  ();
;;

let drawLaneObject ctx (sprite:laneObjectT) = 
  let frameCalc = floor (sprite.frameIndex /. 1000.) in
  let img = sprite.img in
  let startX = (float_of_int img.xStart) +. frameCalc *. (float_of_int img.width) in 
  (List.iter (fun i -> 
       drawImage ctx spriteSheet startX img.yStart img.width img.height ((sprite.rect.x +. (float_of_int (img.width * i)) ) *. magnification) sprite.rect.y (magnification *. (float_of_int img.width)) (magnification *. (float_of_int tileSize))
     ) (0<->(img.number-1)));
;;

let drawDyingFrog ctx rect leftInAnimation = 
  let width = 32 in
  let height = 36 in
  let row = getRowForY (int_of_float rect.y) in
  let y = if row > 7 && row < 13 then 227 else 276 in (* water means sink. car and endzone death is explosion *)
  let frameXs = [230; 275; 322; 358; ] in
  let framesLength = 4 in
  let frame = min (framesLength - 1 ) (framesLength - (int_of_float (ceil ((float_of_int leftInAnimation) /. ((float_of_int frogAnimationLength) /. 4.))))) in
  let startX = List.nth frameXs frame in
  drawImage ctx spriteSheet startX y width height ((rect.x-.10.) *. magnification) (rect.y-.8.) (magnification *. (float_of_int width)) (magnification *. (float_of_int height));
;;

let drawFrog ctx (frog:frogT) = 
  match frog.leftInAnimation with
  | Some n -> drawDyingFrog ctx frog.rect n
  | None ->
    let img = match frog.direction with 
      | Up -> frogUp;
      | Down -> frogDown;
      | Left -> frogLeft;
      | Right -> frogRight in
    let startX = float_of_int (img.xStart + if frog.leftInJump = 0. then 0 else img.width + 5) in
    drawImage ctx spriteSheet startX img.yStart img.width img.height ((frog.rect.x-.10.) *. magnification) frog.rect.y (magnification *. (float_of_int img.width)) (magnification *. (float_of_int img.height));
;;

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
  Canvas2dRe.fillText ~x:50. ~y: 280. "Press any key to start another the game" ctx;
;;

let drawLoseScreen ctx = 
  Canvas2dRe.setFillStyle ctx Canvas2dRe.String "white";
  Canvas2dRe.fillRect ctx ~x:0. ~y:0. ~h: (float_of_int height) ~w:(float_of_int width);
  Canvas2dRe.setFillStyle ctx Canvas2dRe.String "black";
  Canvas2dRe.font ctx "60px/1 sans-serif";
  Canvas2dRe.fillText ~x:80. ~y: 200. "You Lose" ctx;
  Canvas2dRe.font ctx "20px/1 sans-serif";
  Canvas2dRe.fillText ~x:50. ~y: 280. "Press any key to start another game" ctx;
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
  (List.iter 
     (fun i -> (drawImage ctx lifeSprite 0 0 34 40 (10 + 20*i) (getYForRow 1) 28 32))
     (0<->(world.lives-2))
  )
;;

let drawTimer ctx world =
  Canvas2dRe.setFillStyle ctx String("rgb(49,220,39)"); (* green *)
  let pixels = int_of_float (((float_of_int world.timer) /. (float_of_int startWorld.timer) ) *. ((float_of_int width) /. 2.5)) in 
  Canvas2dRe.fillRect ~x:(float_of_int (width - tileSize * 2 - pixels - 3)) ~y: ((float_of_int (getYForRow 1)) +. 10.) ~w:(float_of_int pixels) ~h:15. ctx;
  Canvas2dRe.setFillStyle ctx String("rgb(251,249,55)"); (* yellow *)
  Canvas2dRe.fillText ~x:(float_of_int (width - tileSize * 2)) ~y: ((float_of_int (getYForRow 1)) +. 25.) "TIME" ctx
;;

let drawScore ctx world =
  let scoreText = padWithZeros (string_of_int world.score) 5 in
  let highscoreText = padWithZeros (string_of_int world.highscore) 5 in
  Canvas2dRe.setFillStyle ctx String("rgb(222,222,246)"); (* whiteish *)
  Canvas2dRe.fillText ~x:(float_of_int (tileSize * 2)) ~y: ((float_of_int (getYForRow 16 + halfTileSize + 3))) "1-UP" ctx;
  Canvas2dRe.fillText ~x:(float_of_int (tileSize * 5)) ~y: ((float_of_int (getYForRow 16 + halfTileSize + 3))) "HI-SCORE" ctx;
  Canvas2dRe.setFillStyle ctx String("rgb(252,13,27)"); (* reddish *)
  Canvas2dRe.fillText ~x:(float_of_int (tileSize + halfTileSize)) ~y: ((float_of_int (getYForRow 15) +. 10.)) scoreText ctx;
  Canvas2dRe.fillText ~x:(float_of_int (tileSize * 5 + 10)) ~y: ((float_of_int (getYForRow 15) +. 10.)) highscoreText ctx;
;;

let drawCompletedEndzones ctx world =
  (List.iter (fun (i, rect) ->
       if (List.assoc i world.endzone) then (
         let x = rect.x in
         (drawImage ctx goalSprite 0 0 34 40 x (tileSize*2) 28 32)
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

let drawBackground ctx = 
  Canvas2dRe.setFillStyle ctx String "rgb(1,4,69)";
  Canvas2dRe.fillRect ctx ~x:0. ~y:0. ~h: (float_of_int height) ~w:(float_of_int width);
  Canvas2dRe.setFillStyle ctx String "black";
  Canvas2dRe.fillRect ctx ~x:0. ~y:(float_of_int (getYForRow 7)) ~h: (float_of_int height) ~w:(float_of_int width);
;;

let render ctx (world:worldT) = 
  drawBackground ctx;
  if input.grid then drawGrid ctx;
  if input.bbox then drawBoundingBoxes ctx world;
  drawGoal ctx;
  drawGrass ctx (getYForRow 2);
  drawGrass ctx (getYForRow 8);
  drawLives ctx world;
  drawTimer ctx world;
  drawScore ctx world;
  drawCars ctx world.objects;
  drawFrog ctx world.frog;
  drawCompletedEndzones ctx world;
;;
