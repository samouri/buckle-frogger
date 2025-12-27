open Js_of_ocaml
open Types
open Utils
open State

let magnification = 1. (* visual scaling multiplier *)

let set_fill_style ctx color =
  ctx##.fillStyle := Js.Unsafe.coerce (Js.string color)

let set_stroke_style ctx color =
  ctx##.strokeStyle := Js.Unsafe.coerce (Js.string color)

let fill_rect ctx ~x ~y ~w ~h = ctx##fillRect x y w h
let stroke_rect ctx ~x ~y ~w ~h = ctx##strokeRect x y w h

let fill_text ctx ~x ~y text = ctx##fillText (Js.string text) x y

let drawImage ctx image sourceX sourceY sourceWidth sourceHeight dx dy dWidth dHeight =
  ctx##drawImage_full image sourceX sourceY sourceWidth sourceHeight dx dy dWidth dHeight

let drawLaneObject ctx (sprite : laneObjectT) =
  let frameCalc = floor (sprite.frameIndex /. 1000.) in
  let img = sprite.img in
  let startX = float_of_int img.xStart +. frameCalc *. float_of_int img.width in
  List.iter
    (fun i ->
      drawImage ctx spriteSheet startX (float_of_int img.yStart)
        (float_of_int img.width) (float_of_int img.height)
        ((sprite.rect.x +. float_of_int (img.width * i)) *. magnification)
        sprite.rect.y
        (magnification *. float_of_int img.width)
        (magnification *. float_of_int tileSize))
    (0 <-> (img.number - 1))

let drawDyingFrog ctx rect leftInAnimation =
  let width = 32 in
  let height = 36 in
  let row = getRowForY (int_of_float rect.y) in
  let y = if row > 7 && row < 13 then 227 else 276 in
  let frameXs = [ 230; 275; 322; 358 ] in
  let framesLength = 4 in
  let frame_duration = float_of_int frogAnimationLength /. 4. in
  let frame =
    min (framesLength - 1)
      (framesLength
       - int_of_float
           (ceil (float_of_int leftInAnimation /. frame_duration)))
  in
  let startX = List.nth frameXs frame in
  drawImage ctx spriteSheet (float_of_int startX) (float_of_int y)
    (float_of_int width) (float_of_int height)
    ((rect.x -. 10.) *. magnification)
    (rect.y -. 8.)
    (magnification *. float_of_int width)
    (magnification *. float_of_int height)

let drawFrog ctx (frog : frogT) =
  match frog.leftInAnimation with
  | Some n -> drawDyingFrog ctx frog.rect n
  | None ->
    let img =
      match frog.direction with
      | Up -> frogUp
      | Down -> frogDown
      | Left -> frogLeft
      | Right -> frogRight
    in
    let startX = float_of_int (img.xStart + if frog.leftInJump = 0. then 0 else img.width + 5) in
    drawImage ctx spriteSheet startX (float_of_int img.yStart)
      (float_of_int img.width) (float_of_int img.height)
      ((frog.rect.x -. 10.) *. magnification)
      frog.rect.y (magnification *. float_of_int img.width)
      (magnification *. float_of_int img.height)

let drawStartScreen ctx =
  set_fill_style ctx "white";
  fill_rect ctx ~x:0. ~y:0. ~h:(float_of_int height) ~w:(float_of_int width);
  set_fill_style ctx "black";
  ctx##.font := Js.string "60px/1 sans-serif";
  fill_text ctx ~x:80. ~y:200. "Frogger";
  ctx##.font := Js.string "20px/1 sans-serif";
  fill_text ctx ~x:80. ~y:280. "Press any key to start the game"

let drawWinScreen ctx =
  set_fill_style ctx "white";
  fill_rect ctx ~x:0. ~y:0. ~h:(float_of_int height) ~w:(float_of_int width);
  set_fill_style ctx "black";
  ctx##.font := Js.string "60px/1 sans-serif";
  fill_text ctx ~x:80. ~y:150. "You Win!";
  ctx##.font := Js.string "20px/1 sans-serif";
  fill_text ctx ~x:50. ~y:280. "Press any key to start another the game"

let drawLoseScreen ctx =
  set_fill_style ctx "white";
  fill_rect ctx ~x:0. ~y:0. ~h:(float_of_int height) ~w:(float_of_int width);
  set_fill_style ctx "black";
  ctx##.font := Js.string "60px/1 sans-serif";
  fill_text ctx ~x:80. ~y:200. "You Lose";
  ctx##.font := Js.string "20px/1 sans-serif";
  fill_text ctx ~x:50. ~y:280. "Press any key to start another game"

let drawGoal ctx =
  let y = getYForRow 15 in
  drawImage ctx spriteSheet 0. 62. 398. 45. 0. (float_of_int (y + halfTileSize))
    (magnification *. float_of_int width)
    (magnification *. float_of_int (tileSize + halfTileSize))

let drawGrass ctx y =
  drawImage ctx spriteSheet 0. 120. 398. 33. 0. (float_of_int y)
    (magnification *. float_of_int width)
    (magnification *. float_of_int tileSize)

let rec drawCars ctx cars =
  match cars with
  | [] -> ()
  | hd :: tl ->
    drawLaneObject ctx hd;
    drawCars ctx tl

let drawLives ctx world =
  set_fill_style ctx "red";
  List.iter
    (fun i ->
      drawImage ctx lifeSprite 0. 0. 34. 40. (float_of_int (10 + (20 * i)))
        (float_of_int (getYForRow 1)) 28. 32.)
    (0 <-> (world.lives - 2))

let drawTimer ctx world =
  set_fill_style ctx "rgb(49,220,39)";
  let pixels =
    int_of_float
      ((float_of_int world.timer /. float_of_int startWorld.timer)
       *. (float_of_int width /. 2.5))
  in
  fill_rect ctx
    ~x:(float_of_int (width - (tileSize * 2) - pixels - 3))
    ~y:(float_of_int (getYForRow 1) +. 10.)
    ~w:(float_of_int pixels) ~h:15.;
  set_fill_style ctx "rgb(251,249,55)";
  fill_text ctx ~x:(float_of_int (width - (tileSize * 2)))
    ~y:(float_of_int (getYForRow 1) +. 25.) "TIME"

let drawScore ctx world =
  let scoreText = padWithZeros (string_of_int world.score) 5 in
  let highscoreText = padWithZeros (string_of_int world.highscore) 5 in
  set_fill_style ctx "rgb(222,222,246)";
  fill_text ctx ~x:(float_of_int (tileSize * 2))
    ~y:(float_of_int (getYForRow 16 + halfTileSize + 3)) "1-UP";
  fill_text ctx ~x:(float_of_int (tileSize * 5))
    ~y:(float_of_int (getYForRow 16 + halfTileSize + 3)) "HI-SCORE";
  set_fill_style ctx "rgb(252,13,27)";
  fill_text ctx ~x:(float_of_int (tileSize + halfTileSize))
    ~y:(float_of_int (getYForRow 15) +. 10.) scoreText;
  fill_text ctx ~x:(float_of_int ((tileSize * 5) + 10))
    ~y:(float_of_int (getYForRow 15) +. 10.) highscoreText

let drawCompletedEndzones ctx world =
  List.iter
    (fun (i, rect) ->
      if List.assoc i world.endzone
      then
        drawImage ctx goalSprite 0. 0. 34. 40. rect.x
          (float_of_int (tileSize * 2)) 28. 32.)
    endzoneRects

let drawBoundingBoxes ctx world =
  let frogBoxColor = ref "red" in
  List.iter
    (fun { rect; _ } ->
      let color =
        if intersects world.frog.rect rect
        then (
          frogBoxColor := "blue";
          "blue")
        else "red"
      in
      set_stroke_style ctx color;
      stroke_rect ctx ~x:rect.x ~y:rect.y ~w:(float_of_int rect.width)
        ~h:(float_of_int rect.height))
    world.objects;
  let rect = world.frog.rect in
  set_stroke_style ctx !frogBoxColor;
  stroke_rect ctx ~x:rect.x ~y:rect.y ~w:(float_of_int rect.width)
    ~h:(float_of_int rect.height)

let drawGrid ctx =
  set_stroke_style ctx "red";
  List.iter
    (fun i ->
      ctx##beginPath;
      ctx##moveTo (float_of_int (i * tileSize)) 0.;
      ctx##lineTo (float_of_int (i * tileSize)) (float_of_int height);
      ctx##stroke)
    (0 <-> cols);
  List.iter
    (fun i ->
      ctx##beginPath;
      ctx##moveTo 0. (float_of_int (i * tileSize));
      ctx##lineTo (float_of_int width) (float_of_int (i * tileSize));
      ctx##stroke)
    (0 <-> rows)

let drawBackground ctx =
  set_fill_style ctx "rgb(1,4,69)";
  fill_rect ctx ~x:0. ~y:0. ~h:(float_of_int height) ~w:(float_of_int width);
  set_fill_style ctx "black";
  fill_rect ctx ~x:0. ~y:(float_of_int (getYForRow 7)) ~h:(float_of_int height)
    ~w:(float_of_int width)

let render ctx (world : worldT) =
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
  drawCompletedEndzones ctx world
