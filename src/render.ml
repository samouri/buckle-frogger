open Bindings
open Direction
open Types
open Utils
open Game

let magnification = 1. (* visual scaling multiplier *)

let drawLaneObject ctx (sprite : laneObjectT) =
  let frameCalc = floor (sprite.frameIndex /. 1000.) in
  let img = sprite.img in
  let startX = float_of_int img.xStart +. frameCalc *. float_of_int img.width in
  List.iter
    (fun i ->
      Canvas.draw_image ctx spriteSheet ~source_x:startX
        ~source_y:(float_of_int img.yStart) ~source_w:(float_of_int img.width)
        ~source_h:(float_of_int img.height)
        ~dx:((sprite.rect.x +. float_of_int (img.width * i)) *. magnification)
        ~dy:sprite.rect.y
        ~d_w:(magnification *. float_of_int img.width)
        ~d_h:(magnification *. float_of_int tileSize))
    (0 <-> (img.number - 1))

let drawDyingFrog ctx rect leftInAnimation =
  let width = 32 in
  let height = 36 in
  let row = getRowForY (int_of_float rect.y) in
  let y = if row > 7 && row < 13 then 227 else 276 in
  let frameXs = [ 230; 275; 322; 358 ] in
  let framesLength = 4 in
  let frame_duration = float_of_int frog_animation_length /. 4. in
  let frame =
    min (framesLength - 1)
      (framesLength
       - int_of_float
           (ceil (float_of_int leftInAnimation /. frame_duration)))
  in
  let startX = List.nth frameXs frame in
  Canvas.draw_image ctx spriteSheet ~source_x:(float_of_int startX)
    ~source_y:(float_of_int y) ~source_w:(float_of_int width)
    ~source_h:(float_of_int height) ~dx:((rect.x -. 10.) *. magnification)
    ~dy:(rect.y -. 8.) ~d_w:(magnification *. float_of_int width)
    ~d_h:(magnification *. float_of_int height)

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
    Canvas.draw_image ctx spriteSheet ~source_x:startX
      ~source_y:(float_of_int img.yStart) ~source_w:(float_of_int img.width)
      ~source_h:(float_of_int img.height)
      ~dx:((frog.rect.x -. 10.) *. magnification) ~dy:frog.rect.y
      ~d_w:(magnification *. float_of_int img.width)
      ~d_h:(magnification *. float_of_int img.height)

let drawStartScreen ctx =
  Canvas.set_fill_style ctx "white";
  Canvas.fill_rect ctx ~x:0. ~y:0. ~h:(float_of_int height)
    ~w:(float_of_int width);
  Canvas.set_fill_style ctx "black";
  Canvas.set_font ctx "60px/1 sans-serif";
  Canvas.fill_text ctx ~x:80. ~y:200. "Frogger";
  Canvas.set_font ctx "20px/1 sans-serif";
  Canvas.fill_text ctx ~x:80. ~y:280. "Press any key to start the game"

let drawWinScreen ctx =
  Canvas.set_fill_style ctx "white";
  Canvas.fill_rect ctx ~x:0. ~y:0. ~h:(float_of_int height)
    ~w:(float_of_int width);
  Canvas.set_fill_style ctx "black";
  Canvas.set_font ctx "60px/1 sans-serif";
  Canvas.fill_text ctx ~x:80. ~y:150. "You Win!";
  Canvas.set_font ctx "20px/1 sans-serif";
  Canvas.fill_text ctx ~x:50. ~y:280. "Press any key to start another the game"

let drawLoseScreen ctx =
  Canvas.set_fill_style ctx "white";
  Canvas.fill_rect ctx ~x:0. ~y:0. ~h:(float_of_int height)
    ~w:(float_of_int width);
  Canvas.set_fill_style ctx "black";
  Canvas.set_font ctx "60px/1 sans-serif";
  Canvas.fill_text ctx ~x:80. ~y:200. "You Lose";
  Canvas.set_font ctx "20px/1 sans-serif";
  Canvas.fill_text ctx ~x:50. ~y:280. "Press any key to start another game"

let drawGoal ctx =
  let y = getYForRow 15 in
  Canvas.draw_image ctx spriteSheet ~source_x:0. ~source_y:62.
    ~source_w:398. ~source_h:45. ~dx:0.
    ~dy:(float_of_int (y + halfTileSize))
    ~d_w:(magnification *. float_of_int width)
    ~d_h:(magnification *. float_of_int (tileSize + halfTileSize))

let drawGrass ctx y =
  Canvas.draw_image ctx spriteSheet ~source_x:0. ~source_y:120.
    ~source_w:398. ~source_h:33. ~dx:0. ~dy:(float_of_int y)
    ~d_w:(magnification *. float_of_int width)
    ~d_h:(magnification *. float_of_int tileSize)

let rec drawCars ctx cars =
  match cars with
  | [] -> ()
  | hd :: tl ->
    drawLaneObject ctx hd;
    drawCars ctx tl

let drawLives ctx world =
  Canvas.set_fill_style ctx "red";
  List.iter
    (fun i ->
      Canvas.draw_image ctx lifeSprite ~source_x:0. ~source_y:0.
        ~source_w:34. ~source_h:40. ~dx:(float_of_int (10 + (20 * i)))
        ~dy:(float_of_int (getYForRow 1)) ~d_w:28. ~d_h:32.)
    (0 <-> (world.lives - 2))

let drawTimer ctx world =
  Canvas.set_fill_style ctx "rgb(49,220,39)";
  let pixels =
    int_of_float
      ((float_of_int world.timer /. float_of_int start_timer_ms)
       *. (float_of_int width /. 2.5))
  in
  Canvas.fill_rect ctx
    ~x:(float_of_int (width - (tileSize * 2) - pixels - 3))
    ~y:(float_of_int (getYForRow 1) +. 10.)
    ~w:(float_of_int pixels) ~h:15.;
  Canvas.set_fill_style ctx "rgb(251,249,55)";
  Canvas.fill_text ctx ~x:(float_of_int (width - (tileSize * 2)))
    ~y:(float_of_int (getYForRow 1) +. 25.) "TIME"

let drawScore ctx world =
  let scoreText = padWithZeros (string_of_int world.score) 5 in
  let highscoreText = padWithZeros (string_of_int world.highscore) 5 in
  Canvas.set_fill_style ctx "rgb(222,222,246)";
  Canvas.fill_text ctx ~x:(float_of_int (tileSize * 2))
    ~y:(float_of_int (getYForRow 16 + halfTileSize + 3)) "1-UP";
  Canvas.fill_text ctx ~x:(float_of_int (tileSize * 5))
    ~y:(float_of_int (getYForRow 16 + halfTileSize + 3)) "HI-SCORE";
  Canvas.set_fill_style ctx "rgb(252,13,27)";
  Canvas.fill_text ctx ~x:(float_of_int (tileSize + halfTileSize))
    ~y:(float_of_int (getYForRow 15) +. 10.) scoreText;
  Canvas.fill_text ctx ~x:(float_of_int ((tileSize * 5) + 10))
    ~y:(float_of_int (getYForRow 15) +. 10.) highscoreText

let drawCompletedEndzones ctx world =
  List.iter
    (fun (i, rect) ->
      if List.assoc i world.endzone
      then
        Canvas.draw_image ctx goalSprite ~source_x:0. ~source_y:0.
          ~source_w:34. ~source_h:40. ~dx:rect.x
          ~dy:(float_of_int (tileSize * 2)) ~d_w:28. ~d_h:32.)
    endzone_rects

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
      Canvas.set_stroke_style ctx color;
      Canvas.stroke_rect ctx ~x:rect.x ~y:rect.y
        ~w:(float_of_int rect.width) ~h:(float_of_int rect.height))
    world.objects;
  let rect = world.frog.rect in
  Canvas.set_stroke_style ctx !frogBoxColor;
  Canvas.stroke_rect ctx ~x:rect.x ~y:rect.y ~w:(float_of_int rect.width)
    ~h:(float_of_int rect.height)

let drawGrid ctx =
  Canvas.set_stroke_style ctx "red";
  List.iter
    (fun i ->
      Canvas.begin_path ctx;
      Canvas.move_to ctx ~x:(float_of_int (i * tileSize)) ~y:0.;
      Canvas.line_to ctx ~x:(float_of_int (i * tileSize)) ~y:(float_of_int height);
      Canvas.stroke ctx)
    (0 <-> cols);
  List.iter
    (fun i ->
      Canvas.begin_path ctx;
      Canvas.move_to ctx ~x:0. ~y:(float_of_int (i * tileSize));
      Canvas.line_to ctx ~x:(float_of_int width)
        ~y:(float_of_int (i * tileSize));
      Canvas.stroke ctx)
    (0 <-> rows)

let drawBackground ctx =
  Canvas.set_fill_style ctx "rgb(1,4,69)";
  Canvas.fill_rect ctx ~x:0. ~y:0. ~h:(float_of_int height)
    ~w:(float_of_int width);
  Canvas.set_fill_style ctx "black";
  Canvas.fill_rect ctx ~x:0. ~y:(float_of_int (getYForRow 7))
    ~h:(float_of_int height) ~w:(float_of_int width)

let render ctx (world : worldT) =
  drawBackground ctx;
  if world.input.grid then drawGrid ctx;
  if world.input.bbox then drawBoundingBoxes ctx world;
  drawGoal ctx;
  drawGrass ctx (getYForRow 2);
  drawGrass ctx (getYForRow 8);
  drawLives ctx world;
  drawTimer ctx world;
  drawScore ctx world;
  drawCars ctx world.objects;
  drawFrog ctx world.frog;
  drawCompletedEndzones ctx world
