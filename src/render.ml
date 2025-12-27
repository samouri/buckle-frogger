open Bindings
open Types
open Utils
open Game

let magnification = 1. (* visual scaling multiplier *)

let draw_lane_object ctx sprite =
  let frame_calc = floor (Lane_object.(sprite.frame_index) /. 1000.) in
  let img = Lane_object.(sprite.img) in
  let start_x =
    float_of_int Sprite_image.(img.x_start) +. (frame_calc *. float_of_int img.width)
  in
  List.iter
    (fun i ->
      Canvas.draw_image
        ctx
        sprite_sheet
        ~source_x:start_x
        ~source_y:(float_of_int Sprite_image.(img.y_start))
        ~source_w:(float_of_int Sprite_image.(img.width))
        ~source_h:(float_of_int Sprite_image.(img.height))
        ~dx:
          ((Rect.(Lane_object.(sprite.rect).x) +. float_of_int (img.width * i))
           *. magnification)
        ~dy:Rect.(Lane_object.(sprite.rect).y)
        ~d_w:(magnification *. float_of_int img.width)
        ~d_h:(magnification *. float_of_int tileSize))
    (0 <-> img.number - 1)
;;

let draw_dying_frog ctx rect left_in_animation =
  let width = 32 in
  let height = 36 in
  let row = get_row_for_y (int_of_float Rect.(rect.y)) in
  let y = if row > 7 && row < 13 then 227 else 276 in
  let frame_xs = [ 230; 275; 322; 358 ] in
  let frames_length = 4 in
  let frame_duration = float_of_int frog_animation_length /. 4. in
  let frame =
    min
      (frames_length - 1)
      (frames_length
       - int_of_float (ceil (float_of_int left_in_animation /. frame_duration)))
  in
  let start_x = List.nth frame_xs frame in
  Canvas.draw_image
    ctx
    sprite_sheet
    ~source_x:(float_of_int start_x)
    ~source_y:(float_of_int y)
    ~source_w:(float_of_int width)
    ~source_h:(float_of_int height)
    ~dx:((Rect.(rect.x) -. 10.) *. magnification)
    ~dy:(Rect.(rect.y) -. 8.)
    ~d_w:(magnification *. float_of_int width)
    ~d_h:(magnification *. float_of_int height)
;;

let draw_frog ctx frog =
  match Frog.(frog.left_in_animation) with
  | Some n -> draw_dying_frog ctx Frog.(frog.rect) n
  | None ->
    let img =
      match Frog.(frog.direction) with
      | Direction.Up -> frog_up
      | Direction.Down -> frog_down
      | Direction.Left -> frog_left
      | Direction.Right -> frog_right
    in
    let start_x =
      float_of_int
        (Sprite_image.(img.x_start)
         + if Frog.(frog.left_in_jump) = 0. then 0 else img.width + 5)
    in
    Canvas.draw_image
      ctx
      sprite_sheet
      ~source_x:start_x
      ~source_y:(float_of_int Sprite_image.(img.y_start))
      ~source_w:(float_of_int Sprite_image.(img.width))
      ~source_h:(float_of_int Sprite_image.(img.height))
      ~dx:((Rect.(Frog.(frog.rect).x) -. 10.) *. magnification)
      ~dy:Rect.(Frog.(frog.rect).y)
      ~d_w:(magnification *. float_of_int img.width)
      ~d_h:(magnification *. float_of_int img.height)
;;

let draw_start_screen ctx =
  Canvas.set_fill_style ctx "white";
  Canvas.fill_rect ctx ~x:0. ~y:0. ~h:(float_of_int height) ~w:(float_of_int width);
  Canvas.set_fill_style ctx "black";
  Canvas.set_font ctx "60px/1 sans-serif";
  Canvas.fill_text ctx ~x:80. ~y:200. "Frogger";
  Canvas.set_font ctx "20px/1 sans-serif";
  Canvas.fill_text ctx ~x:80. ~y:280. "Press any key to start the game"
;;

let draw_win_screen ctx =
  Canvas.set_fill_style ctx "white";
  Canvas.fill_rect ctx ~x:0. ~y:0. ~h:(float_of_int height) ~w:(float_of_int width);
  Canvas.set_fill_style ctx "black";
  Canvas.set_font ctx "60px/1 sans-serif";
  Canvas.fill_text ctx ~x:80. ~y:150. "You Win!";
  Canvas.set_font ctx "20px/1 sans-serif";
  Canvas.fill_text ctx ~x:50. ~y:280. "Press any key to start another the game"
;;

let draw_lose_screen ctx =
  Canvas.set_fill_style ctx "white";
  Canvas.fill_rect ctx ~x:0. ~y:0. ~h:(float_of_int height) ~w:(float_of_int width);
  Canvas.set_fill_style ctx "black";
  Canvas.set_font ctx "60px/1 sans-serif";
  Canvas.fill_text ctx ~x:80. ~y:200. "You Lose";
  Canvas.set_font ctx "20px/1 sans-serif";
  Canvas.fill_text ctx ~x:50. ~y:280. "Press any key to start another game"
;;

let draw_goal ctx =
  let y = get_y_for_row 15 in
  Canvas.draw_image
    ctx
    sprite_sheet
    ~source_x:0.
    ~source_y:62.
    ~source_w:398.
    ~source_h:45.
    ~dx:0.
    ~dy:(float_of_int (y + halfTileSize))
    ~d_w:(magnification *. float_of_int width)
    ~d_h:(magnification *. float_of_int (tileSize + halfTileSize))
;;

let draw_grass ctx y =
  Canvas.draw_image
    ctx
    sprite_sheet
    ~source_x:0.
    ~source_y:120.
    ~source_w:398.
    ~source_h:33.
    ~dx:0.
    ~dy:(float_of_int y)
    ~d_w:(magnification *. float_of_int width)
    ~d_h:(magnification *. float_of_int tileSize)
;;

let rec draw_cars ctx cars =
  match cars with
  | [] -> ()
  | hd :: tl ->
    draw_lane_object ctx hd;
    draw_cars ctx tl
;;

let draw_lives ctx world =
  Canvas.set_fill_style ctx "red";
  List.iter
    (fun i ->
      Canvas.draw_image
        ctx
        life_sprite
        ~source_x:0.
        ~source_y:0.
        ~source_w:34.
        ~source_h:40.
        ~dx:(float_of_int (10 + (20 * i)))
        ~dy:(float_of_int (get_y_for_row 1))
        ~d_w:28.
        ~d_h:32.)
    (0 <-> World.(world.lives) - 2)
;;

let draw_timer ctx world =
  Canvas.set_fill_style ctx "rgb(49,220,39)";
  let pixels =
    int_of_float
      (float_of_int World.(world.timer)
       /. float_of_int start_timer_ms
       *. (float_of_int width /. 2.5))
  in
  Canvas.fill_rect
    ctx
    ~x:(float_of_int (width - (tileSize * 2) - pixels - 3))
    ~y:(float_of_int (get_y_for_row 1) +. 10.)
    ~w:(float_of_int pixels)
    ~h:15.;
  Canvas.set_fill_style ctx "rgb(251,249,55)";
  Canvas.fill_text
    ctx
    ~x:(float_of_int (width - (tileSize * 2)))
    ~y:(float_of_int (get_y_for_row 1) +. 25.)
    "TIME"
;;

let draw_score ctx world =
  let scoreText = padWithZeros (string_of_int World.(world.score)) 5 in
  let highscoreText = padWithZeros (string_of_int World.(world.highscore)) 5 in
  Canvas.set_fill_style ctx "rgb(222,222,246)";
  Canvas.fill_text
    ctx
    ~x:(float_of_int (tileSize * 2))
    ~y:(float_of_int (get_y_for_row 16 + halfTileSize + 3))
    "1-UP";
  Canvas.fill_text
    ctx
    ~x:(float_of_int (tileSize * 5))
    ~y:(float_of_int (get_y_for_row 16 + halfTileSize + 3))
    "HI-SCORE";
  Canvas.set_fill_style ctx "rgb(252,13,27)";
  Canvas.fill_text
    ctx
    ~x:(float_of_int (tileSize + halfTileSize))
    ~y:(float_of_int (get_y_for_row 15) +. 10.)
    scoreText;
  Canvas.fill_text
    ctx
    ~x:(float_of_int ((tileSize * 5) + 10))
    ~y:(float_of_int (get_y_for_row 15) +. 10.)
    highscoreText
;;

let draw_completed_endzones ctx world =
  List.iter
    (fun (i, rect) ->
      if List.assoc i World.(world.endzone)
      then
        Canvas.draw_image
          ctx
          goal_sprite
          ~source_x:0.
          ~source_y:0.
          ~source_w:34.
          ~source_h:40.
          ~dx:Rect.(rect.x)
          ~dy:(float_of_int (tileSize * 2))
          ~d_w:28.
          ~d_h:32.)
    endzone_rects
;;

let draw_bounding_boxes ctx world =
  let frogBoxColor = ref "red" in
  List.iter
    (fun obj ->
      let rect = Lane_object.(obj.rect) in
      let color =
        if intersects World.(world.frog.rect) rect
        then (
          frogBoxColor := "blue";
          "blue")
        else "red"
      in
      Canvas.set_stroke_style ctx color;
      Canvas.stroke_rect
        ctx
        ~x:Rect.(rect.x)
        ~y:Rect.(rect.y)
        ~w:(float_of_int Rect.(rect.width))
        ~h:(float_of_int Rect.(rect.height)))
    World.(world.objects);
  let rect = World.(world.frog.rect) in
  Canvas.set_stroke_style ctx !frogBoxColor;
  Canvas.stroke_rect
    ctx
    ~x:Rect.(rect.x)
    ~y:Rect.(rect.y)
    ~w:(float_of_int Rect.(rect.width))
    ~h:(float_of_int Rect.(rect.height))
;;

let draw_grid ctx =
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
      Canvas.line_to ctx ~x:(float_of_int width) ~y:(float_of_int (i * tileSize));
      Canvas.stroke ctx)
    (0 <-> rows)
;;

let draw_background ctx =
  Canvas.set_fill_style ctx "rgb(1,4,69)";
  Canvas.fill_rect ctx ~x:0. ~y:0. ~h:(float_of_int height) ~w:(float_of_int width);
  Canvas.set_fill_style ctx "black";
  Canvas.fill_rect
    ctx
    ~x:0.
    ~y:(float_of_int (get_y_for_row 7))
    ~h:(float_of_int height)
    ~w:(float_of_int width)
;;

let render ctx world =
  draw_background ctx;
  if World.(world.input.grid) then draw_grid ctx;
  if World.(world.input.bbox) then draw_bounding_boxes ctx world;
  draw_goal ctx;
  draw_grass ctx (get_y_for_row 2);
  draw_grass ctx (get_y_for_row 8);
  draw_lives ctx world;
  draw_timer ctx world;
  draw_score ctx world;
  draw_cars ctx World.(world.objects);
  draw_frog ctx World.(world.frog);
  draw_completed_endzones ctx world
;;
