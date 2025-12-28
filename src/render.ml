open Bindings
open Types
open Utils
open Game

let magnification = 1. (* visual scaling multiplier *)

let draw_lane_object (canvas : Canvas.t) (sprite : Lane_object.t) =
  let frame_calc = floor (sprite.frame_index /. 1000.) in
  let img = sprite.img in
  let start_x = float_of_int img.x_start +. (frame_calc *. float_of_int img.width) in
  List.iter
    (fun i ->
      Canvas.draw_image
        canvas
        sprite_sheet
        ~source_x:start_x
        ~source_y:(float_of_int img.y_start)
        ~source_w:(float_of_int img.width)
        ~source_h:(float_of_int img.height)
        ~dx:((sprite.rect.x +. float_of_int (img.width * i)) *. magnification)
        ~dy:sprite.rect.y
        ~d_w:(magnification *. float_of_int img.width)
        ~d_h:(magnification *. float_of_int tile_size))
    (0 <-> img.number - 1)
;;

let draw_dying_frog canvas rect left_in_animation =
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
    canvas
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

let draw_frog canvas (frog : Frog.t) =
  match frog.left_in_animation with
  | Some n -> draw_dying_frog canvas frog.rect n
  | None ->
    let img =
      match frog.direction with
      | Up -> frog_up
      | Down -> frog_down
      | Left -> frog_left
      | Right -> frog_right
    in
    let start_x =
      float_of_int (img.x_start + if frog.left_in_jump = 0. then 0 else img.width + 5)
    in
    Canvas.draw_image
      canvas
      sprite_sheet
      ~source_x:start_x
      ~source_y:(float_of_int img.y_start)
      ~source_w:(float_of_int img.width)
      ~source_h:(float_of_int img.height)
      ~dx:((Rect.(Frog.(frog.rect).x) -. 10.) *. magnification)
      ~dy:Rect.(Frog.(frog.rect).y)
      ~d_w:(magnification *. float_of_int img.width)
      ~d_h:(magnification *. float_of_int img.height)
;;

let draw_start_screen canvas =
  Canvas.set_fill_style canvas "white";
  Canvas.fill_rect canvas ~x:0. ~y:0. ~h:(float_of_int height) ~w:(float_of_int width);
  Canvas.set_fill_style canvas "black";
  Canvas.set_font canvas "60px/1 sans-serif";
  Canvas.fill_text canvas ~x:80. ~y:200. "Frogger";
  Canvas.set_font canvas "20px/1 sans-serif";
  Canvas.fill_text canvas ~x:80. ~y:280. "Press any key to start the game"
;;

let draw_win_screen canvas =
  Canvas.set_fill_style canvas "white";
  Canvas.fill_rect canvas ~x:0. ~y:0. ~h:(float_of_int height) ~w:(float_of_int width);
  Canvas.set_fill_style canvas "black";
  Canvas.set_font canvas "60px/1 sans-serif";
  Canvas.fill_text canvas ~x:80. ~y:150. "You Win!";
  Canvas.set_font canvas "20px/1 sans-serif";
  Canvas.fill_text canvas ~x:50. ~y:280. "Press any key to start another the game"
;;

let draw_lose_screen canvas =
  Canvas.set_fill_style canvas "white";
  Canvas.fill_rect canvas ~x:0. ~y:0. ~h:(float_of_int height) ~w:(float_of_int width);
  Canvas.set_fill_style canvas "black";
  Canvas.set_font canvas "60px/1 sans-serif";
  Canvas.fill_text canvas ~x:80. ~y:200. "You Lose";
  Canvas.set_font canvas "20px/1 sans-serif";
  Canvas.fill_text canvas ~x:50. ~y:280. "Press any key to start another game"
;;

let draw_goal canvas =
  let y = get_y_for_row 15 in
  Canvas.draw_image
    canvas
    sprite_sheet
    ~source_x:0.
    ~source_y:62.
    ~source_w:398.
    ~source_h:45.
    ~dx:0.
    ~dy:(float_of_int (y + half_tile_size))
    ~d_w:(magnification *. float_of_int width)
    ~d_h:(magnification *. float_of_int (tile_size + half_tile_size))
;;

let draw_grass canvas y =
  Canvas.draw_image
    canvas
    sprite_sheet
    ~source_x:0.
    ~source_y:120.
    ~source_w:398.
    ~source_h:33.
    ~dx:0.
    ~dy:(float_of_int y)
    ~d_w:(magnification *. float_of_int width)
    ~d_h:(magnification *. float_of_int tile_size)
;;

let rec draw_cars canvas cars =
  match cars with
  | [] -> ()
  | hd :: tl ->
    draw_lane_object canvas hd;
    draw_cars canvas tl
;;

let draw_lives canvas (game : Game.t) =
  Canvas.set_fill_style canvas "red";
  List.iter
    (fun i ->
      Canvas.draw_image
        canvas
        life_sprite
        ~source_x:0.
        ~source_y:0.
        ~source_w:34.
        ~source_h:40.
        ~dx:(float_of_int (10 + (20 * i)))
        ~dy:(float_of_int (get_y_for_row 1))
        ~d_w:28.
        ~d_h:32.)
    (0 <-> game.lives - 2)
;;

let draw_timer canvas (game : Game.t) =
  Canvas.set_fill_style canvas "rgb(49,220,39)";
  let pixels =
    int_of_float
      (float_of_int game.timer
       /. float_of_int start_timer_ms
       *. (float_of_int width /. 2.5))
  in
  Canvas.fill_rect
    canvas
    ~x:(float_of_int (width - (tile_size * 2) - pixels - 3))
    ~y:(float_of_int (get_y_for_row 1) +. 10.)
    ~w:(float_of_int pixels)
    ~h:15.;
  Canvas.set_fill_style canvas "rgb(251,249,55)";
  Canvas.fill_text
    canvas
    ~x:(float_of_int (width - (tile_size * 2)))
    ~y:(float_of_int (get_y_for_row 1) +. 25.)
    "TIME"
;;

let draw_score canvas (game : Game.t) =
  let score_text = pad_with_zeros (string_of_int game.score) 5 in
  let highscore_text = pad_with_zeros (string_of_int game.highscore) 5 in
  Canvas.set_fill_style canvas "rgb(222,222,246)";
  Canvas.fill_text
    canvas
    ~x:(float_of_int (tile_size * 2))
    ~y:(float_of_int (get_y_for_row 16 + half_tile_size + 3))
    "1-UP";
  Canvas.fill_text
    canvas
    ~x:(float_of_int (tile_size * 5))
    ~y:(float_of_int (get_y_for_row 16 + half_tile_size + 3))
    "HI-SCORE";
  Canvas.set_fill_style canvas "rgb(252,13,27)";
  Canvas.fill_text
    canvas
    ~x:(float_of_int (tile_size + half_tile_size))
    ~y:(float_of_int (get_y_for_row 15) +. 10.)
    score_text;
  Canvas.fill_text
    canvas
    ~x:(float_of_int ((tile_size * 5) + 10))
    ~y:(float_of_int (get_y_for_row 15) +. 10.)
    highscore_text
;;

let draw_completed_endzones canvas (game : Game.t) =
  List.iter
    (fun (i, rect) ->
      if List.assoc i game.endzone
      then
        Canvas.draw_image
          canvas
          goal_sprite
          ~source_x:0.
          ~source_y:0.
          ~source_w:34.
          ~source_h:40.
          ~dx:Rect.(rect.x)
          ~dy:(float_of_int (tile_size * 2))
          ~d_w:28.
          ~d_h:32.)
    endzone_rects
;;

let draw_bounding_boxes (canvas : Canvas.t) (game : Game.t) =
  let frog_box_color = ref "red" in
  List.iter
    (fun (obj : Lane_object.t) ->
      let rect = obj.rect in
      let color =
        if intersects game.frog.rect rect
        then (
          frog_box_color := "blue";
          "blue")
        else "red"
      in
      Canvas.set_stroke_style canvas color;
      Canvas.stroke_rect
        canvas
        ~x:rect.x
        ~y:rect.y
        ~w:(float_of_int rect.width)
        ~h:(float_of_int rect.height))
    game.objects;
  let rect = game.frog.rect in
  Canvas.set_stroke_style canvas !frog_box_color;
  Canvas.stroke_rect
    canvas
    ~x:rect.x
    ~y:rect.y
    ~w:(float_of_int rect.width)
    ~h:(float_of_int rect.height)
;;

let draw_grid canvas =
  Canvas.set_stroke_style canvas "red";
  List.iter
    (fun i ->
      Canvas.begin_path canvas;
      Canvas.move_to canvas ~x:(float_of_int (i * tile_size)) ~y:0.;
      Canvas.line_to canvas ~x:(float_of_int (i * tile_size)) ~y:(float_of_int height);
      Canvas.stroke canvas)
    (0 <-> cols);
  List.iter
    (fun i ->
      Canvas.begin_path canvas;
      Canvas.move_to canvas ~x:0. ~y:(float_of_int (i * tile_size));
      Canvas.line_to canvas ~x:(float_of_int width) ~y:(float_of_int (i * tile_size));
      Canvas.stroke canvas)
    (0 <-> rows)
;;

let draw_background canvas =
  Canvas.set_fill_style canvas "rgb(1,4,69)";
  Canvas.fill_rect canvas ~x:0. ~y:0. ~h:(float_of_int height) ~w:(float_of_int width);
  Canvas.set_fill_style canvas "black";
  Canvas.fill_rect
    canvas
    ~x:0.
    ~y:(float_of_int (get_y_for_row 7))
    ~h:(float_of_int height)
    ~w:(float_of_int width)
;;

let render canvas (game : Game.t) =
  draw_background canvas;
  if game.input.grid then draw_grid canvas;
  if game.input.bbox then draw_bounding_boxes canvas game;
  draw_goal canvas;
  draw_grass canvas (get_y_for_row 2);
  draw_grass canvas (get_y_for_row 8);
  draw_lives canvas game;
  draw_timer canvas game;
  draw_score canvas game;
  draw_cars canvas game.objects;
  draw_frog canvas game.frog;
  draw_completed_endzones canvas game
;;
