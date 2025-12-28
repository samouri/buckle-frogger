open Bindings
open Game
open Input
open Render
open Utils

let last_time : float option ref = ref None
let input_ref = ref Input.empty
let touch_start : (int * int) option ref = ref None
let update_direction direction = input_ref := Input.set_direction !input_ref direction

let handle_keycode key_code =
  match key_code with
  | 38 | 32 | 87 -> update_direction Up
  | 39 | 68 -> update_direction Right
  | 37 | 65 -> update_direction Left
  | 40 | 83 -> update_direction Down
  | 66 -> input_ref := Input.toggle_bbox !input_ref
  | 71 -> input_ref := Input.toggle_grid !input_ref
  | _ -> ()
;;

let handle_touch_start (x, y) = touch_start := Some (x, y)

let handle_touch_move (x, y) =
  match !touch_start with
  | None -> ()
  | Some (x_down, y_down) ->
    let x_diff = x_down - x in
    let y_diff = y_down - y in
    if abs x_diff > abs y_diff
    then update_direction (if x_diff > 0 then Left else Right)
    else update_direction (if y_diff > 0 then Up else Down)
;;

let start_playing (world : Game.t) =
  let fresh = Game.init ~highscore:world.highscore in
  { fresh with state = Playing }
;;

let rec gameloop (canvas : Canvas.t) (timestamp : float) (world : Game.t) =
  let dt =
    match !last_time with
    | None -> 0.
    | Some last -> timestamp -. last
  in
  last_time := Some timestamp;
  let now = int_of_float timestamp in
  let dt_ms = int_of_float dt in
  (match world.state with
   | Playing -> render canvas world
   | Start -> draw_start_screen canvas
   | Won -> draw_win_screen canvas
   | Lost -> draw_lose_screen canvas);
  let next_world, events =
    match world.state, !input_ref.direction with
    | Playing, _ -> Game.step world ~input:!input_ref ~now_ms:now ~dt_ms
    | _, None -> world, []
    | _, Some _ -> start_playing world, []
  in
  List.iter
    (function
      | Highscore_updated score -> Storage.set "highscore" (string_of_int score)
      | _ -> ())
    events;
  input_ref := Input.clear_direction !input_ref;
  Window.request_animation_frame (fun ts -> gameloop canvas ts next_world)
;;

let load () =
  match Canvas.create "canvas" with
  | None -> ()
  | Some canvas ->
    Canvas.set_dimensions canvas ~width ~height;
    Canvas.set_max_size canvas ~width ~height;
    Input_events.on_keydown handle_keycode;
    Input_events.on_touch ~start:handle_touch_start ~move:handle_touch_move;
    let saved_highscore =
      match Storage.get "highscore" with
      | Some n -> int_of_string n
      | None -> 0
    in
    let initial = Game.init ~highscore:saved_highscore in
    Window.request_animation_frame (fun ts -> gameloop canvas ts initial)
;;

let () = Window.on_load load
