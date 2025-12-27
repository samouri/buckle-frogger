open Bindings
open State
open Render
open Types
open Utils

let lastTime : float option ref = ref None

let rec gameloop canvas timestamp (world : worldT) =
  let dt =
    match !lastTime with
    | None -> 0.
    | Some last -> timestamp -. last
  in
  lastTime := Some timestamp;
  let now = int_of_float timestamp in
  let dt_int = int_of_float dt in
  (match world.state with
   | Playing -> render canvas world
   | Start -> drawStartScreen canvas
   | Won -> drawWinScreen canvas
   | Lost -> drawLoseScreen canvas);

  let nextWorld =
    match (world.state, input.direction) with
    | Playing, _ -> stepWorld world now dt_int
    | _, None -> world
    | _, Some _ -> { startWorld with state = Playing; highscore = world.highscore }
  in

  input.direction <- None;
  Window.request_animation_frame (fun ts -> gameloop canvas ts nextWorld)

let load () =
  match Canvas.create "canvas" with
  | None -> ()
  | Some canvas ->
    Canvas.set_dimensions canvas ~width ~height;
    Canvas.set_max_size canvas ~width ~height;
    Window.request_animation_frame (fun ts -> gameloop canvas ts startWorld)

let () = Window.on_load load
