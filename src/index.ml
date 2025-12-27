open Bindings
open State
open Render
open Types
open Utils

let lastTime : float option ref = ref None
let frameCount = ref 0

let log_state world dt =
  if !frameCount mod 60 = 0
  then
    Console.log
      (Printf.sprintf "frame=%d state=%s dt=%d objects=%d"
         !frameCount
         (match world.state with
          | Start -> "Start"
          | Playing -> "Playing"
          | Won -> "Won"
          | Lost -> "Lost")
         dt
         (List.length world.objects))

let rec gameloop canvas timestamp (world : worldT) =
  incr frameCount;
  let dt =
    match !lastTime with
    | None -> 0.
    | Some last -> timestamp -. last
  in
  lastTime := Some timestamp;
  let now = int_of_float timestamp in
  let dt_int = int_of_float dt in
  log_state world dt_int;
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
  Console.log "load: starting";
  match Canvas.create "canvas" with
  | None ->
    Console.log "load: can't find canvas element"
  | Some canvas ->
    Console.log "load: canvas found";
    Canvas.set_dimensions canvas ~width ~height;
    Canvas.set_max_size canvas ~width ~height;
    Console.log "load: starting gameloop";
    Window.request_animation_frame (fun ts -> gameloop canvas ts startWorld)

let () = Window.on_load load
