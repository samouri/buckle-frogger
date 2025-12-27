open Js_of_ocaml
open State
open Render
open Types
open Utils

let lastTime = ref (int_of_float (now_ms ()))

let rec gameloop ctx (world : worldT) =
  let now = int_of_float (now_ms ()) in
  let dt = now - !lastTime in
  (match world.state with
   | Playing -> render ctx world
   | Start -> drawStartScreen ctx
   | Won -> drawWinScreen ctx
   | Lost -> drawLoseScreen ctx);

  let nextWorld =
    match (world.state, input.direction) with
    | Playing, _ -> stepWorld world now dt
    | _, None -> world
    | _, Some _ -> { startWorld with state = Playing; highscore = world.highscore }
  in

  lastTime := int_of_float (now_ms ());
  input.direction <- None;
  ignore
    (Dom_html.window##requestAnimationFrame
       (Js.wrap_callback (fun _ -> gameloop ctx nextWorld)))

let load _ =
  let canvas =
    match Dom_html.getElementById_coerce "canvas" Dom_html.CoerceTo.canvas with
    | None ->
      Firebug.console##log (Js.string "can't find canvas element");
      None
    | Some canvas -> Some canvas
  in
  match canvas with
  | None -> Js._false
  | Some canvas ->
    canvas##.height := height;
    canvas##.width := width;
    canvas##.style##.maxWidth := Js.string (Printf.sprintf "%dpx" width);
    canvas##.style##.maxHeight := Js.string (Printf.sprintf "%dpx" height);
    let context = canvas##getContext Dom_html._2d_ in
    gameloop context startWorld;
    Js._false

let () = Dom_html.window##.onload := Dom_html.handler load
