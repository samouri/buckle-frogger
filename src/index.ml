open Webapi.Dom;;
open State;;
open Render;;
open Types

let lastTime = ref (int_of_float (Js.Date.now ()));; 
let rec update ctx (world:worldT) = 
  let now = int_of_float (Js.Date.now ()) in
  let dt = now - !lastTime in
  let nextWorld = ref world in
  if world.state = Playing then (
    nextWorld := stepWorld world now dt;
    render ctx world;
  ) else if world.state = Start then (
    nextWorld := if pressedKeys.direction = None then world else {startWorld with state = Playing };
    drawStartScreen ctx;
  ) else if world.state = Won then (
    nextWorld := if pressedKeys.direction = None then world else {startWorld with state = Playing };
    drawWinScreen ctx;
  ) else (
    nextWorld := if pressedKeys.direction = None then world else {startWorld with state = Playing };
    drawLoseScreen ctx;
  );

  lastTime := int_of_float (Js.Date.now ());
  pressedKeys.direction <- None; (* remove the press once processed *)
  (Webapi.requestAnimationFrame (fun _ -> (update ctx !nextWorld )))
;;


let load _ =
  let canvas_id = "canvas" in
  let canvas =
    match document |> (Document.getElementById "canvas") with
    | None  ->
      (print_endline ("cant find canvas " ^ (canvas_id ^ " \n"));
       failwith "fail")
    | Some el -> el in
  canvas |> (Element.setAttribute "height" ((string_of_int height) ^ "px"));
  canvas |> (Element.setAttribute "width" ((string_of_int width) ^ "px"));
  canvas |> (Element.setAttribute "style" ("max-width: " ^ (string_of_int width)  ^ "px; max-height: " ^ (string_of_int height) ^ "px"));
  let context = CanvasRe.CanvasElement.getContext2d canvas in
  (update context startWorld);
;; 

let _ = Window.setOnLoad window load;;
