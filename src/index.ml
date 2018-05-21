open Webapi.Dom
type dimensions = {
  height: int;
  width: int;}
let canvasDimensions: dimensions = { height = 400; width = 760 }
let load _ =
  let canvas_id = "canvas" in
  let canvas =
    match document |> (Document.getElementById "canvas") with
    | None  ->
        (print_endline ("cant find canvas " ^ (canvas_id ^ " \n"));
         failwith "fail")
    | Some el -> el in
  let context = CanvasRe.CanvasElement.getContext2d canvas in
  Canvas2dRe.setFillStyle context String "red";
  Canvas2dRe.fillRect context ~x:0. ~y:0. ~h:100. ~w:100.
let _ = Window.setOnLoad window load