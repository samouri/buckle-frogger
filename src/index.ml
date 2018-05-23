open Webapi.Dom
external canvasRenderingContext2DToJsObj : Canvas2dRe.t -> < .. > Js.t = "%identity"


external spritesUrl: string = "../assets/frogger_sprites.png" [@@bs.module];;

let img = HtmlImageElement.make ();;
(HtmlImageElement.src img spritesUrl);;

type spriteT = {x : int; y: int; width: int; height: int; frames: int; frameIndex: float; frameSpeed: float};;
type worldT = { 
  frog: spriteT;
  width: int;
  height: int;
};;
let startWorld : worldT = { 
  frog =  { 
    x = 0; y = 0; 
    width = 35; height = 30; 
    frames = 2; frameIndex= 0.; 
    frameSpeed= 4.; 
  };
  width= 760;
  height= 600;
};;

let drawSprite ctx sprite = 
  let unsafeCtx = (canvasRenderingContext2DToJsObj ctx) in
  let frogFrame = (int_of_float (sprite.frameIndex /. 1000.)) mod sprite.frames in
  let startX = (float_of_int frogFrame) *. (float_of_int sprite.width) in 
  ignore @@ unsafeCtx##drawImage img startX 370 sprite.width sprite.height 0 300 sprite.width sprite.height

let render ctx world = 
  Canvas2dRe.setFillStyle ctx String "black";
  Canvas2dRe.fillRect ctx ~x:0. ~y:0. ~h: (float_of_int world.height) ~w:(float_of_int world.width);
  (drawSprite ctx world.frog)
;;

let lastTime = ref (Js.Date.now ());;

let rec update ctx world = 
  let now = Js.Date.now () in
  let dt = now -. !lastTime in
  (render ctx world);

  lastTime := Js.Date.now ();
  let newWorld = {world with frog = { world.frog with frameIndex = (world.frog.frameIndex +. dt *.world.frog.frameSpeed) }; } in
  (Webapi.requestAnimationFrame (fun dt -> (update ctx newWorld )))
;;

let load _ =
  let canvas_id = "canvas" in
  let canvas =
    match document |> (Document.getElementById "canvas") with
    | None  ->
      (print_endline ("cant find canvas " ^ (canvas_id ^ " \n"));
       failwith "fail")
    | Some el -> el in
  canvas |> (Element.setAttribute "height" ((string_of_int startWorld.height) ^ "px"));
  canvas |> (Element.setAttribute "width" ((string_of_int startWorld.width) ^ "px"));
  let context = CanvasRe.CanvasElement.getContext2d canvas in
  (update context startWorld);
;;

let _ = Window.setOnLoad window load
