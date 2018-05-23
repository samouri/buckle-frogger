open Webapi.Dom
external toUnsafe : 'a -> < .. > Js.t = "%identity"
external spritesUrl: string = "../assets/frogger_sprites.png" [@@bs.module];;


(* Represents the values of relevant key bindings. *)
type keys = {
  mutable left: bool;
  mutable right: bool;
  mutable up: bool;
  mutable down: bool;
  mutable bbox: int;
}

(*pressed_keys instantiates the keys.*)
let pressedKeys = {
  left = false;
  right = false;
  up = false;
  down = false;
  bbox = 0;
};;

(* Keydown event handler translates a key press *)
let keydown (evt:Dom.event) =
  match (toUnsafe evt)##keyCode with
  | 38 | 32 | 87 -> pressedKeys.up <- true
  | 39 | 68 -> pressedKeys.right <- true
  | 37 | 65 -> pressedKeys.left <- true
  | 40 | 83 -> pressedKeys.down <- true
  | 66 -> pressedKeys.bbox <- (pressedKeys.bbox + 1) mod 2
  | _ -> Js.log ("did not find nothing" ^ ((toUnsafe evt)##keyCode))
;;


(* Keyup event handler translates a key release *)
let keyup (evt:Dom.event) =
  match (toUnsafe evt)##keyCode with
  | 38 | 32 | 87 -> pressedKeys.up <- false
  | 39 | 68 -> pressedKeys.right <- false
  | 37 | 65 -> pressedKeys.left <- false
  | 40 | 83 -> pressedKeys.down <- false
  | _ -> ()
;;

(Window.addEventListener "keydown" keydown window );;
(Window.addEventListener "keyup" keyup window );;

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
  let unsafeCtx = (toUnsafe ctx) in
  let frogFrame = (int_of_float (sprite.frameIndex /. 1000.)) mod sprite.frames in
  let startX = (float_of_int frogFrame) *. (float_of_int sprite.width) in 
  ignore @@ unsafeCtx##drawImage img startX 370 sprite.width sprite.height sprite.x sprite.y sprite.width sprite.height

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
  let vel = 500. in
  let frog = { world.frog with 
               x = world.frog.x + (int_of_float ( vel *. dt /. 1000.)) * if pressedKeys.left then -1 else if pressedKeys.right then 1 else 0;
               y = world.frog.y + (int_of_float ( vel *. dt /. 1000.)) * if pressedKeys.up then -1 else if pressedKeys.down then 1 else 0;
               frameIndex = (world.frog.frameIndex +. dt *.world.frog.frameSpeed);
             } in 
  let newWorld = {world with frog  } in
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
