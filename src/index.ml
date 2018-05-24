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
(* let keyup (evt:Dom.event) =
   match (toUnsafe evt)##keyCode with
   | 38 | 32 | 87 -> pressedKeys.up <- false
   | 39 | 68 -> pressedKeys.right <- false
   | 37 | 65 -> pressedKeys.left <- false
   | 40 | 83 -> pressedKeys.down <- false
   | _ -> ()
   ;; *)

(Window.addEventListener "keydown" keydown window );;
(* (Window.addEventListener "keyup" keyup window );; *)

let img = HtmlImageElement.make ();;
(HtmlImageElement.src img spritesUrl);;

type spriteImageT = { xStart: int; yStart: int; frames: int; frameSpeed: float};;
type spriteT = {
  x: int;
  y: int;
  width: int;
  height: int;
  (* velocity: float; *)
  frameIndex: float;
  currentSprite: spriteImageT;
  leftSprite: spriteImageT;
  rightSprite: spriteImageT;
  upSprite: spriteImageT;
  downSprite: spriteImageT
}
type worldT = { 
  frog: spriteT;
  width: int;
  height: int;
  keys: keys;
};;

let rowheight = 30;;
let colwidth = 42;;

let makeSprite xStart yStart frames frameSpeed = { 
  xStart; yStart; frames; frameSpeed;
};;

let worldHeight = 440;;
let worldWidth = 400;;

let startWorld : worldT = { 
  frog =  { 
    x = worldWidth / 2 - 25; y = 387; 
    currentSprite = makeSprite 0 370 2 20.;
    upSprite= makeSprite 0 370 2 20.;
    downSprite= makeSprite 70 370 2 20.;
    leftSprite= makeSprite 70 335 2 20.;
    rightSprite= makeSprite 0 335 2 20.;
    frameIndex = 0.;
    width = 35; height = 30; 
  };
  width = worldWidth;
  height = worldHeight;
  keys = pressedKeys;
};;

let magnification = 1;; (* visual scaling multiplier *)

let drawSprite ctx sprite = 
  let unsafeCtx = (toUnsafe ctx) in
  let frameCalc = (int_of_float (sprite.frameIndex /. 1000.)) in
  let frogFrame = if frameCalc >= sprite.currentSprite.frames then 0 else frameCalc in
  let startX = (float_of_int sprite.currentSprite.xStart) +. (float_of_int frogFrame) *. (float_of_int sprite.width) in 
  ignore @@ unsafeCtx##drawImage img startX sprite.currentSprite.yStart sprite.width sprite.height sprite.x sprite.y (magnification * sprite.width) (magnification * sprite.height)

let drawGoal ctx = 
  let unsafeCtx = (toUnsafe ctx) in
  ignore @@ unsafeCtx##drawImage img 0 62 worldWidth 45 0 0 (magnification * worldWidth) (magnification * 50);;

let drawGrass ctx y = 
  let unsafeCtx = (toUnsafe ctx) in
  ignore @@ unsafeCtx##drawImage img 0 120 worldWidth 33 0 y (magnification * worldWidth) (magnification * 33);;

let render ctx world = 
  Canvas2dRe.setFillStyle ctx String "black";
  Canvas2dRe.fillRect ctx ~x:0. ~y:0. ~h: (float_of_int world.height) ~w:(float_of_int world.width);
  (drawGoal ctx);
  (drawGrass ctx (world.height - 60));
  (drawGrass ctx (world.height - 60 - (rowheight * 6)));
  (drawSprite ctx world.frog);
;;

let lastTime = ref (Js.Date.now ());;

let rec update ctx world = 
  let now = Js.Date.now () in
  let dt = now -. !lastTime in
  (render ctx world);

  lastTime := Js.Date.now ();
  let frog = { world.frog with 
               x = world.frog.x + (colwidth ) * if pressedKeys.left then -1 else if pressedKeys.right then 1 else 0;
               y = world.frog.y + (rowheight) * if pressedKeys.up then -1 else if pressedKeys.down then 1 else 0;
               currentSprite = if pressedKeys.up then world.frog.upSprite  
                 else if pressedKeys.down then world.frog.downSprite
                 else if pressedKeys.left then world.frog.leftSprite 
                 else if pressedKeys.right then world.frog.rightSprite 
                 else world.frog.currentSprite;
               frameIndex = if (pressedKeys.down || pressedKeys.up || pressedKeys.left || pressedKeys.right) then 0.
                 else (world.frog.frameIndex +. dt *.world.frog.currentSprite.frameSpeed);
             } in 

  (* we want to reset directional pressedKeys after we process it once since frogger doesn't continously move, he jumps  *)
  pressedKeys.left <- false;
  pressedKeys.right <- false;
  pressedKeys.up <- false;
  pressedKeys.down <- false;
  let newWorld = {world with frog } in
  Js.log world;
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
