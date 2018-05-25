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
  x: float;
  y: float;
  width: int;
  height: int;
  frameIndex: float;
  currentSprite: spriteImageT;
}

type carT = {
  row: int;
  (* velocity: float; *)
  sprite: spriteT;
};;

type worldT = { 
  frog: spriteT;
  width: int;
  height: int;
  keys: keys;
  cars: carT list;
};;

let rowheight = 30;;
let colwidth = 42;;

let makeSpriteImage xStart yStart frames frameSpeed = { 
  xStart; yStart; frames; frameSpeed;
};;


let worldHeight = 440;;
let worldWidth = 400;;

let yellowCarImage = makeSpriteImage 80 260 0 0.;;
let greenCarImage = makeSpriteImage 80 260 0 0.;;

let makeCar row img = 
  {
    row;
    sprite = {
      currentSprite = img;
      x = float_of_int worldWidth;
      y = float_of_int (worldHeight - 62 - (row * rowheight));
      frameIndex = 0.;
      width = 33;
      height = 30;
    }
  };;

type frogSpritesT = { upSprite: spriteImageT; downSprite: spriteImageT; leftSprite: spriteImageT; rightSprite: spriteImageT };;
let frogSprites = {
  upSprite= makeSpriteImage 0 370 2 20.;
  downSprite= makeSpriteImage 70 370 2 20.;
  leftSprite= makeSpriteImage 70 335 2 20.;
  rightSprite= makeSpriteImage 0 335 2 20.;
};;

let startWorld : worldT = { 
  frog = { 
    x = float_of_int (worldWidth / 2 - 25); y = 387.; 
    currentSprite = frogSprites.upSprite;
    frameIndex = 0.;
    width = 35; height = 30; 
  };
  cars = [];
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

let rec drawCars ctx cars = 
  match cars with 
  | [] -> ();
  | hd::tl -> drawSprite ctx hd.sprite; (drawCars ctx tl)
;;

type carsData = {
  velocity: float;
  carsAtOnceIsh: int;
  lastMade: float;
  image: spriteImageT
};;

(* velocities is measured in percent screen crossed per second *)
let carConfig = [|
  (* 0 *) { velocity = 0.; carsAtOnceIsh = 0; lastMade = 0.; image = yellowCarImage};
  (* 1 *) { velocity = 5.; carsAtOnceIsh = 3; lastMade = 0.; image = yellowCarImage};
  (* 2 *) { velocity = 3.; carsAtOnceIsh = 3; lastMade = 0.; image = yellowCarImage};
  (* 3 *)
  (* 4 *)
  (* 5 *)
|];;

let updateCar car dt = { car with sprite = { 
    car.sprite with x = let rowConfig = (Array.get carConfig car.row ) in 
                      let rowSpeed = (float_of_int worldWidth) /. rowConfig.velocity in 
                      car.sprite.x -. (rowSpeed *. dt /. 1000.) 
  } };;

let rec updateCars cars dt = 
  match cars with 
  | [] -> [];
  | hd::tl -> (updateCar hd dt) :: (updateCars tl dt)
;;


let render ctx world = 
  Canvas2dRe.setFillStyle ctx String "black";
  Canvas2dRe.fillRect ctx ~x:0. ~y:0. ~h: (float_of_int world.height) ~w:(float_of_int world.width);
  (drawGoal ctx);
  (drawGrass ctx (world.height - 60));
  (drawGrass ctx (world.height - 60 - (rowheight * 6)));
  (drawCars ctx world.cars);
  (drawSprite ctx world.frog);
;;

let lastTime = ref (Js.Date.now ());;
let lastCarTime = ref 0.;;
let lastRowProbabilities = [| 0; 3; 2 |] (* how many cars-ish you should expect at any one time *)

let rec update ctx world = 
  let now = Js.Date.now () in
  let dt = now -. !lastTime in
  (render ctx world);

  lastTime := Js.Date.now ();
  let frog = { world.frog with 
               x = world.frog.x +. (float_of_int colwidth ) *. if pressedKeys.left then -1. else if pressedKeys.right then 1. else 0.;
               y = world.frog.y +. (float_of_int rowheight) *. if pressedKeys.up then -1. else if pressedKeys.down then 1. else 0.;
               currentSprite = if pressedKeys.up then frogSprites.upSprite  
                 else if pressedKeys.down then frogSprites.downSprite
                 else if pressedKeys.left then frogSprites.leftSprite 
                 else if pressedKeys.right then frogSprites.rightSprite 
                 else world.frog.currentSprite;
               frameIndex = if (pressedKeys.down || pressedKeys.up || pressedKeys.left || pressedKeys.right) then 0.
                 else (world.frog.frameIndex +. dt *.world.frog.currentSprite.frameSpeed);
             } in 
  let movedCars = (updateCars world.cars dt) in
  let cars = if now -. !lastCarTime > 5000. then 
      (makeCar 1 yellowCarImage) :: (makeCar 2 greenCarImage):: movedCars
    else movedCars in 
  if now -. !lastCarTime > 5000. then lastCarTime := now;


  (* we want to reset directional pressedKeys after we process it once since frogger doesn't continously move, he jumps  *)
  pressedKeys.left <- false;
  pressedKeys.right <- false;
  pressedKeys.up <- false;
  pressedKeys.down <- false;
  let newWorld = {world with frog; cars; } in
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
