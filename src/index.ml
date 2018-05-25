open Webapi.Dom
external toUnsafe : 'a -> < .. > Js.t = "%identity"
external spritesUrl: string = "../assets/frogger_sprites.png" [@@bs.module];;

let isSome = function 
  | Some _ -> true 
  | None -> false;;

let deoptionalize lst = 
  List.filter isSome lst
  |> List.map (function 
      | Some x -> x 
      | None -> assert false
    )

(* define an infix operator to create a range between numbers. WTF this is crazy *)
let (--) i j = 
  let rec aux n acc =
    if n < i then acc else aux (n-1) (n :: acc)
  in aux j [] ;;

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
let greenCarImage = makeSpriteImage 70 290 0 0.;;
let pinkCarImage = makeSpriteImage 10 260 0 0.;;
let raceCarImage = makeSpriteImage 40 260 0 0.;;
let whiteTruckImage = makeSpriteImage 110 290 0 0.;;
let threeTurtleImage = makeSpriteImage 15 400 3 1.5;;
let smallLogImage = makeSpriteImage 10 230 0 0.;;
let mediumLogImage = makeSpriteImage 10 198 0 0.;;
let bigLogImage = makeSpriteImage 10 160 0 0.;;

let makeCar ?width:(width=33) ?height:(height=30) row img = [
  {
    row;
    sprite = {
      currentSprite = img;
      x = if row mod 2 = 0 then -30. else float_of_int worldWidth;
      y = float_of_int (worldHeight - 62 - (row * rowheight));
      frameIndex = 0.;
      width;
      height;
    }
  }];; 

let makeTurtles ?width:(width=33) ?height:(height=30) row img n = 
  List.map (fun i -> {
        row;
        sprite = {
          currentSprite = img;
          x = if row mod 2 = 0 then -30. -. (float_of_int (width * i)) else float_of_int worldWidth +. (float_of_int (width * i));
          y = float_of_int (worldHeight - 62 - (row * rowheight));
          frameIndex = 0.;
          width;
          height;
        }
      }) (1--n);; 

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
  mutable nextSpawnTime: float;
  make: unit -> carT list;
};;

let getJitter () = float_of_int (Random.int 1000 );;
let getJitterFromNow () = (Js.Date.now ()) +. (getJitter ());;

(* velocities is measured in percent screen crossed per second *)
let carConfig = [|
  (* 0 *) { velocity = 0.; carsAtOnceIsh = 0; nextSpawnTime = 0.; make = fun () -> makeCar 0 yellowCarImage};
  (* 1 *) { velocity = 10.; carsAtOnceIsh = 4; nextSpawnTime = (getJitterFromNow ()); make = fun () -> makeCar 1 yellowCarImage};
  (* 2 *) { velocity = 6.; carsAtOnceIsh = 3; nextSpawnTime = (getJitterFromNow ()); make = fun () -> makeCar 2 greenCarImage };
  (* 3 *) { velocity = 6.; carsAtOnceIsh = 4; nextSpawnTime = (getJitterFromNow ()); make = fun () -> makeCar 3  pinkCarImage };
  (* 4 *) { velocity = 6.; carsAtOnceIsh = 2; nextSpawnTime = (getJitterFromNow ()); make = fun () -> makeCar 4 raceCarImage };
  (* 5 *) { velocity = 6.; carsAtOnceIsh = 3; nextSpawnTime = (getJitterFromNow ()); make = fun () -> makeCar ~width:66 5 whiteTruckImage};
  (* 6 *) { velocity = 0.; carsAtOnceIsh = 0; nextSpawnTime = 0.; make = fun () -> makeCar 0 yellowCarImage };
  (* 7 *) { velocity = 10.; carsAtOnceIsh = 2; nextSpawnTime = (getJitterFromNow ()); make = fun () -> makeTurtles ~width:36 7 threeTurtleImage 3 };
  (* 8 *) { velocity = 6.; carsAtOnceIsh = 3; nextSpawnTime = (getJitterFromNow ()); make = fun () -> makeCar 8 ~width:80 smallLogImage };
  (* 9 *) { velocity = 6.; carsAtOnceIsh = 1; nextSpawnTime = (getJitterFromNow ()); make = fun () -> makeCar 9 ~width:180 bigLogImage };
  (* 10 *) { velocity = 6.; carsAtOnceIsh = 2; nextSpawnTime = (getJitterFromNow ()); make = fun () -> makeTurtles ~width:36 10 threeTurtleImage 2 };
  (* 11 *) { velocity = 6.; carsAtOnceIsh = 3; nextSpawnTime = (getJitterFromNow ()); make = fun () -> makeCar ~width:120 11 mediumLogImage};
  (* 12 *) { velocity = 0.; carsAtOnceIsh = 0; nextSpawnTime = 0.; make = fun () -> makeCar 0 yellowCarImage };
|];;

let updateCar car dt = { car with sprite = { 
    car.sprite with 
    frameIndex = if (int_of_float (car.sprite.frameIndex /. 1000.)) >= car.sprite.currentSprite.frames then 0. else car.sprite.frameIndex +. dt *. car.sprite.currentSprite.frameSpeed;
    x = let rowConfig = (Array.get carConfig car.row ) in 
      let rowSpeed = (float_of_int worldWidth) /. rowConfig.velocity in 
      let direction = if car.row mod 2 = 0 then 1. else -1. in
      car.sprite.x +. direction *. (rowSpeed *. dt /. 1000.); 
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
  let newCars = (Array.map
                   (fun cfg -> if cfg.velocity > 0. && now > cfg.nextSpawnTime then (
                        cfg.nextSpawnTime <- (now +. ((cfg.velocity) *. 1000. /. (float_of_int cfg.carsAtOnceIsh )) +. (getJitter ()) );
                        Some (cfg.make ()) 
                      ) 
                      else None) carConfig) |> Array.to_list |> deoptionalize |> List.flatten in
  let cars = (newCars @ movedCars ) in

  (* we want to reset directional pressedKeys after we process it once since frogger doesn't continously move, he jumps  *)
  pressedKeys.left <- false;
  pressedKeys.right <- false;
  pressedKeys.up <- false;
  pressedKeys.down <- false;
  let newWorld = {world with frog; cars; } in
  (Webapi.requestAnimationFrame (fun _ -> (update ctx newWorld )))
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
