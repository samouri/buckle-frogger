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

type directionT = Left | Right | Up | Down;;

(* define an infix operator to create a range between numbers. WTF this is crazy *)
let (<->) i j = 
  let rec aux n acc =
    if n < i then acc else aux (n-1) (n :: acc)
  in aux j [] ;;

(* Represents the values of relevant key bindings. *)
type keys = {
  mutable direction: directionT option;
  mutable bbox: bool;
}

(*pressed_keys instantiates the keys.*)
let pressedKeys = {
  direction = None;
  bbox = false;
};;

(* Keydown event handler translates a key press *)
let keydown (evt:Dom.event) =
  match (toUnsafe evt)##keyCode with
  | 38 | 32 | 87 -> pressedKeys.direction <- Some Up
  | 39 | 68 -> pressedKeys.direction <- Some Right
  | 37 | 65 -> pressedKeys.direction <- Some Left
  | 40 | 83 -> pressedKeys.direction <- Some Down
  | 66 -> pressedKeys.bbox <- (not pressedKeys.bbox)
  | _ -> Js.log ("did not find nothing" ^ ((toUnsafe evt)##keyCode))
;;


(* Keyup event handler translates a key release *)
(* let keyup (evt:Dom.event) =
   match (toUnsafe evt)##keyCode with
   | 38 | 32 | 87 -> pressedKeys.direction <- None
   | 39 | 68 -> pressedKeys.direction <- None
   | 37 | 65 -> pressedKeys.direction <- None
   | 40 | 83 -> pressedKeys.direction <- None
   | _ -> ()
   ;; *)

(Window.addEventListener "keydown" keydown window );;
(* (Window.addEventListener "keyup" keyup window );; *)

let spriteSheet = HtmlImageElement.make ();;
(HtmlImageElement.src spriteSheet spritesUrl);;

type spriteImageT = { xStart: int; yStart: int; frames: int; frameSpeed: float; width: int; height: int;};;
type rectT = {
  x: float;
  y: float;
  width: int;
  height: int;
}

type frogT = {
  rect: rectT;
  leftInJump: float;
  direction: directionT;
};;
type spriteT = Car | BasicFloater | DivingTurtles;; 

type laneObjectT = {
  rect: rectT;
  frameIndex: float;
  direction: directionT;
  img: spriteImageT;
  velocity: float;
  objType: spriteT;
}

type worldT = { 
  frog: frogT;
  keys: keys;
  objects: laneObjectT list;
};;

type laneConfigT = {
  velocity: float;
  objectsAtOnceIsh: int;
  mutable nextSpawnTime: int;
  objType: spriteT;
  img: spriteImageT;
};;

let makeSpriteImage xStart yStart frames frameSpeed width = { 
  xStart; yStart; frames; frameSpeed; width; height = 30;
};;

let windowHeight = Window.innerHeight window;;
let windowWidth = Window.innerWidth window;;

(* Frogger had a 4:3 ratio, so lets stick with that and scale at the render step *)
let height = 640;;
let width = 480;;
let tileSize = 40;;
let halfTileSize = 20;;
let rows = 16;;
let cols = 12;;

let getRowForY y = (height - y) / tileSize;;
let getYForRow row = height - ((row) * tileSize);;

(* let worldHeight = rows * tileSize;;
   let worldWidth = cols * tileSize;; *)
let magnification = 1.;; (* visual scaling multiplier *)

let yellowCarImage = makeSpriteImage 80 260 0 0. 33;;
let greenCarImage = makeSpriteImage 70 290 0 0. 33;;
let pinkCarImage = makeSpriteImage 10 260 0 0. 33;;
let raceCarImage = makeSpriteImage 40 260 0 0. 33;;
let whiteTruckImage = makeSpriteImage 110 290 0 0. 66;;
let threeTurtleImage = makeSpriteImage 15 400 3 1.5 36;;
let smallLogImage = makeSpriteImage 10 230 0 0. 80;;
let mediumLogImage = makeSpriteImage 10 198 0 0. 120;;
let bigLogImage = makeSpriteImage 10 160 0 0. 180;;
let frogUp = makeSpriteImage 0 370 2 20. 33;;
let frogDown = makeSpriteImage 70 370 2 20. 33;;
let frogLeft = makeSpriteImage 70 335 2 20. 33;;
let frogRight = makeSpriteImage 0 335 2 20. 33;;

let startWorld : worldT = { 
  frog = { 
    rect = { 
      x = float_of_int (tileSize * (cols / 2 -1) ); 
      y = float_of_int (getYForRow 2) +. 10.;  
      width = 30;
      height = 30;
    };
    direction = Up;
    leftInJump = 0.;
  };
  objects = [];
  keys = pressedKeys;
};;


let makeLaneObject ((row, { img; velocity; objType }): (int * laneConfigT)) = 
  let direction = if velocity > 0. then Right else Left in 
  [{
    rect = {
      x = (match direction with 
          | Right -> float_of_int (-width - 10) 
          | Left -> float_of_int width
          | Up | Down -> assert false);
      y = float_of_int (height - (row * tileSize));
      width = img.width;
      height = img.height;
    };
    direction;
    img;
    velocity;
    objType;
    frameIndex = 0.;
  }];;

(* let makeTurtles row (img:spriteImageT) n direction = 
   List.map (fun i -> {
        row;
        sprite = {
          currentSprite = img;
          x = (match direction with Right -> -30. -. (float_of_int (img.width * i)) | Left ->  float_of_int worldWidth +. (float_of_int (img.width * i)));
          y = float_of_int (worldHeight - 62 - (row * rowHeight));
          frameIndex = 0.;
        }
      }) (1<->n);;  *)

let drawImage ctx image sourceX sourceY sourceWidth sourceHeight dx dy dWidth dHeight =
  let unsafeCtx = (toUnsafe ctx) in
  ignore @@ unsafeCtx##drawImage image sourceX sourceY sourceWidth sourceHeight dx dy dWidth dHeight;;

let drawLaneObject ctx (sprite:laneObjectT) = 
  let frameCalc = (sprite.frameIndex /. 1000.) in
  let img = sprite.img in
  let startX = (float_of_int img.xStart) +. frameCalc *. (float_of_int img.width) in 
  drawImage ctx spriteSheet startX img.yStart img.width img.height (sprite.rect.x *. magnification) sprite.rect.y (magnification *. (float_of_int img.width)) (magnification *. (float_of_int img.height))

let drawFrog ctx (frog:frogT) = 
  let img = match frog.direction with 
    | Up -> frogUp;
    | Down -> frogDown;
    | Left -> frogLeft;
    | Right -> frogRight in
  let startX = float_of_int (img.xStart + if frog.leftInJump = 0. then 0 else img.width) in 
  drawImage ctx spriteSheet startX img.yStart img.width img.height (frog.rect.x *. magnification) frog.rect.y (magnification *. (float_of_int img.width)) (magnification *. (float_of_int img.height))

let drawGoal ctx = 
  let y = getYForRow 15 in
  drawImage ctx spriteSheet 0 62 398 45 0 (y+halfTileSize) (magnification *. (float_of_int width)) (magnification *. 60.);;

let drawGrass ctx y = 
  drawImage ctx spriteSheet 0 120 398 33 0 y (magnification *. (float_of_int width)) (magnification *. (float_of_int tileSize));;

let rec drawCars ctx cars = 
  match cars with 
  | [] -> ();
  | hd::tl -> drawLaneObject ctx hd; (drawCars ctx tl)
;;

let getJitter () = Random.int 1000 ;;
let getJitterFromNow () = (int_of_float (Js.Date.now ())) + (getJitter ());;

(* velocities is measured in percent screen crossed per second *)
let laneConfig = [
  (2, { velocity = -10.; objectsAtOnceIsh = 4; nextSpawnTime = (getJitterFromNow ()); objType = Car; img = yellowCarImage;} );
  (3, { velocity = 6.; objectsAtOnceIsh = 3; nextSpawnTime = (getJitterFromNow ()); objType = Car ;img = greenCarImage; } );
  (4, { velocity = -6.; objectsAtOnceIsh = 4; nextSpawnTime = (getJitterFromNow ()); objType = Car ; img=pinkCarImage; } );
  (5, { velocity = 6.; objectsAtOnceIsh = 2; nextSpawnTime = (getJitterFromNow ()); objType = Car; img=raceCarImage;} );
  (6, { velocity = -6.; objectsAtOnceIsh = 3; nextSpawnTime = (getJitterFromNow ()); objType = Car; img=whiteTruckImage;});
  (8, { velocity = -10.; objectsAtOnceIsh = 2; nextSpawnTime = (getJitterFromNow ()); objType = Car; img=threeTurtleImage;} );
  (9, { velocity = 6.; objectsAtOnceIsh = 3; nextSpawnTime = (getJitterFromNow ()); objType = Car; img=smallLogImage;} );
  (10, { velocity = 6.; objectsAtOnceIsh = 1; nextSpawnTime = (getJitterFromNow ()); objType = Car; img=bigLogImage; } );
  (11, {velocity = -6.; objectsAtOnceIsh = 2; nextSpawnTime = (getJitterFromNow ()); objType = Car; img=threeTurtleImage;} );
  (12, {velocity = 6.; objectsAtOnceIsh = 3; nextSpawnTime = (getJitterFromNow ()); objType = Car; img=mediumLogImage; } );
];;

let updateObj obj dt = { obj with 
                         rect = { obj.rect with 
                                  x = let row = getRowForY (int_of_float obj.rect.y) in
                                    let rowConfig = (List.assoc row laneConfig)  in
                                    let rowSpeed = (float_of_int width) /. rowConfig.velocity in 
                                    obj.rect.x +. (rowSpeed *. (float_of_int dt) /. 1000.); 
                                };
                         frameIndex = if (int_of_float (obj.frameIndex /. 1000.)) >= obj.img.frames then 0. else obj.frameIndex +. (float_of_int dt) *. obj.img.frameSpeed;
                       };;

let rec updateCars cars dt = 
  match cars with 
  | [] -> [];
  | hd::tl -> (updateObj hd dt) :: (updateCars tl dt)
;;


let drawGrid ctx = 
  Canvas2dRe.setStrokeStyle ctx Canvas2dRe.String"red";
  (* draw columns *)
  (List.iter (fun i -> (
         Canvas2dRe.beginPath ctx;
         Canvas2dRe.moveTo ~x: (float_of_int (i * tileSize)) ~y: 0. ctx;
         Canvas2dRe.lineTo ~x: (float_of_int (i * tileSize)) ~y: (float_of_int height) ctx;
         Canvas2dRe.stroke ctx;
       )) (0 <-> cols) );

  (* draw rows *)
  (List.iter (fun i -> (
         Canvas2dRe.beginPath ctx;
         Canvas2dRe.moveTo ~x: 0. ~y: (float_of_int (i * tileSize)) ctx;
         Canvas2dRe.lineTo ~x: (float_of_int width) ~y: (float_of_int (i * tileSize)) ctx;
         Canvas2dRe.stroke ctx;
       )) (0 <-> rows)
  );;

let render ctx (world:worldT) = 
  Canvas2dRe.setFillStyle ctx String "black";
  Canvas2dRe.fillRect ctx ~x:0. ~y:0. ~h: (float_of_int height) ~w:(float_of_int width);
  drawGrid ctx;
  (drawGoal ctx);
  (drawGrass ctx (getYForRow 2));
  (drawGrass ctx (getYForRow 7));
  (drawCars ctx world.objects);
  (drawFrog ctx world.frog);
;;

let lastTime = ref (int_of_float (Js.Date.now ()));;

let updateFrog frog dt = 
  if frog.leftInJump > 0. then
    let distanceToTravel = min ((float_of_int tileSize) *. ((float_of_int dt) /. 100. )) frog.leftInJump in
    { frog with 
      rect = {
        frog.rect with
        x = frog.rect.x +. ( distanceToTravel *. match frog.direction with Left -> -1. | Right -> 1. | _ -> 0. );
        y = frog.rect.y +. ( distanceToTravel *. match frog.direction with Down -> 1. | Up -> -1. | _ -> 0. );
      };
      leftInJump = frog.leftInJump -. distanceToTravel;
    }
  else match pressedKeys.direction with 
    | None -> frog
    | Some direction -> {frog with direction; leftInJump = float_of_int tileSize; };;

let rec update ctx (world:worldT) = 
  let now = int_of_float (Js.Date.now ()) in
  let dt = now - !lastTime in
  render ctx world;

  let frog = updateFrog world.frog dt in
  let movedLaneObjects = (updateCars world.objects dt) in
  let newLaneObjects = (List.map
                          (fun (rowNum, (cfg:laneConfigT)) -> if now > cfg.nextSpawnTime then (
                               cfg.nextSpawnTime <- (getJitterFromNow ()) + (int_of_float ((abs_float cfg.velocity) *. 1000. /. (float_of_int cfg.objectsAtOnceIsh )));
                               Some (makeLaneObject (rowNum, cfg));
                             ) 
                             else None) laneConfig) |> deoptionalize |> List.flatten in
  let objects = (movedLaneObjects @ newLaneObjects ) in
  let newWorld = {world with frog; objects; } in
  lastTime := int_of_float (Js.Date.now ());
  pressedKeys.direction <- None; (* remove the press once processed *)
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
  canvas |> (Element.setAttribute "height" ((string_of_int height) ^ "px"));
  canvas |> (Element.setAttribute "width" ((string_of_int width) ^ "px"));
  canvas |> (Element.setAttribute "style" ("max-width: " ^ (string_of_int width)  ^ "px; max-height: " ^ (string_of_int height) ^ "px"));
  let context = CanvasRe.CanvasElement.getContext2d canvas in
  (update context startWorld);
;;


let _ = Window.setOnLoad window load;;
