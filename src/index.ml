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
    );;
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
  mutable grid: bool;
}

(*pressed_keys instantiates the keys.*)
let pressedKeys = {
  direction = None;
  bbox = false;
  grid = false;
};;

(* Keydown event handler translates a key press *)
let keydown (evt:Dom.event) =
  match (toUnsafe evt)##keyCode with
  | 38 | 32 | 87 -> pressedKeys.direction <- Some Up
  | 39 | 68 -> pressedKeys.direction <- Some Right
  | 37 | 65 -> pressedKeys.direction <- Some Left
  | 40 | 83 -> pressedKeys.direction <- Some Down
  | 66 -> pressedKeys.bbox <- (not pressedKeys.bbox)
  | 71 -> pressedKeys.grid <- (not pressedKeys.grid)
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

type spriteImageT = { xStart: int; yStart: int; frames: int; frameSpeed: float; width: int; height: int; number: int; };;
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

type gameStateT = Start | Playing | Won;;

type worldT = { 
  frog: frogT;
  keys: keys;
  objects: laneObjectT list;
  state: gameStateT;
  lives: int;
  score: int;
  highscore: int;
  timer: int;
  endzone: (int * bool) list;
};;

type laneConfigT = {
  velocity: float;
  objectsAtOnceIsh: int;
  mutable nextSpawnTime: int;
  objType: spriteT;
  img: spriteImageT;
};;

let makeSpriteImage ?(number=1) ?(height=30) xStart yStart frames frameSpeed width = { 
  xStart; yStart; frames; frameSpeed; width; height; number;
};;

let windowHeight = Window.innerHeight window;;
let windowWidth = Window.innerWidth window;;

(* Frogger had a 4:3 ratio, so lets stick with that and scale at the render step *)
let height = 640;;
let width = 560;;
(* let height = 256;;
   let width = 224;; *)
let rows = 16;;
let cols = 14;;
let tileSize = height / rows ;;
let halfTileSize = tileSize / 2;;

let getRowForY y = (height - y) / tileSize;;
let getYForRow row = height - ((row) * tileSize);;

(* let worldHeight = rows * tileSize;;
   let worldWidth = cols * tileSize;; *)
let magnification = 1.;; (* visual scaling multiplier *)

let yellowCarImage = makeSpriteImage 80 262 0 0. 33;;
let greenCarImage = makeSpriteImage 70 296 0 0. 33;;
let pinkCarImage = makeSpriteImage 10 262 0 0. 31;;
let raceCarImage = makeSpriteImage 40 260 0 0. 33;;
let whiteTruckImage = makeSpriteImage 110 296 0 0. 43;;
let threeTurtleImage = makeSpriteImage ~number:3 15 402 3 2. 35;;
let twoTurleImage = makeSpriteImage ~number:2 15 402 3 2. 35;;
let smallLogImage = makeSpriteImage 10 225 0 0. 80;;
let mediumLogImage = makeSpriteImage 10 193 0 0. 115;;
let bigLogImage = makeSpriteImage 10 162 0 0. 175;;
let frogUp = makeSpriteImage ~height:23 8 362 2 20. 28;;
let frogDown = makeSpriteImage ~height:23 76 370 2 20. 28;;
let frogLeft = makeSpriteImage ~height:23 76 335 2 20. 33;;
let frogRight = makeSpriteImage ~height:23 8 335 2 20. 33;;

let intersects (rect1:rectT) (rect2:rectT) =
  let bottom1 = rect1.y +. (float_of_int rect1.height) in
  let bottom2 = rect2.y +. (float_of_int rect2.height) in
  let top1 = rect1.y in
  let top2 = rect2.y in
  let left1 = rect1.x in 
  let left2 = rect2.x in
  let right1 = rect1.x +. (float_of_int rect1.width) in 
  let right2 = rect2.x +. (float_of_int rect2.width ) in
  not ((bottom1 < top2 )|| 
       (top1 > bottom2) ||
       (right1 < left2) || 
       (left1 > right2));;

let startWorld : worldT = { 
  frog = { 
    rect = { 
      x = float_of_int (tileSize * (cols / 2 -1) ); 
      y = float_of_int (getYForRow 2) +. 10.;  
      width = 10;
      height = 20;
    };
    direction = Up;
    leftInJump = 0.;
  };
  objects = [];
  keys = pressedKeys;
  state = Start;
  lives = 5;
  score = 0;
  highscore = 0;
  timer = 30 * 1000;
  endzone = [ (0, false); (1,false); (2,false); (3,false); (4,false);]
};;

let endzoneRects = 
  (List.map (fun i ->
       let x = (float_of_int ((3*i*tileSize) +halfTileSize-(1*i))) in
       let rect = { 
         x; 
         y = (float_of_int (tileSize*2)); 
         width = tileSize; 
         height= tileSize;
       } in
       (i, rect)
     ) (0<->4))
;;


let makeLaneObject ((row, { img; velocity; objType; }): (int * laneConfigT)) = 
  let direction = if velocity > 0. then Right else Left in 
  [{
    rect = {
      x = (match direction with 
          | Right -> float_of_int (-img.width - 10) 
          | Left -> float_of_int width
          | Up | Down -> assert false);
      y = float_of_int (getYForRow row);
      width = img.width * img.number;
      height = img.height;
    };
    direction;
    img;
    velocity;
    objType;
    frameIndex = 0.;
  }];;

let drawImage ctx image sourceX sourceY sourceWidth sourceHeight dx dy dWidth dHeight =
  let unsafeCtx = (toUnsafe ctx) in
  ignore @@ unsafeCtx##drawImage image sourceX sourceY sourceWidth sourceHeight dx dy dWidth dHeight;;

let drawLaneObject ctx (sprite:laneObjectT) = 
  let frameCalc = floor (sprite.frameIndex /. 1000.) in
  let img = sprite.img in
  let startX = (float_of_int img.xStart) +. frameCalc *. (float_of_int img.width) in 
  (List.map (fun i -> 
       drawImage ctx spriteSheet startX img.yStart img.width img.height ((sprite.rect.x +. (float_of_int (img.width * i)) ) *. magnification) sprite.rect.y (magnification *. (float_of_int img.width)) (magnification *. (float_of_int tileSize))
     ) (0<->(img.number-1)))
;;

let drawFrog ctx (frog:frogT) = 
  let img = match frog.direction with 
    | Up -> frogUp;
    | Down -> frogDown;
    | Left -> frogLeft;
    | Right -> frogRight in
  let startX = float_of_int (img.xStart + if frog.leftInJump = 0. then 0 else img.width + 5) in 
  drawImage ctx spriteSheet startX img.yStart img.width img.height ((frog.rect.x-.10.) *. magnification) frog.rect.y (magnification *. (float_of_int img.width)) (magnification *. (float_of_int img.height))

let drawStartScreen ctx = 
  Canvas2dRe.setFillStyle ctx Canvas2dRe.String "white";
  Canvas2dRe.fillRect ctx ~x:0. ~y:0. ~h: (float_of_int height) ~w:(float_of_int width);
  Canvas2dRe.setFillStyle ctx Canvas2dRe.String "black";
  Canvas2dRe.font ctx "60px/1 sans-serif";
  Canvas2dRe.fillText ~x:150. ~y: 200. "Frogger" ctx;
  Canvas2dRe.font ctx "20px/1 sans-serif";
  Canvas2dRe.fillText ~x:150. ~y: 280. "Press any key to start the game" ctx;
;;

let drawWinScreen ctx = 
  Canvas2dRe.setFillStyle ctx Canvas2dRe.String "white";
  Canvas2dRe.fillRect ctx ~x:0. ~y:0. ~h: (float_of_int height) ~w:(float_of_int width);
  Canvas2dRe.setFillStyle ctx Canvas2dRe.String "black";
  Canvas2dRe.font ctx "60px/1 sans-serif";
  Canvas2dRe.fillText ~x:150. ~y: 200. "You Win!" ctx;
;;


let drawGoal ctx = 
  let y = getYForRow 15 in
  drawImage ctx spriteSheet 0 62 398 45 0 (y+halfTileSize) (magnification *. (float_of_int width)) (magnification *. (float_of_int (tileSize + halfTileSize)));;

let drawGrass ctx y = 
  drawImage ctx spriteSheet 0 120 398 33 0 y (magnification *. (float_of_int width)) (magnification *. (float_of_int tileSize));;

let rec drawCars ctx cars = 
  match cars with 
  | [] -> ();
  | hd::tl -> drawLaneObject ctx hd; (drawCars ctx tl)
;;

let drawLives ctx world =
  Canvas2dRe.setFillStyle ctx String("red");
  let livesText = "Lives: " ^ (string_of_int world.lives) in
  Canvas2dRe.fillText ~x:(float_of_int halfTileSize) ~y: ((float_of_int (getYForRow 1)) +. 20.) livesText ctx
;;

let drawTimer ctx world =
  Canvas2dRe.setFillStyle ctx String("red");
  let timerText = "Timer: " ^ (string_of_int (world.timer / 1000)) in
  Canvas2dRe.fillText ~x:(float_of_int (width - tileSize * 3)) ~y: ((float_of_int (getYForRow 1)) +. 20.) timerText ctx
;;

let drawCompletedEndzones ctx world =
  Canvas2dRe.setFillStyle ctx String("purple");
  (List.iter (fun (i, rect) ->
       if (List.assoc i world.endzone) then (
         let x = rect.x in
         Canvas2dRe.fillRect ~x ~y:(float_of_int (tileSize*2))
           ~w: (float_of_int tileSize) ~h: (float_of_int tileSize) ctx;
       ); 
     ) endzoneRects);
;;

let getJitter () = Random.int 1000 ;;
let getJitterFromNow () = (int_of_float (Js.Date.now ())) + (getJitter ());;

(* velocities is the number of seconds it takes to cross the screen. the smaller the faster *)
let laneConfig = [
  (3, { velocity = -10.; objectsAtOnceIsh = 4; nextSpawnTime = (getJitterFromNow ()); objType = Car; img = yellowCarImage;} );
  (4, { velocity = 6.; objectsAtOnceIsh = 3; nextSpawnTime = (getJitterFromNow ()); objType = Car ;img = greenCarImage; } );
  (5, { velocity = -6.; objectsAtOnceIsh = 4; nextSpawnTime = (getJitterFromNow ()); objType = Car ; img=pinkCarImage; } );
  (6, { velocity = 6.; objectsAtOnceIsh = 2; nextSpawnTime = (getJitterFromNow ()); objType = Car; img=raceCarImage;} );
  (7, { velocity = -6.; objectsAtOnceIsh = 3; nextSpawnTime = (getJitterFromNow ()); objType = Car; img=whiteTruckImage;});
  (9, { velocity = -10.; objectsAtOnceIsh = 2; nextSpawnTime = (getJitterFromNow ()); objType = BasicFloater; img=threeTurtleImage;} );
  (10, { velocity = 6.; objectsAtOnceIsh = 3; nextSpawnTime = (getJitterFromNow ()); objType = BasicFloater; img=smallLogImage;} );
  (11, { velocity = 4.; objectsAtOnceIsh = 1; nextSpawnTime = (getJitterFromNow ()); objType = BasicFloater; img=bigLogImage; } );
  (12, {velocity = -6.; objectsAtOnceIsh = 2; nextSpawnTime = (getJitterFromNow ()); objType = BasicFloater; img=twoTurleImage;} );
  (13, {velocity = 5.; objectsAtOnceIsh = 3; nextSpawnTime = (getJitterFromNow ()); objType = BasicFloater; img=mediumLogImage; } );
];;

let secondsPerWidthToPixels vel dt = 
  let speed = (float_of_int width) /. vel in
  speed *. (float_of_int dt) /. 1000.;;

let updateObj obj dt = { obj with 
                         rect = { obj.rect with 
                                  x = obj.rect.x +. secondsPerWidthToPixels obj.velocity dt
                                };
                         frameIndex = 
                           let nextFrameIndex = (obj.frameIndex +. ((float_of_int dt) *. obj.img.frameSpeed )) in
                           if (int_of_float (nextFrameIndex /. 1000.)) < obj.img.frames then nextFrameIndex else 0.;
                       };;

let rec updateCars cars dt = 
  match cars with 
  | [] -> [];
  | hd::tl -> (updateObj hd dt) :: (updateCars tl dt)
;;

let drawBoundingBoxes ctx world = 
  let frogBoxColor = ref "red" in
  (List.iter (fun { rect; } -> (
         let color = if (intersects world.frog.rect rect) then ( frogBoxColor := "blue"; "blue") else "red" in
         Canvas2dRe.setStrokeStyle ctx Canvas2dRe.String color;
         Canvas2dRe.strokeRect ~x: rect.x ~y: rect.y ~w: (float_of_int rect.width) ~h: (float_of_int rect.height) ctx;
       )) (world.objects) );
  let rect = world.frog.rect in
  Canvas2dRe.setStrokeStyle ctx Canvas2dRe.String !frogBoxColor;
  Canvas2dRe.strokeRect ~x: rect.x ~y: rect.y ~w: (float_of_int rect.width) ~h: (float_of_int rect.height) ctx;
;;

let drawGrid ctx = 
  Canvas2dRe.setStrokeStyle ctx Canvas2dRe.String "red";
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
  Canvas2dRe.setFillStyle ctx String "rgb(1,4,69)";
  Canvas2dRe.fillRect ctx ~x:0. ~y:0. ~h: (float_of_int height) ~w:(float_of_int width);
  Canvas2dRe.setFillStyle ctx String "black";
  Canvas2dRe.fillRect ctx ~x:0. ~y:(float_of_int (getYForRow 7)) ~h: (float_of_int height) ~w:(float_of_int width);
  if pressedKeys.grid then drawGrid ctx;
  if pressedKeys.bbox then drawBoundingBoxes ctx world;
  (drawGoal ctx);
  (drawGrass ctx (getYForRow 2));
  (drawGrass ctx (getYForRow 8));
  (drawLives ctx world);
  (drawTimer ctx world);
  (drawCars ctx world.objects);
  (drawFrog ctx world.frog);
  (drawCompletedEndzones ctx world);
;;

let lastTime = ref (int_of_float (Js.Date.now ()));;

let updateFrog frog (collisions:laneObjectT list) dt = 
  let floatedX = try 
      let floatieThing = List.find (fun (obj:laneObjectT) -> match obj.objType with BasicFloater -> true | _ -> false) collisions in
      secondsPerWidthToPixels floatieThing.velocity dt 
    with Not_found -> 0. in
  if frog.leftInJump > 0. then
    let distanceToTravel = min ((float_of_int tileSize) *. ((float_of_int dt) /. 100. )) frog.leftInJump in
    { frog with 
      rect = {
        frog.rect with
        x = frog.rect.x +. ( distanceToTravel *. match frog.direction with Left -> -1. | Right -> 1. | _ -> 0. ) +. floatedX;
        y = frog.rect.y +. ( distanceToTravel *. match frog.direction with Down -> 1. | Up -> -1. | _ -> 0. );
      };
      leftInJump = frog.leftInJump -. distanceToTravel;
    }
  else match pressedKeys.direction with 
    | None -> { frog with rect= { frog.rect with x = frog.rect.x +. floatedX } }
    | Some direction -> {frog with direction; leftInJump = float_of_int tileSize; };;

let isCar (obj:laneObjectT) = match obj.objType with Car -> true | _ -> false;; 

let stepWorld world now dt = 
  let collisions = List.filter (fun obj -> intersects obj.rect world.frog.rect ) world.objects in
  let endzoneCollisions = List.filter (fun (_,rect) -> intersects rect world.frog.rect ) endzoneRects in 
  let hasCarCollision = List.exists isCar collisions in
  let isInWater = List.length collisions = 0 && (getRowForY (int_of_float world.frog.rect.y)) > 7 && world.frog.leftInJump = 0. in
  let timerIsUp = world.timer <= 0 in
  let frog = updateFrog world.frog collisions dt in
  let movedLaneObjects = (updateCars world.objects dt) in
  let newLaneObjects = (List.map
                          (fun (rowNum, (cfg:laneConfigT)) -> if now > cfg.nextSpawnTime then (
                               cfg.nextSpawnTime <- (getJitterFromNow ()) + (int_of_float ((abs_float cfg.velocity) *. 1000. /. (float_of_int cfg.objectsAtOnceIsh )));
                               Some (makeLaneObject (rowNum, cfg));
                             ) 
                             else None) laneConfig) |> deoptionalize |> List.flatten in
  let objects = (movedLaneObjects @ newLaneObjects ) in 
  if (List.length endzoneCollisions) > 0 then (
    let (ithCollision, _ ) = (List.hd endzoneCollisions)in
    let endzone = (List.map (fun (i, curr) -> (i, curr || ithCollision = i)) world.endzone) in
    if List.exists (fun (_, boo) -> boo ) endzone then { world with state=Won}
    else { startWorld with lives=world.lives; state=Playing; endzone; };
  ) 
  else if hasCarCollision || isInWater || timerIsUp then ( 
    if world.lives = 1
    then startWorld 
    else { world with 
           frog=startWorld.frog; 
           objects=startWorld.objects; 
           timer=startWorld.timer; 
           lives=world.lives-1 
         }
  ) else (
    {world with frog; objects; timer = world.timer - dt; }
  ) 
;;

let rec update ctx (world:worldT) = 
  let now = int_of_float (Js.Date.now ()) in
  let dt = now - !lastTime in
  let nextWorld = ref world in
  if world.state = Playing then (
    nextWorld := stepWorld world now dt;
    render ctx world;
  ) else if world.state = Start then (
    nextWorld := if pressedKeys.direction = None then world else {world with state = Playing };
    drawStartScreen ctx;
  ) else (
    drawWinScreen ctx;
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
