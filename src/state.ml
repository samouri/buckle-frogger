external toUnsafe : 'a -> < .. > Js.t = "%identity"

open Utils
open Types

let savedHighScore = match (Dom.Storage.getItem "highscore" Dom.Storage.localStorage) with
  | Some n -> int_of_string n;
  | None -> 0
;;

(* Represents the values of relevant key bindings. *)
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

let xDown : int option ref = ref None;;
let yDown : int option ref = ref None;;

let handleTouchStart _ = 
  ignore @@ [%raw "arguments[0].preventDefault()"];

  xDown := Some [%raw "arguments[0].touches[0].clientX"];
  yDown := Some [%raw "arguments[0].touches[0].clientY"];
;;

let handleTouchMove _ = 
  ignore @@ [%raw "arguments[0].preventDefault()"];

  match (!xDown, !yDown) with
  | (Some xdwn, Some ydwn) ->
    let xUp = [%raw "arguments[0].touches[0].clientX"] in
    let yUp = [%raw "arguments[0].touches[0].clientY"] in
    let xDiff = xdwn - xUp in
    let yDiff = ydwn - yUp in
    if abs xDiff > abs yDiff then 
      pressedKeys.direction <- if xDiff > 0 then Some Left else Some Right
    else
      (pressedKeys.direction <- if yDiff > 0 then Some Up else Some Down);

  | (_,_) -> ();
;;

(Webapi.Dom.Window.addEventListener "touchstart" handleTouchStart Webapi.Dom.window );;
(Webapi.Dom.Window.addEventListener "touchmove" handleTouchMove Webapi.Dom.window );;
(Webapi.Dom.Window.addEventListener "keydown" keydown Webapi.Dom.window );;

let frogAnimationLength = 1000;;
let startWorld : worldT = { 
  frog = { 
    rect = { 
      x = float_of_int (tileSize * (cols / 2 -1) ); 
      y = float_of_int (getYForRow 2 + 8);  
      width = 10;
      height = 10;
    };
    direction = Up;
    leftInJump = 0.;
    leftInAnimation = None;
  };
  objects = [];
  keys = pressedKeys;
  state = Start;
  lives = 5;
  score = 0;
  maxRow = 1;
  highscore = savedHighScore;
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

let secondsPerWidthToPixels vel dt = 
  let speed = (float_of_int width) /. vel in
  speed *. (float_of_int dt) /. 1000.;;

let updateObj obj dt = 
  let nextFrameIndex = (obj.frameIndex +. ((float_of_int dt) *. obj.img.frameSpeed )) in
  let nextFrame = nextFrameIndex /. 1000. in
  let frameSpeed = 
    if (nextFrame >= (float_of_int obj.img.frames) && obj.img.frameSpeed > 0.) || (nextFrame <= 0. && obj.img.frameSpeed < 0.) then 
      obj.img.frameSpeed *. -1.
    else 
      obj.img.frameSpeed in
  { 
    obj with 
    rect = { 
      obj.rect with 
      x = obj.rect.x +. secondsPerWidthToPixels obj.velocity dt
    };
    img = {
      obj.img with
      frameSpeed;
    };
    frameIndex = if nextFrame < float_of_int obj.img.frames && nextFrame > 0. then 
        nextFrameIndex 
      else if obj.objType = DivingTurtles then 
        obj.frameIndex
      else
        0.;
  };;

let isCar (obj:laneObjectT) = match obj.objType with Car -> true | _ -> false;; 

let twoTurtleCount = ref 0;;
let threeTurtleCount = ref 0;;

let makeLaneObject ((row, laneConfig): (int * laneConfigT)) = 
  let direction = if laneConfig.velocity > 0. then Right else Left in 
  let (objType, img) = match row with
    | 9 -> 
      threeTurtleCount := !threeTurtleCount + 1; 
      if !threeTurtleCount mod 4 = 0 then 
        (DivingTurtles, divingThreeTurtles) 
      else 
        (laneConfig.objType, threeTurtleImage);
    | 12 ->
      twoTurtleCount := !twoTurtleCount + 1;  
      if !twoTurtleCount mod 4 = 0 then
        (DivingTurtles, divingTwoTurtles)
      else
        (laneConfig.objType, twoTurtleImage);
    | _ -> (laneConfig.objType, laneConfig.img)
  in 
  {
    rect = {
      x = (match direction with 
          | Right -> float_of_int (-img.width) 
          | Left -> float_of_int width
          | Up | Down -> assert false);
      y = float_of_int (getYForRow row);
      width = img.width * img.number;
      height = img.height;
    };
    direction;
    img;
    velocity = laneConfig.velocity;
    objType;
    frameIndex = 0.;
  };;

let getJitter () = Random.int 1000 ;;
let getJitterFromNow () = (int_of_float (Js.Date.now ())) + (getJitter ());;

(* velocities is the number of seconds it takes to cross the screen. the smaller the faster *)
let laneConfig = [
  (3, { velocity = -10.; objectsAtOnceIsh = 4.; nextSpawnTime = (getJitterFromNow ()); objType = Car; img = yellowCarImage;} );
  (4, { velocity = 6.; objectsAtOnceIsh = 3.; nextSpawnTime = (getJitterFromNow ()); objType = Car ;img = greenCarImage; } );
  (5, { velocity = -6.; objectsAtOnceIsh = 4.; nextSpawnTime = (getJitterFromNow ()); objType = Car ; img=pinkCarImage; } );
  (6, { velocity = 6.; objectsAtOnceIsh = 2.; nextSpawnTime = (getJitterFromNow ()); objType = Car; img=raceCarImage;} );
  (7, { velocity = -6.; objectsAtOnceIsh = 3.; nextSpawnTime = (getJitterFromNow ()); objType = Car; img=whiteTruckImage;});
  (9, { velocity = -10.; objectsAtOnceIsh = 2.; nextSpawnTime = (getJitterFromNow ()); objType = BasicFloater; img=threeTurtleImage;} );
  (10, { velocity = 6.; objectsAtOnceIsh = 3.; nextSpawnTime = (getJitterFromNow ()); objType = BasicFloater; img=smallLogImage;} );
  (11, { velocity = 4.; objectsAtOnceIsh = 1.7; nextSpawnTime = (getJitterFromNow ()); objType = BasicFloater; img=bigLogImage; } );
  (12, {velocity = -6.; objectsAtOnceIsh = 2.; nextSpawnTime = (getJitterFromNow ()); objType = BasicFloater; img=twoTurtleImage;} );
  (13, {velocity = 5.; objectsAtOnceIsh = 3.; nextSpawnTime = (getJitterFromNow ()); objType = BasicFloater; img=mediumLogImage; } );
];; 

(* state updates are modeled as a series of transformations to state.
 * the fn signature is: (world, dt, temp) -> (nextWorld, dt, temp).
 * world is all of current state, dt is time that has passed since the last update, and temp is working memory for passes to communicate through.
 * for example, collisions are detected early on in the process and then that work is reused in various other passes.
*)

let updateFrog (world, dt, tmp ) = 
  let frog = world.frog in
  let floatedX = try 
      let floatieThing = List.find (fun (obj:laneObjectT) -> match obj.objType with Car -> false | _  -> true) tmp.laneCollisions in
      secondsPerWidthToPixels floatieThing.velocity dt 
    with Not_found -> 0. in
  let newFrog =  if isSome frog.leftInAnimation then world.frog
    else if frog.leftInJump > 0. then
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
      | Some direction -> 
        let nextRect = {frog.rect with
                        x = frog.rect.x +. ( (float_of_int tileSize) *. match direction with Left -> -1. | Right -> 1. | _ -> 0. );
                        y = frog.rect.y +. ( (float_of_int tileSize) *. match direction with Down -> 1. | Up -> -1. | _ -> 0. );
                       } in
        let isValid = isRectInBounds nextRect in
        if isValid 
        then {frog with direction; leftInJump = float_of_int tileSize; }
        else frog 
  in
  ({ world with frog = newFrog }, dt, tmp)
;;

let rejectUnderWaterTurtles laneObjects = List.filter 
    (fun (obj:laneObjectT) -> 
       not (obj.objType = DivingTurtles) ||
       not ( (floor ( obj.frameIndex /. 1000.) ) = 5.)) 
    laneObjects
;;

let handleDeathCheck (world, dt, {laneCollisions} as tmp) = 
  let hasCarCollision = List.exists isCar laneCollisions in
  let isInWater = laneCollisions |> rejectUnderWaterTurtles |> List.length = 0 && 
                  (getRowForY (int_of_float world.frog.rect.y)) > 7 && 
                  world.frog.leftInJump = 0. in
  let isOutOfBounds = isRectOutOfBounds world.frog.rect in
  let timerIsUp = world.timer <= 0 in
  let isDead =  hasCarCollision || isInWater || timerIsUp || isOutOfBounds in
  (* either start an animation, or handle death scenario once animation is over *)
  let newWorld = match (isDead, world.frog.leftInAnimation, world.lives) with 
    | (false, None, _) -> world
    | (true, None, _) -> { world with frog = { world.frog with leftInAnimation = Some frogAnimationLength } }
    | (_, Some 0, 1) -> { world with state = Lost }
    | (_, Some 0, n) -> { world with 
                          frog = startWorld.frog; 
                          timer = startWorld.timer; 
                          lives = n - 1 
                        } 
    | (_, Some n, _) -> { world with frog = { world.frog with leftInAnimation = Some (max 0 (n - dt)) } }
  in
  (newWorld, dt, tmp)
;;

let handleGameWinCheck (world, dt, tmp) = 
  let allGoalsFilled = not (List.exists (fun (_, boo) -> not boo ) world.endzone) in
  if allGoalsFilled then 
    ({ world with state=Won}, dt, tmp)
  else 
    (world, dt, tmp)
;;

let handleEndzoneCheck (world, dt, tmp) = 
  let intersectsWithFrog = fun (_, rect) -> intersects world.frog.rect rect in
  let endzoneCollision = find_opt intersectsWithFrog endzoneRects in
  match endzoneCollision with 
  | None -> (world, dt, tmp)
  | Some (matchedI, _) -> 
    let alreadyFilled = List.assoc matchedI world.endzone in
    let endzone = (List.map (fun (i, curr) -> (i, curr || matchedI = i)) world.endzone) in
    let newWorld = if alreadyFilled then 
        world 
      else { world with 
             frog = startWorld.frog; 
             timer = startWorld.timer; 
             maxRow = startWorld.maxRow;
             score = world.score + 200 + (world.timer / 1000);
             endzone; 
           };
    in
    (newWorld, dt, tmp)
;; 

let findCollisions (world, dt, tmp) = 
  let collisions = List.filter (fun obj -> intersects obj.rect world.frog.rect ) world.objects in
  let newTmp = { tmp with laneCollisions = collisions } in
  (world, dt, newTmp )
;;


let handleScoreUpdate (world, dt, tmp) = 
  let newFrogRow = getRowForY (int_of_float world.frog.rect.y) in
  let score = world.score + if newFrogRow > world.maxRow then 10 else 0 in
  let newWorld = {
    world with 
    score; 
    maxRow = max newFrogRow world.maxRow
  } in
  (newWorld, dt, tmp)
;;

(* a glorified check for max between score and highscore. 
   side effect: save into localstorage if the max is exceeded 
*) 
let handleHighScoreCheck (world, dt, tmp) = 
  let highscore = if world.score > world.highscore then (
      (Dom.Storage.setItem "highscore" (string_of_int world.score) Dom.Storage.localStorage);
      world.score;
    ) else world.highscore in
  let newWorld = {world with highscore} in
  (newWorld, dt, tmp)
;;

let shrinkTimer (world, dt, tmp) = 
  let timer = world.timer - dt in
  let newWorld = {world with timer} in
  (newWorld, dt, tmp)
;;

let updateLaneObjects (world, dt, tmp) =
  let filterOutOfBounds = List.filter (fun obj -> isRectInBounds obj.rect ) in
  let movedLaneObjects = (List.map (fun o -> updateObj o dt ) world.objects) 
                         |> filterOutOfBounds in
  let spawnedLaneObjects = (List.map
                              (fun (rowNum, (cfg:laneConfigT)) -> if tmp.now > cfg.nextSpawnTime then (
                                   cfg.nextSpawnTime <- (getJitterFromNow ()) + (int_of_float ((abs_float cfg.velocity) *. 1000. /. cfg.objectsAtOnceIsh ));
                                   Some (makeLaneObject (rowNum, cfg));
                                 ) 
                                 else None) laneConfig) |> deoptionalize in
  let objects = (movedLaneObjects @ spawnedLaneObjects ) in
  let newWorld = {world with objects;} in
  (newWorld, dt, tmp) 
;;

let getWorld (world, _, _) = world;;

let stepWorld world now dt = 
  (world, dt, {laneCollisions = []; now;} ) 
  |> findCollisions
  |> updateFrog
  |> updateLaneObjects
  |> shrinkTimer
  |> handleDeathCheck 
  |> handleEndzoneCheck 
  |> handleGameWinCheck
  |> handleScoreUpdate
  |> handleHighScoreCheck
  |> getWorld 
;;