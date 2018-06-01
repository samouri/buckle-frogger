external toUnsafe : 'a -> < .. > Js.t = "%identity"
open Utils
open Types

(* Frogger had a 4:3 ratio, so lets stick with that and scale at the render step *)
let height = 480;;
let width = 420;;
(* let height = 256;;
   let width = 224;; *)
let rows = 16;;
let cols = 14;;
let tileSize = height / rows ;;
let halfTileSize = tileSize / 2;;

let getRowForY y = (height - y) / tileSize;;
let getYForRow row = height - ((row) * tileSize);;

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

(Webapi.Dom.Window.addEventListener "keydown" keydown Webapi.Dom.window );;

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

let makeLaneObject ((row, { img; velocity; objType; }): (int * laneConfigT)) = 
  let direction = if velocity > 0. then Right else Left in 
  {
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
  (12, {velocity = -6.; objectsAtOnceIsh = 2.; nextSpawnTime = (getJitterFromNow ()); objType = BasicFloater; img=twoTurleImage;} );
  (13, {velocity = 5.; objectsAtOnceIsh = 3.; nextSpawnTime = (getJitterFromNow ()); objType = BasicFloater; img=mediumLogImage; } );
];;


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
                               cfg.nextSpawnTime <- (getJitterFromNow ()) + (int_of_float ((abs_float cfg.velocity) *. 1000. /. cfg.objectsAtOnceIsh ));
                               Some (makeLaneObject (rowNum, cfg));
                             ) 
                             else None) laneConfig) |> deoptionalize in
  let objects = (movedLaneObjects @ newLaneObjects ) in 
  if (List.length endzoneCollisions) > 0 then (
    let (ithCollision, _ ) = (List.hd endzoneCollisions)in
    let endzone = (List.map (fun (i, curr) -> (i, curr || ithCollision = i)) world.endzone) in
    if not (List.exists (fun (_, boo) -> not boo ) endzone) then { world with state=Won}
    else { startWorld with lives=world.lives; state=Playing; endzone; };
  ) 
  else if hasCarCollision || isInWater || timerIsUp then ( 
    if world.lives = 1
    then { world with state = Lose } 
    else { world with 
           frog=startWorld.frog; 
           timer=startWorld.timer; 
           lives=world.lives-1 
         }
  ) else (
    {world with frog; objects; timer = world.timer - dt; }
  ) 
;;