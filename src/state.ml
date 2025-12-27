open Direction
open Input
open Types
open Utils

type t = worldT

type event =
  | Start
  | Reset
  | Scored of int
  | Highscore_updated of int

let frog_animation_length = 1000
let start_timer_ms = 30 * 1000

let start_world ~highscore ~input : worldT =
  { frog =
      { rect =
          { x = float_of_int (tileSize * ((cols / 2) - 1))
          ; y = float_of_int (get_y_for_row 2 + 8)
          ; width = 10
          ; height = 10
          }
      ; direction = Up
      ; leftInJump = 0.
      ; leftInAnimation = None
      }
  ; input
  ; objects = []
  ; state = Start
  ; lives = 5
  ; score = 0
  ; maxRow = 1
  ; highscore
  ; timer = start_timer_ms
  ; endzone = [ 0, false; 1, false; 2, false; 3, false; 4, false ]
  }
;;

let endzone_rects =
  List.map
    (fun i ->
      let x = float_of_int ((3 * i * tileSize) + halfTileSize - (1 * i)) in
      let rect =
        { x; y = float_of_int (tileSize * 2); width = tileSize; height = tileSize }
      in
      i, rect)
    (0 <-> 4)
;;

let secondsPerWidthToPixels vel dt =
  let speed = float_of_int width /. vel in
  speed *. float_of_int dt /. 1000.
;;

let updateObj obj dt =
  let nextFrameIndex = obj.frameIndex +. (float_of_int dt *. obj.img.frameSpeed) in
  let nextFrame = nextFrameIndex /. 1000. in
  let frameSpeed =
    if (nextFrame >= float_of_int obj.img.frames && obj.img.frameSpeed > 0.)
       || (nextFrame <= 0. && obj.img.frameSpeed < 0.)
    then obj.img.frameSpeed *. -1.
    else obj.img.frameSpeed
  in
  { obj with
    rect = { obj.rect with x = obj.rect.x +. secondsPerWidthToPixels obj.velocity dt }
  ; img = { obj.img with frameSpeed }
  ; frameIndex =
      (if nextFrame < float_of_int obj.img.frames && nextFrame > 0.
       then nextFrameIndex
       else if obj.objType = DivingTurtles
       then obj.frameIndex
       else 0.)
  }
;;

let isCar (obj : laneObjectT) =
  match obj.objType with
  | Car -> true
  | _ -> false
;;

let twoTurtleCount = ref 0
let threeTurtleCount = ref 0

let makeLaneObject ((row, laneConfig) : int * laneConfigT) =
  let direction = if laneConfig.velocity > 0. then Right else Left in
  let objType, img =
    match row with
    | 9 ->
      threeTurtleCount := !threeTurtleCount + 1;
      if !threeTurtleCount mod 4 = 0
      then DivingTurtles, divingThreeTurtles
      else laneConfig.objType, threeTurtleImage
    | 12 ->
      twoTurtleCount := !twoTurtleCount + 1;
      if !twoTurtleCount mod 4 = 0
      then DivingTurtles, divingTwoTurtles
      else laneConfig.objType, twoTurtleImage
    | _ -> laneConfig.objType, laneConfig.img
  in
  { rect =
      { x =
          (match direction with
           | Right -> float_of_int (-img.width)
           | Left -> float_of_int width
           | Up | Down -> assert false)
      ; y = float_of_int (get_y_for_row row)
      ; width = img.width * img.number
      ; height = img.height
      }
  ; direction
  ; img
  ; velocity = laneConfig.velocity
  ; objType
  ; frameIndex = 0.
  }
;;

let getJitter () = Random.int 1000
let getJitterFromNow now = now + getJitter ()

(* velocities is the number of seconds it takes to cross the screen. the smaller the faster *)
let laneConfig =
  [ ( 3
    , { velocity = -10.
      ; objectsAtOnceIsh = 4.
      ; nextSpawnTime = 0
      ; objType = Car
      ; img = yellowCarImage
      } )
  ; ( 4
    , { velocity = 6.
      ; objectsAtOnceIsh = 3.
      ; nextSpawnTime = 0
      ; objType = Car
      ; img = greenCarImage
      } )
  ; ( 5
    , { velocity = -6.
      ; objectsAtOnceIsh = 4.
      ; nextSpawnTime = 0
      ; objType = Car
      ; img = pinkCarImage
      } )
  ; ( 6
    , { velocity = 6.
      ; objectsAtOnceIsh = 2.
      ; nextSpawnTime = 0
      ; objType = Car
      ; img = raceCarImage
      } )
  ; ( 7
    , { velocity = -6.
      ; objectsAtOnceIsh = 3.
      ; nextSpawnTime = 0
      ; objType = Car
      ; img = whiteTruckImage
      } )
  ; ( 9
    , { velocity = -10.
      ; objectsAtOnceIsh = 2.
      ; nextSpawnTime = 0
      ; objType = BasicFloater
      ; img = threeTurtleImage
      } )
  ; ( 10
    , { velocity = 6.
      ; objectsAtOnceIsh = 3.
      ; nextSpawnTime = 0
      ; objType = BasicFloater
      ; img = smallLogImage
      } )
  ; ( 11
    , { velocity = 4.
      ; objectsAtOnceIsh = 1.7
      ; nextSpawnTime = 0
      ; objType = BasicFloater
      ; img = bigLogImage
      } )
  ; ( 12
    , { velocity = -6.
      ; objectsAtOnceIsh = 2.
      ; nextSpawnTime = 0
      ; objType = BasicFloater
      ; img = twoTurtleImage
      } )
  ; ( 13
    , { velocity = 5.
      ; objectsAtOnceIsh = 3.
      ; nextSpawnTime = 0
      ; objType = BasicFloater
      ; img = mediumLogImage
      } )
  ]
;;

let reset_spawn_times now =
  List.iter (fun (_, cfg) -> cfg.nextSpawnTime <- getJitterFromNow now) laneConfig
;;

(* state updates are modeled as a series of transformations to state.
 * the fn signature is: (world, dt, temp) -> (nextWorld, dt, temp).
 * world is all of current state, dt is time that has passed since the last update, and temp is working memory for passes to communicate through.
 * for example, collisions are detected early on in the process and then that work is reused in various other passes.
 *)

let updateFrog (input : Input.t) (world, dt, tmp) =
  let frog = world.frog in
  let floatedX =
    try
      let floatieThing =
        List.find
          (fun (obj : laneObjectT) ->
            match obj.objType with
            | Car -> false
            | _ -> true)
          tmp.laneCollisions
      in
      secondsPerWidthToPixels floatieThing.velocity dt
    with
    | Not_found -> 0.
  in
  let newFrog =
    if isSome frog.leftInAnimation
    then world.frog
    else if frog.leftInJump > 0.
    then (
      let distanceToTravel =
        min (float_of_int tileSize *. (float_of_int dt /. 100.)) frog.leftInJump
      in
      { frog with
        rect =
          { frog.rect with
            x =
              frog.rect.x
              +. (distanceToTravel
                  *.
                  match frog.direction with
                  | Left -> -1.
                  | Right -> 1.
                  | _ -> 0.)
              +. floatedX
          ; y =
              (frog.rect.y
               +. (distanceToTravel
                   *.
                   match frog.direction with
                   | Down -> 1.
                   | Up -> -1.
                   | _ -> 0.))
          }
      ; leftInJump = frog.leftInJump -. distanceToTravel
      })
    else (
      match input.direction with
      | None -> { frog with rect = { frog.rect with x = frog.rect.x +. floatedX } }
      | Some direction ->
        let nextRect =
          { frog.rect with
            x =
              (frog.rect.x
               +. (float_of_int tileSize
                   *.
                   match direction with
                   | Left -> -1.
                   | Right -> 1.
                   | _ -> 0.))
          ; y =
              (frog.rect.y
               +. (float_of_int tileSize
                   *.
                   match direction with
                   | Down -> 1.
                   | Up -> -1.
                   | _ -> 0.))
          }
        in
        let isValid = isRectInBounds nextRect in
        if isValid
        then { frog with direction; leftInJump = float_of_int tileSize }
        else frog)
  in
  { world with frog = newFrog }, dt, tmp
;;

let rejectUnderWaterTurtles laneObjects =
  List.filter
    (fun (obj : laneObjectT) ->
      (not (obj.objType = DivingTurtles)) || not (floor (obj.frameIndex /. 1000.) = 5.))
    laneObjects
;;

let handleDeathCheck (world, dt, ({ laneCollisions; _ } as tmp)) =
  let hasCarCollision = List.exists isCar laneCollisions in
  let isInWater =
    laneCollisions |> rejectUnderWaterTurtles |> List.length = 0
    && get_row_for_y (int_of_float world.frog.rect.y) > 7
    && world.frog.leftInJump = 0.
  in
  let isOutOfBounds = isRectOutOfBounds world.frog.rect in
  let timerIsUp = world.timer <= 0 in
  let isDead = hasCarCollision || isInWater || timerIsUp || isOutOfBounds in
  (* either start an animation, or handle death scenario once animation is over *)
  let newWorld =
    match isDead, world.frog.leftInAnimation, world.lives with
    | false, None, _ -> world
    | true, None, _ ->
      { world with
        frog = { world.frog with leftInAnimation = Some frog_animation_length }
      }
    | _, Some 0, 1 -> { world with state = Lost }
    | _, Some 0, n ->
      { world with
        frog = (start_world ~highscore:world.highscore ~input:world.input).frog
      ; timer = start_timer_ms
      ; lives = n - 1
      }
    | _, Some n, _ ->
      { world with frog = { world.frog with leftInAnimation = Some (max 0 (n - dt)) } }
  in
  newWorld, dt, tmp
;;

let handleGameWinCheck (world, dt, tmp) =
  let allGoalsFilled = not (List.exists (fun (_, boo) -> not boo) world.endzone) in
  if allGoalsFilled then { world with state = Won }, dt, tmp else world, dt, tmp
;;

let handleEndzoneCheck (world, dt, tmp) =
  let intersectsWithFrog (_, rect) = intersects world.frog.rect rect in
  let endzoneCollision = find_opt intersectsWithFrog endzone_rects in
  match endzoneCollision with
  | None -> world, dt, tmp
  | Some (matchedI, _) ->
    let alreadyFilled = List.assoc matchedI world.endzone in
    let endzone = List.map (fun (i, curr) -> i, curr || matchedI = i) world.endzone in
    let newWorld =
      if alreadyFilled
      then world
      else
        { world with
          frog = (start_world ~highscore:world.highscore ~input:world.input).frog
        ; timer = start_timer_ms
        ; maxRow = 1
        ; score = world.score + 200 + (world.timer / 1000)
        ; endzone
        }
    in
    newWorld, dt, tmp
;;

let findCollisions (world, dt, tmp) =
  let collisions =
    List.filter (fun obj -> intersects obj.rect world.frog.rect) world.objects
  in
  let newTmp = { tmp with laneCollisions = collisions } in
  world, dt, newTmp
;;

let handleScoreUpdate (world, dt, tmp) =
  let newFrogRow = get_row_for_y (int_of_float world.frog.rect.y) in
  let score = world.score + if newFrogRow > world.maxRow then 10 else 0 in
  let newWorld = { world with score; maxRow = max newFrogRow world.maxRow } in
  newWorld, dt, tmp
;;

let shrinkTimer (world, dt, tmp) =
  let timer = world.timer - dt in
  let newWorld = { world with timer } in
  newWorld, dt, tmp
;;

let updateLaneObjects (world, dt, tmp) =
  let filterOutOfBounds = List.filter (fun obj -> isRectInBounds obj.rect) in
  let movedLaneObjects =
    List.map (fun o -> updateObj o dt) world.objects |> filterOutOfBounds
  in
  let spawnedLaneObjects =
    List.map
      (fun (rowNum, (cfg : laneConfigT)) ->
        if tmp.now > cfg.nextSpawnTime
        then (
          cfg.nextSpawnTime
          <- getJitterFromNow tmp.now
             + int_of_float (abs_float cfg.velocity *. 1000. /. cfg.objectsAtOnceIsh);
          Some (makeLaneObject (rowNum, cfg)))
        else None)
      laneConfig
    |> deoptionalize
  in
  let objects = movedLaneObjects @ spawnedLaneObjects in
  let newWorld = { world with objects } in
  newWorld, dt, tmp
;;

let getWorld (world, _, _) = world

let step_world input world now dt =
  (world, dt, { laneCollisions = []; now })
  |> findCollisions
  |> updateFrog input
  |> updateLaneObjects
  |> shrinkTimer
  |> handleDeathCheck
  |> handleEndzoneCheck
  |> handleGameWinCheck
  |> handleScoreUpdate
  |> getWorld
;;

let init ~highscore =
  reset_spawn_times 0;
  start_world ~highscore ~input:Input.empty
;;

let step world ~input ~now_ms ~dt_ms =
  let world = { world with input } in
  let next_world = step_world input world now_ms dt_ms in
  let events =
    []
    |> (fun evs ->
         if next_world.score > world.score
         then Scored (next_world.score - world.score) :: evs
         else evs)
    |> fun evs ->
    if next_world.highscore > world.highscore
    then Highscore_updated next_world.highscore :: evs
    else evs
  in
  next_world, List.rev events
;;
