open Types
open Utils

type t =
  { frog : Frog.t
  ; input : Input.t
  ; objects : Lane_object.t list
  ; state : Game_state.t
  ; lives : int
  ; score : int
  ; highscore : int
  ; max_row : int
  ; timer : int
  ; endzone : (int * bool) list
  }

type event =
  | Start
  | Reset
  | Scored of int
  | Highscore_updated of int

let frog_animation_length = 1000
let start_timer_ms = 30 * 1000

let start_game ~highscore ~input : t =
  { frog =
      { rect =
          { x = float_of_int (tile_size * ((cols / 2) - 1))
          ; y = float_of_int (get_y_for_row 2 + 8)
          ; width = 10
          ; height = 10
          }
      ; direction = Up
      ; left_in_jump = 0.
      ; left_in_animation = None
      }
  ; input
  ; objects = []
  ; state = Start
  ; lives = 5
  ; score = 0
  ; max_row = 1
  ; highscore
  ; timer = start_timer_ms
  ; endzone = [ 0, false; 1, false; 2, false; 3, false; 4, false ]
  }
;;

let endzone_rects =
  List.map
    (fun i ->
      let x = float_of_int ((3 * i * tile_size) + half_tile_size - (1 * i)) in
      let rect =
        Rect.
          { x; y = float_of_int (tile_size * 2); width = tile_size; height = tile_size }
      in
      i, rect)
    (0 <-> 4)
;;

let seconds_per_width_to_pixels vel dt =
  let speed = float_of_int width /. vel in
  speed *. float_of_int dt /. 1000.
;;

let update_obj (obj : Lane_object.t) dt : Lane_object.t =
  let next_frame_index = obj.frame_index +. (float_of_int dt *. obj.img.frame_speed) in
  let next_frame = next_frame_index /. 1000. in
  let frame_speed =
    if (next_frame >= float_of_int obj.img.frames && obj.img.frame_speed > 0.)
       || (next_frame <= 0. && obj.img.frame_speed < 0.)
    then obj.img.frame_speed *. -1.
    else obj.img.frame_speed
  in
  { obj with
    rect = { obj.rect with x = obj.rect.x +. seconds_per_width_to_pixels obj.velocity dt }
  ; img = { obj.img with frame_speed }
  ; frame_index =
      (if next_frame < float_of_int Lane_object.(obj.img.frames) && next_frame > 0.
       then next_frame_index
       else if obj.obj_type = Sprite.DivingTurtles
       then obj.frame_index
       else 0.)
  }
;;

let is_car (obj : Lane_object.t) =
  match obj.obj_type with
  | Car -> true
  | _ -> false
;;

let two_turtle_count = ref 0
let three_turtle_count = ref 0

let make_lane_object ((row, lane_config) : int * Lane_config.t) : Lane_object.t =
  let direction : Direction.t = if lane_config.velocity > 0. then Right else Left in
  let obj_type, img =
    match row with
    | 9 ->
      three_turtle_count := !three_turtle_count + 1;
      if !three_turtle_count mod 4 = 0
      then Sprite.DivingTurtles, diving_three_turtles
      else lane_config.obj_type, three_turtle_image
    | 12 ->
      two_turtle_count := !two_turtle_count + 1;
      if !two_turtle_count mod 4 = 0
      then DivingTurtles, diving_two_turtles
      else lane_config.obj_type, two_turtle_image
    | _ -> lane_config.obj_type, Lane_config.(lane_config.img)
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
  ; velocity = lane_config.velocity
  ; obj_type
  ; frame_index = 0.
  }
;;

let get_jitter () = Random.int 1000
let get_jitter_from_now now = now + get_jitter ()

(* velocities is the number of seconds it takes to cross the screen. the smaller the faster *)
let lane_config : (int * Lane_config.t) list =
  [ ( 3
    , { velocity = -10.
      ; objects_at_once_ish = 4.
      ; next_spawn_time = 0
      ; obj_type = Sprite.Car
      ; img = yellow_car_image
      } )
  ; ( 4
    , { velocity = 6.
      ; objects_at_once_ish = 3.
      ; next_spawn_time = 0
      ; obj_type = Sprite.Car
      ; img = green_car_image
      } )
  ; ( 5
    , { velocity = -6.
      ; objects_at_once_ish = 4.
      ; next_spawn_time = 0
      ; obj_type = Sprite.Car
      ; img = pink_car_image
      } )
  ; ( 6
    , { velocity = 6.
      ; objects_at_once_ish = 2.
      ; next_spawn_time = 0
      ; obj_type = Sprite.Car
      ; img = race_car_image
      } )
  ; ( 7
    , { velocity = -6.
      ; objects_at_once_ish = 3.
      ; next_spawn_time = 0
      ; obj_type = Sprite.Car
      ; img = white_truck_image
      } )
  ; ( 9
    , { velocity = -10.
      ; objects_at_once_ish = 2.
      ; next_spawn_time = 0
      ; obj_type = Sprite.BasicFloater
      ; img = three_turtle_image
      } )
  ; ( 10
    , { velocity = 6.
      ; objects_at_once_ish = 3.
      ; next_spawn_time = 0
      ; obj_type = Sprite.BasicFloater
      ; img = small_log_image
      } )
  ; ( 11
    , { velocity = 4.
      ; objects_at_once_ish = 1.7
      ; next_spawn_time = 0
      ; obj_type = Sprite.BasicFloater
      ; img = big_log_image
      } )
  ; ( 12
    , { velocity = -6.
      ; objects_at_once_ish = 2.
      ; next_spawn_time = 0
      ; obj_type = Sprite.BasicFloater
      ; img = two_turtle_image
      } )
  ; ( 13
    , { velocity = 5.
      ; objects_at_once_ish = 3.
      ; next_spawn_time = 0
      ; obj_type = Sprite.BasicFloater
      ; img = medium_log_image
      } )
  ]
;;

let reset_spawn_times now =
  List.iter
    (fun (_, cfg) -> Lane_config.(cfg.next_spawn_time <- get_jitter_from_now now))
    lane_config
;;

(* state updates are modeled as a series of transformations to state.
 * the fn signature is: (game, dt, temp) -> (next_game, dt, temp).
 * game is all of current state, dt is time that has passed since the last update, and temp is working memory for passes to communicate through.
 * for example, collisions are detected early on in the process and then that work is reused in various other passes.
 *)

let update_frog (input : Input.t) (game, dt, tmp) =
  let frog = game.frog in
  let floated_x =
    try
      let floatie_thing =
        List.find
          (fun obj ->
            match Lane_object.(obj.obj_type) with
            | Car -> false
            | _ -> true)
          Temp.(tmp.lane_collisions)
      in
      seconds_per_width_to_pixels floatie_thing.velocity dt
    with
    | Not_found -> 0.
  in
  let new_frog =
    if is_some frog.left_in_animation
    then game.frog
    else if frog.left_in_jump > 0.
    then (
      let distance_to_travel =
        min (float_of_int tile_size *. (float_of_int dt /. 100.)) frog.left_in_jump
      in
      { frog with
        rect =
          { frog.rect with
            x =
              frog.rect.x
              +. (distance_to_travel
                  *.
                  match frog.direction with
                  | Left -> -1.
                  | Right -> 1.
                  | _ -> 0.)
              +. floated_x
          ; y =
              (frog.rect.y
               +. (distance_to_travel
                   *.
                   match frog.direction with
                   | Down -> 1.
                   | Up -> -1.
                   | _ -> 0.))
          }
      ; left_in_jump = frog.left_in_jump -. distance_to_travel
      })
    else (
      match input.direction with
      | None -> { frog with rect = { frog.rect with x = frog.rect.x +. floated_x } }
      | Some direction ->
        let next_rect =
          { frog.rect with
            x =
              (frog.rect.x
               +. (float_of_int tile_size
                   *.
                   match direction with
                   | Left -> -1.
                   | Right -> 1.
                   | _ -> 0.))
          ; y =
              (frog.rect.y
               +. (float_of_int tile_size
                   *.
                   match direction with
                   | Down -> 1.
                   | Up -> -1.
                   | _ -> 0.))
          }
        in
        let is_valid = is_rect_in_bounds next_rect in
        if is_valid
        then { frog with direction; left_in_jump = float_of_int tile_size }
        else frog)
  in
  { game with frog = new_frog }, dt, tmp
;;

let reject_under_water_turtles (lane_objects : Lane_object.t list) =
  List.filter
    (fun (obj : Lane_object.t) ->
      (not (obj.obj_type = Sprite.DivingTurtles))
      || not (floor (obj.frame_index /. 1000.) = 5.))
    lane_objects
;;

let handle_death_check (game, dt, ({ Temp.lane_collisions; _ } as tmp)) =
  let has_car_collision = List.exists is_car lane_collisions in
  let is_in_water =
    lane_collisions |> reject_under_water_turtles |> List.length = 0
    && get_row_for_y (int_of_float game.frog.rect.y) > 7
    && game.frog.left_in_jump = 0.
  in
  let is_out_of_bounds = is_rect_out_of_bounds game.frog.rect in
  let timer_is_up = game.timer <= 0 in
  let is_dead = has_car_collision || is_in_water || timer_is_up || is_out_of_bounds in
  (* either start an animation, or handle death scenario once animation is over *)
  let new_game =
    match is_dead, Frog.(game.frog.left_in_animation), game.lives with
    | false, None, _ -> game
    | true, None, _ ->
      { game with
        frog = Frog.{ game.frog with left_in_animation = Some frog_animation_length }
      }
    | _, Some 0, 1 -> { game with state = Lost }
    | _, Some 0, n ->
      { game with
        frog = (start_game ~highscore:game.highscore ~input:game.input).frog
      ; timer = start_timer_ms
      ; lives = n - 1
      }
    | _, Some n, _ ->
      { game with frog = { game.frog with left_in_animation = Some (max 0 (n - dt)) } }
  in
  new_game, dt, tmp
;;

let handle_game_win_check (game, dt, tmp) =
  let all_goals_filled = not (List.exists (fun (_, boo) -> not boo) game.endzone) in
  if all_goals_filled then { game with state = Won }, dt, tmp else game, dt, tmp
;;

let handle_endzone_check (game, dt, tmp) =
  let intersects_with_frog (_, rect) = intersects game.frog.rect rect in
  let endzone_collision = find_opt intersects_with_frog endzone_rects in
  match endzone_collision with
  | None -> game, dt, tmp
  | Some (matched_idx, _) ->
    let already_filled = List.assoc matched_idx game.endzone in
    let endzone = List.map (fun (i, curr) -> i, curr || matched_idx = i) game.endzone in
    let new_game =
      if already_filled
      then game
      else
        { game with
          frog = (start_game ~highscore:game.highscore ~input:game.input).frog
        ; timer = start_timer_ms
        ; max_row = 1
        ; score = game.score + 200 + (game.timer / 1000)
        ; endzone
        }
    in
    new_game, dt, tmp
;;

let find_collisions (game, dt, tmp) =
  let collisions =
    List.filter
      (fun (obj : Lane_object.t) -> intersects obj.rect game.frog.rect)
      game.objects
  in
  let new_tmp = Temp.{ tmp with lane_collisions = collisions } in
  game, dt, new_tmp
;;

let handle_score_update (game, dt, tmp) =
  let new_frog_row = get_row_for_y (int_of_float game.frog.rect.y) in
  let score = game.score + if new_frog_row > game.max_row then 10 else 0 in
  let new_game = { game with score; max_row = max new_frog_row game.max_row } in
  new_game, dt, tmp
;;

let shrink_timer (game, dt, tmp) =
  let timer = game.timer - dt in
  let new_game = { game with timer } in
  new_game, dt, tmp
;;

let update_lane_objects (game, dt, tmp) =
  let filter_out_of_bounds =
    List.filter (fun obj -> is_rect_in_bounds Lane_object.(obj.rect))
  in
  let moved_lane_objects =
    List.map (fun o -> update_obj o dt) game.objects |> filter_out_of_bounds
  in
  let spawned_lane_objects =
    List.map
      (fun (row_num, (cfg : Lane_config.t)) ->
        if Temp.(tmp.now) > cfg.next_spawn_time
        then (
          cfg.next_spawn_time
          <- get_jitter_from_now Temp.(tmp.now)
             + int_of_float (abs_float cfg.velocity *. 1000. /. cfg.objects_at_once_ish);
          Some (make_lane_object (row_num, cfg)))
        else None)
      lane_config
    |> deoptionalize
  in
  let objects = moved_lane_objects @ spawned_lane_objects in
  let new_game = { game with objects } in
  new_game, dt, tmp
;;

let get_game (game, _, _) : t = game

let step_game input game now dt : t =
  (game, dt, { lane_collisions = []; now })
  |> find_collisions
  |> update_frog input
  |> update_lane_objects
  |> shrink_timer
  |> handle_death_check
  |> handle_endzone_check
  |> handle_game_win_check
  |> handle_score_update
  |> get_game
;;

let init ~highscore =
  reset_spawn_times 0;
  start_game ~highscore ~input:Input.empty
;;

let step game ~input ~now_ms ~dt_ms =
  let game = { game with input } in
  let next_game = step_game input game now_ms dt_ms in
  let events =
    []
    |> (fun evs ->
         if next_game.score > game.score
         then Scored (next_game.score - game.score) :: evs
         else evs)
    |> fun evs ->
    if next_game.highscore > game.highscore
    then Highscore_updated next_game.highscore :: evs
    else evs
  in
  next_game, List.rev events
;;
