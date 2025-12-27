open Js_of_ocaml
open Bindings
open Types

(* function composition *)
let ( << ) f g x = f (g x)

let isSome = function
  | Some _ -> true
  | None -> false
;;

let deoptionalize lst =
  List.filter isSome lst
  |> List.map (function
    | Some x -> x
    | None -> assert false)
;;

(* define an infix operator to create a range between numbers. WTF this is crazy *)
let ( <-> ) i j =
  let rec aux n acc = if n < i then acc else aux (n - 1) (n :: acc) in
  aux j []
;;

let rec repeat s n = if n = 0 then "" else s ^ repeat s (n - 1)

let padWithZeros (str : string) (n : int) =
  let strlen = String.length str in
  if strlen >= n then str else repeat "0" (n - strlen) ^ str
;;

let find_opt f lst =
  try Some (List.find f lst) with
  | Not_found -> None
;;

(* let height = 256;; (* original was 224 x 256 *)
   let width = 224;; *)

(* Frogger had a 14:16 ratio, so lets stick with that and scale at the render step *)
let height = 480
let width = 420
let rows = 16
let cols = 14
let tileSize = height / rows
let halfTileSize = tileSize / 2
let get_row_for_y y = (height - y) / tileSize
let get_y_for_row row = height - (row * tileSize)

let intersects (rect1 : Rect.t) (rect2 : Rect.t) =
  let bottom1 = rect1.y +. float_of_int rect1.height in
  let bottom2 = rect2.y +. float_of_int rect2.height in
  let top1 = rect1.y in
  let top2 = rect2.y in
  let left1 = rect1.x in
  let left2 = rect2.x in
  let right1 = rect1.x +. float_of_int rect1.width in
  let right2 = rect2.x +. float_of_int rect2.width in
  not (bottom1 < top2 || top1 > bottom2 || right1 < left2 || left1 > right2)
;;

let isRectOutOfBounds (rect : Rect.t) =
  let x = int_of_float rect.x in
  let y = int_of_float rect.y in
  x + rect.width < 0 || x > width || y + rect.height < 0 || y > height - tileSize
;;

let isRectInBounds = not << isRectOutOfBounds

let now_ms () : float =
  let global = Js.Unsafe.global in
  let perf : Js.Unsafe.any Js.optdef = Js.Unsafe.get global "performance" in
  let value =
    if Js.Optdef.test perf
    then (
      let perf_obj = Js.Optdef.get perf (fun () -> assert false) in
      Js.Unsafe.meth_call perf_obj "now" [||])
    else (
      let date = Js.Unsafe.get global "Date" in
      Js.Unsafe.meth_call date "now" [||])
  in
  Js.float_of_number value
;;

let create_image = Image.create
let sprite_sheet = create_image "assets/frogger_sprites2.png"
let goal_sprite = create_image "assets/goal_frog_0.png"
let life_sprite = create_image "assets/life.png"

let make_sprite_image
  ?(number = 1)
  ?(height = 30)
  x_start
  y_start
  frames
  frame_speed
  width
  =
  { Sprite_image.x_start; y_start; frames; frame_speed; width; height; number }
;;

let yellow_car_image = make_sprite_image 80 262 0 0. 33
let green_car_image = make_sprite_image 70 296 0 0. 33
let pink_car_image = make_sprite_image 10 262 0 0. 31
let race_car_image = make_sprite_image 40 260 0 0. 33
let white_truck_image = make_sprite_image 110 296 0 0. 43
let three_turtle_image = make_sprite_image ~number:3 15 402 3 2. 35
let diving_three_turtles = make_sprite_image ~number:3 15 402 6 2. 36
let two_turtle_image = make_sprite_image ~number:2 15 402 3 2. 35
let diving_two_turtles = make_sprite_image ~number:2 15 402 6 2. 36
let small_log_image = make_sprite_image 10 225 0 0. 80
let medium_log_image = make_sprite_image 10 193 0 0. 115
let big_log_image = make_sprite_image 10 162 0 0. 175
let frog_up = make_sprite_image ~height:23 8 370 2 20. 28
let frog_down = make_sprite_image ~height:23 76 370 2 20. 28
let frog_left = make_sprite_image ~height:28 76 336 2 20. 33
let frog_right = make_sprite_image ~height:28 8 336 2 20. 33
