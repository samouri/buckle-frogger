type t = Types.worldT

type event =
  | Start
  | Reset
  | Scored of int
  | Highscore_updated of int

val frog_animation_length : int
val start_timer_ms : int
val endzone_rects : (int * Types.rectT) list

val init : highscore:int -> t
val step : t -> input:Input.t -> now_ms:int -> dt_ms:int -> t * event list
