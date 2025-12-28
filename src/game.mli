open Types

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

val frog_animation_length : int
val start_timer_ms : int
val endzone_rects : (int * Types.Rect.t) list
val init : highscore:int -> t
val step : t -> input:Input.t -> now_ms:int -> dt_ms:int -> t * event list
