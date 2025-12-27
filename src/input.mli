type t = {
  direction : Direction.t option;
  bbox : bool;
  grid : bool;
}

val empty : t
val set_direction : t -> Direction.t -> t
val clear_direction : t -> t
val toggle_bbox : t -> t
val toggle_grid : t -> t
