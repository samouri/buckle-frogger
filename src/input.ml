type t =
  { direction : Direction.t option
  ; bbox : bool
  ; grid : bool
  }

let empty = { direction = None; bbox = false; grid = false }
let set_direction t direction = { t with direction = Some direction }
let clear_direction t = { t with direction = None }
let toggle_bbox t = { t with bbox = not t.bbox }
let toggle_grid t = { t with grid = not t.grid }
