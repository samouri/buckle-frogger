module Rect = struct
  type t =
    { x : float
    ; y : float
    ; width : int
    ; height : int
    }
end

module Frog = struct
  type t =
    { rect : Rect.t
    ; left_in_jump : float
    ; left_in_animation : int option
    ; direction : Direction.t
    }
end

module Sprite = struct
  type t =
    | Car
    | BasicFloater
    | DivingTurtles
end

module Game_state = struct
  type t =
    | Start
    | Playing
    | Won
    | Lost
end

module Sprite_image = struct
  type t =
    { x_start : int
    ; y_start : int
    ; frames : int
    ; frame_speed : float
    ; width : int
    ; height : int
    ; number : int
    }
end

module Lane_object = struct
  type t =
    { rect : Rect.t
    ; frame_index : float
    ; direction : Direction.t
    ; img : Sprite_image.t
    ; velocity : float
    ; obj_type : Sprite.t
    }
end

module World = struct
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
end

module Lane_config = struct
  type t =
    { velocity : float
    ; objects_at_once_ish : float
    ; mutable next_spawn_time : int
    ; obj_type : Sprite.t
    ; img : Sprite_image.t
    }
end

module Temp = struct
  type t =
    { lane_collisions : Lane_object.t list
    ; now : int
    }
end
