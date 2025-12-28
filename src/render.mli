val magnification : float
val draw_lane_object : Bindings.Canvas.t -> Types.Lane_object.t -> unit
val draw_dying_frog : Bindings.Canvas.t -> Types.Rect.t -> int -> unit
val draw_frog : Bindings.Canvas.t -> Types.Frog.t -> unit
val draw_start_screen : Bindings.Canvas.t -> unit
val draw_win_screen : Bindings.Canvas.t -> unit
val draw_lose_screen : Bindings.Canvas.t -> unit
val draw_goal : Bindings.Canvas.t -> unit
val draw_grass : Bindings.Canvas.t -> int -> unit
val draw_cars : Bindings.Canvas.t -> Types.Lane_object.t list -> unit
val draw_lives : Bindings.Canvas.t -> Game.t -> unit
val draw_timer : Bindings.Canvas.t -> Game.t -> unit
val draw_score : Bindings.Canvas.t -> Game.t -> unit
val draw_completed_endzones : Bindings.Canvas.t -> Game.t -> unit
val draw_bounding_boxes : Bindings.Canvas.t -> Game.t -> unit
val draw_grid : Bindings.Canvas.t -> unit
val draw_background : Bindings.Canvas.t -> unit
val render : Bindings.Canvas.t -> Game.t -> unit
