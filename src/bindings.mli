module Image : sig
  type t

  val create : string -> t
end

module Canvas : sig
  type t

  (* Find a canvas element by id and capture its 2d context. *)
  val create : string -> t option

  val set_dimensions : t -> width:int -> height:int -> unit
  val set_max_size : t -> width:int -> height:int -> unit

  val set_fill_style : t -> string -> unit
  val set_stroke_style : t -> string -> unit
  val set_font : t -> string -> unit

  val fill_rect : t -> x:float -> y:float -> w:float -> h:float -> unit
  val stroke_rect : t -> x:float -> y:float -> w:float -> h:float -> unit
  val fill_text : t -> x:float -> y:float -> string -> unit

  val draw_image :
    t ->
    Image.t ->
    source_x:float ->
    source_y:float ->
    source_w:float ->
    source_h:float ->
    dx:float ->
    dy:float ->
    d_w:float ->
    d_h:float ->
    unit

  val begin_path : t -> unit
  val move_to : t -> x:float -> y:float -> unit
  val line_to : t -> x:float -> y:float -> unit
  val stroke : t -> unit
end

module Window : sig
  val request_animation_frame : (float -> unit) -> unit
  val on_load : (unit -> unit) -> unit
end

module Console : sig
  val log : string -> unit
end
