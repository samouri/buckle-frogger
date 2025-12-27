open Js_of_ocaml

module Image = struct
  type t = Dom_html.imageElement Js.t

  let create src =
    let image = Dom_html.createImg Dom_html.document in
    image##.src := Js.string src;
    image
end

module Canvas = struct
  type t =
    { canvas : Dom_html.canvasElement Js.t
    ; ctx : Dom_html.canvasRenderingContext2D Js.t
    }

  let num = Js.number_of_float

  let create id =
    match Dom_html.getElementById_coerce id Dom_html.CoerceTo.canvas with
    | None -> None
    | Some canvas ->
      let ctx = canvas##getContext Dom_html._2d_ in
      Some { canvas; ctx }

  let set_dimensions t ~width ~height =
    t.canvas##.width := width;
    t.canvas##.height := height

  let set_max_size t ~width ~height =
    t.canvas##.style##.maxWidth := Js.string (Printf.sprintf "%dpx" width);
    t.canvas##.style##.maxHeight := Js.string (Printf.sprintf "%dpx" height)

  let set_fill_style t color = t.ctx##.fillStyle := Js.string color
  let set_stroke_style t color = t.ctx##.strokeStyle := Js.string color
  let set_font t font = t.ctx##.font := Js.string font

  let fill_rect t ~x ~y ~w ~h = t.ctx##fillRect (num x) (num y) (num w) (num h)
  let stroke_rect t ~x ~y ~w ~h = t.ctx##strokeRect (num x) (num y) (num w) (num h)
  let fill_text t ~x ~y text = t.ctx##fillText (Js.string text) (num x) (num y)

  let draw_image t image ~source_x ~source_y ~source_w ~source_h ~dx ~dy ~d_w
      ~d_h =
    t.ctx##drawImage_full image (num source_x) (num source_y) (num source_w)
      (num source_h) (num dx) (num dy) (num d_w) (num d_h)

  let begin_path t = t.ctx##beginPath
  let move_to t ~x ~y = t.ctx##moveTo (num x) (num y)
  let line_to t ~x ~y = t.ctx##lineTo (num x) (num y)
  let stroke t = t.ctx##stroke
end

module Window = struct
  let request_animation_frame f =
    let cb ts = f (Js.float_of_number ts) in
    ignore (Dom_html.window##requestAnimationFrame (Js.wrap_callback cb))

  let on_load f =
    let run () = f () in
    let handler _ =
      run ();
      Js._false
    in
    (match Js.to_string Dom_html.document##.readyState with
     | "complete" | "interactive" -> run ()
     | _ ->
       Dom_html.window##.onload := Dom_html.handler handler)
end

module Console = struct
  let log message = Console.console##log (Js.string message)
end
