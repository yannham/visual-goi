
module type MakeDrawer_sig =
  functor (T : INET) -> sig
    type canvas
    type direction = Up | Down | Left | Right 
    type style = Default
    type context = {x : float; y : float; width : float; height : float;
    dir : direction; s : style}

    val init : unit -> canvas

    val cell_preferred_size : float * float
    val box_preferred_size : T.t -> float * float
    val net_preferred_size : T.t -> float * float

    val draw_cell : canvas -> context -> T.vertex_type -> (canvas,context)
    val draw_edge : canvas -> context -> T.edge -> (canvas,context)
    val draw_net : canvas -> context -> T.t -> (canvas,context)
    val draw_box : canvas -> context -> T.t -> (canvas,context)

    val to_svg : canvas -> string
    val write_to : canvas -> (string -> unit) -> unit
end
