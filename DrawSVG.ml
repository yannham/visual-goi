(* open Batteries *)
open INet

module SVG = struct
  end

module MakeDrawer (T : INET) : MakeDrawer_sig = struct
  type direction = Up | Down | Left | Right
  type style = Default
  type context = {x : float; y : float; width : float; height : float;
                  dir : direction; s : style}

  type canvas = {content : BatBuffer.t; vertices_pos : (T.vertex, context) BatMap.t} 
 end
