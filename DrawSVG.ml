(* open Batteries *)
open INet

module MakeDrawer (T : INET) : MakeDrawer_sig = struct
  type direction = Up | Down | Left | Right
  type style = Default
  type context = {x : float; y : float; width : float; height : float;
                  dir : direction; s : style}

  type canvas = {content : BatBuffer.t; vertices_pos : (T.vertex, context) BatMap.t} 
  
  type point = {x : float; y : float}

  type svg_pos = Absolute | Relative

  type svg_path_point = 
    | MoveTo of point
    | LineTo of point
    | HLineTo of float
    | VLineTo of float
    | CurveTo of { x1 : float; y1 : float; x2 : float; y2 : float;
      x : float; y : float}
    | SCurveTo of {x2 : float; y2 : float; x : float; y : float} 
    | QuadTo of {x1 : float; y1 : float; x : float; y : float}
    | SQuadTo of point 
    | EllipticalArc of {rx : float; ry : float; x_axis_rotation : float;
      large_arc_flag : float; sweep_flag : float; x : float; y : float}
    | Close

  type svg_node_type =
    | Rect of {x : float; y : float; width : float; height : float}
    | Circle of {cx : float; cy : float; r : float}
    | Ellipse of {cx : float; cy : float; rx : float; ry : float}
    | Line of {x1 : float; y1 : float; x2 : float; y2 : float}
    | Polyline of point list
    | Polygon of point list 
    | Path of {points : svg_path_point list; pos : svg_pos}
    | Group 
    | Other of string

  type svg_tree = Node of svg_node_type * attr list * svg_tree list 
  
  let buffer_default_size = 500

  let svg_print_deep buff d s =
    BatPrintf.bprintf buff "%s" (BatString.make d ' ');
    BatPrintf.bprintf buff s 

  let svg_init buff =
    BatPrintf.bprintf buff "<svg xmlns=\"http://www.w3.org/2000/svg\""
    ^"xmlns:xlink=\"http://www.w3.org/1999/xlink\" viewBox=\"0 0 100 100\">\n"

  let svg_finalize buff =
    BatPrintf.bprintf buff "</svg>"
  
  let init () = 
    let c = {content=BatBuffer.create buffer_default_size; vertices_pos = BatMap.empty} in
    svg_init c.content;

  let finalize c =
    svg_finalize c.content;

  let vertex_type_to_tex _ = ""

  let to_attr_list = List.map2 (fun x y -> (x, string_of_float y))

  let attr_list_print buff l =
    Lister.iter (fun (x,y) -> Printf.bprintf buff " %s=\"%s\"" x y) l

  let svg_node_type_str = function
    Rect _ -> "rect"
    | Circle _ -> "circle"
    | Ellipse _ -> "ellipse"
    | Line _ -> "line"
    | Polyline _ -> "polyline"
    | Polygon _ -> "polygon"
    | Path _ -> "path"
    | Group -> "group"
    | Other s -> s

  let rec svg_print_tree ?(d=0) buff Node(node_type,attrs,children) = 
    let printer = svg_print_deep buff
    let node_type_str = svg_node_type_str node_type 
    printer "<%s" node_type_str 
    let core_attributes = 
    match node_type with 
      Rect {x;y;width;height} ->
        to_attr_list ["x";"y";"width";"height"] [x;y;width;height]
      | Circle {cx;cy;r;} ->
        to_attr_list ["cx";"cy";"r"] [cx;cy;r]
      | Ellipse {cx;cy;rx;ry} ->
        to_attr_list ["cx";"cy";"rx";"ry"] [cx;cy;rx;ry]
      | Line {x1;y1;x2;y2} ->
        to_attr_list ["x1";"y1";"x2";"y2"] [x1;y1;x2;y2]
      | Polyline points ->
        (* TODO *)
        []
      | Polygon points ->
        (* TODO *)
        []
      | Path {points;pos} ->
        [("d",svg_path_str points pos)] 
    in
    attr_list_print buff core_attributes;
    attr_list_print buff attrs;
    match children with 
      [] -> printer "/>\n"
      | _ ->
        Lister.iter (svg_print_tree (d+1) buff)
        printer "</%s>\n" node_type_str

  let svg_path_str points pos =
    let buff = BatBuffer.create 50 in
    let l = if pos=Absolute then BatString.uppercase else (fun x -> x) in
    let p = BatPrintf.bprintf buff in
    let p0 s = printer " %s" (l s) in
    let p1 s x = printer " %s %f" (l s) x in
    let p2 s x y = printer " %s %f %f" (l s) x y in
    let plist = List.iter (printer " %f") in
    let path_printer = function
      MoveTo {x;y} -> p2 "m" x y
      | LineTo {x;y}-> p2 "l" x y
      | HLineTo x -> p1 "h" x 
      | VLineTo y -> p1 "v" y 
      | CurveTo {x1;y1;x2;y2;x;y} ->
          p0 "c";
          plist [x1;y1;x2;y2;x;y]
      | SCurveTo {x2;y2;x2;y2} -> 
          p0 "s";
          plist [x2;y2;x;y] 
      | QuadTo {x1;y1;x;y} -> 
          p0 "q";
          plist [x1;y1;x;y]
      | SQuadTo {x;y} -> p2 "t" x y
      | EllipticalArc {rx;ry;x_axis_rotation;large_arc_flag;sweep_flag;x;y} ->
        p0 "a";
        plist [rx;ry;x_axis_rotation;large_arc_flag;sweep_flag;x;y]
      | Close -> p0 "z"
    in
    List.iter path_printer points;
    BatBuffer.content buff
end
