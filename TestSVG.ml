type direction = Up | Down | Left | Right
type style = Default
type point = {x : float; y : float}
(* type rect = {x : float; y : float; width : float; height : float} *)
(* type context = {p : point; dir : direction; s : style}
type cell_info = {clip : rect; base : point * point; vert : point} 
type canvas = {content : BatBuffer.t; vertices_pos : (T.vertex, cell_info) BatMap.t} *)

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

type attr = string * string 
type svg_tree = Node of svg_node_type * attr list * svg_tree list 

let buffer_default_size = 500

let svg_print_deep buff d s =
  Printf.bprintf buff "%s" (String.make d ' ');
  Printf.bprintf buff s 

let svg_init buff =
  Printf.bprintf buff "%s" ("<svg xmlns=\"http://www.w3.org/2000/svg\""
  ^" xmlns:xlink=\"http://www.w3.org/1999/xlink\" viewBox=\"0 0 2 2\">\n")

let svg_finalize buff =
  Printf.bprintf buff "</svg>"

let to_attr_list = List.map2 (fun x y -> (x, string_of_float y))

let attr_list_print buff l =
  List.iter (fun (x,y) -> Printf.bprintf buff " %s=\"%s\"" x y) l

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

let svg_path_str points pos =
  let buff = Buffer.create 50 in
  let l = if pos=Absolute then String.uppercase_ascii else (fun x -> x) in
  let p = Printf.bprintf buff in
  let p0 s = Printf.bprintf buff " %s" (l s) in
  let p1 s x = Printf.bprintf buff " %s %f" (l s) x in
  let p2 s x y = Printf.bprintf buff " %s %f %f" (l s) x y in
  let plist = List.iter (p " %f") in
  let path_printer = function
    MoveTo {x;y} -> p2 "m" x y
    | LineTo {x;y}-> p2 "l" x y
    | HLineTo x -> p1 "h" x 
    | VLineTo y -> p1 "v" y 
    | CurveTo {x1;y1;x2;y2;x;y} ->
        p0 "c";
        plist [x1;y1;x2;y2;x;y]
    | SCurveTo {x2;y2;x;y} -> 
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
  Buffer.contents buff

let rec svg_print_tree ?(d=0) buff (Node(node_type,attrs,children)) = 
  let node_type_str = svg_node_type_str node_type in
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
    | _ -> []
  in
  Printf.bprintf buff "<%s" node_type_str; 
  attr_list_print buff core_attributes;
  attr_list_print buff attrs;
  match children with 
    [] -> Printf.bprintf buff "/>\n"
    | _ ->
      List.iter (fun x -> svg_print_tree ~d:(d+1) buff x) children;
      Printf.bprintf buff "</%s>\n" node_type_str

let barycentre x x' w = 
  w *. x +. (1. -. w) *. x' 

let rec trim_lines ?(accu=[]) w : point list -> point list = function
  p::p'::ps -> 
    let p1 : point = { x = barycentre p.x p'.x w; y = barycentre p.y p'.y w} in
    let p1' : point = { x = barycentre p'.x p.x w; y = barycentre p'.y p.y w} in
    trim_lines ~accu:(p'::p1'::p1::accu) w (p'::ps)
  | _ -> accu

let points_to_poly from lpoints cpoints = 
  let rec corner accu l c = match l,c with
    lp::lps, cp::cps ->
      line ((QuadTo {x1=cp.x; y1=cp.y; x=lp.x; y=lp.y})::accu) lps cps 
    | _ -> accu
  and line accu l c = match l with 
    lp::lps -> corner ((LineTo lp)::accu) lps c 
    | _ -> accu
  in
  Close::(corner [MoveTo from] lpoints cpoints) |> List.rev

let svg_rounded_poly w points =  
  let open List in 
  let points = match points with
    [] -> []
    | p::ps -> p::(rev (p::ps)) in
  let lpoints = trim_lines w points in
  match lpoints with 
    lp::lps ->
      points_to_poly lp (rev lps) points 
    | _ -> []

let () = 
  let b = Buffer.create 50 in
  let points = svg_rounded_poly 0.1 [{x=0.;y=0.};
  {x=1.;y=0.} ; {x=1.; y=1.}] in
  let n = Node(Path {points=points; pos=Absolute}, [], []) in
  svg_init b;
  svg_print_tree b n;
  svg_finalize b;
  Printf.printf "%s" (Buffer.contents b)
