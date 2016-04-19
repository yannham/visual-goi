type point = {x : float; y : float}

type pos = Absolute | Relative

type path_point = 
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

type node_type =
  | Rect of {x : float; y : float; width : float; height : float}
  | Circle of {cx : float; cy : float; r : float}
  | Ellipse of {cx : float; cy : float; rx : float; ry : float}
  | Line of {x1 : float; y1 : float; x2 : float; y2 : float}
  | Polyline of point list
  | Polygon of point list 
  | Path of {points : path_point list; pos : pos}
  | Group 
  | Other of string

type tree = Node of node_type * attr list * tree list 

let print_at_depth out d s =
  BatPrintf.fprintf out "%s" (BatString.make (2*d) ' ');
  BatPrintf.fprintf out s 

let init out =
  BatPrintf.fprintf out "<svg xmlns=\"http://www.w3.org/2000/svg\""
  ^"xmlns:xlink=\"http://www.w3.org/1999/xlink\" viewBox=\"0 0 100 100\">\n"

let finalize out =
  BatPrintf.fprintf out "</svg>"

let to_attr_list = List.map2 (fun x y -> (x, string_of_float y))

let print_attr_list out l =
  Lister.iter (fun (x,y) -> Printf.fprintf out " %s=\"%s\"" x y) l

let node_type_str = function
  Rect _ -> "rect"
  | Circle _ -> "circle"
  | Ellipse _ -> "ellipse"
  | Line _ -> "line"
  | Polyline _ -> "polyline"
  | Polygon _ -> "polygon"
  | Path _ -> "path"
  | Group -> "group"
  | Other s -> s

let rec print_tree ?(d=0) out Node(node_type,attrs,children) = 
  let printer = print_at_depth out d
  let node_type_str = node_type_str node_type 
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
      [("d",path_data points pos)] 
  in
  print_attr_list out core_attributes;
  print_attr_list out attrs;
  match children with 
    [] -> printer "/>\n"
    | _ ->
      Lister.iter (print_tree (d+1) out)
      printer "</%s>\n" node_type_str

let path_data points pos =
  let out = BatBuffer.create 50 in
  let l = if pos=Absolute then BatString.uppercase else (fun x -> x) in
  let p = BatPrintf.fprintf out in
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
  BatBuffer.content out

