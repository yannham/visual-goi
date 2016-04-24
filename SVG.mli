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

type attr = string * string
type tree = Node of node_type * attr list * tree list 

val init : out_channel -> unit
val finalize : out_channel -> unit

val path_data : path_point list -> pos -> string  
val print_tree : ?d:int -> out_channel -> tree -> unit

val rounded_poly : float -> point list -> path_point list
