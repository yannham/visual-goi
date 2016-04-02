open Batteries
open INet

module MakeDrawer (T : INET) : MakeDrawer_sig = struct
  type canvas
  type direction = Up | Down | Left | Right
  type style = Default
  type context = {x : float; y : float; width : float; height : float;
                  dir : direction; s : style}

  type canvas = {content : BatBuffer.t; vertices_pos : (T.vertex, context) BatMap.t} 

  type attr = string * string

  type svg_tree = Node of string * attr list * svg_tree list 

  type svg_ppos_type = Absolute | Relative

  type svg_path_point = 
    MoveTo of float*float
    | LineTo of float*float
    | HLineTo of float*float
    | VLineTo of float*float
    | CurveTo of float*float*float*float*float*float
    | SCurveTo of float*float*float*float
    | QuadTo of float*float*float*float
    | SQuadTo of float*float
    | EllipticalArc of (float*float*float*float*float*float*float)
    | Close

  let buffer_default_size = 500

  let svg_printf buff d s =
    BatPrintf.bprintf buff "%s" (BatString.make d ' ');
    BatPrintf.bprintf buff s 

  let svg_attr_printer buff d (x,y) = svg_printf buff d " %s=\"%s\"" x y

  let svg_init buff =
    BatPrintf.bprintf buff "<svg xmlns=\"http://www.w3.org/2000/svg\""
    ^"xmlns:xlink=\"http://www.w3.org/1999/xlink\" viewBox=\"0 0 100 100\">\n"

  let svg_finalize buff =
    BatPrintf.bprintf buff "</svg>"

  let rec svg_write buff d = function
    Node(s,attrs,[]) ->
      svg_printf buff d "<%s" s;
      List.iter (svg_attr_printer buff d);
      svg_printf buff d "/>\n"
    | Node(s,attrs,children) ->
      svg_printf buff d "<%s" e;
      List.iter (svg_attr_printer buff d);
      svg_printf buff d ">\n";
      List.iter (svg_write buff (d+1)) 
      svg_printf buff d "</%s>\n" e;



  let init () = 
    let c = {content=BatBuffer.create buffer_default_size; vertices_pos = BatMap.empty} in
    svg_init  c.content;

  let finalize c =
    svg_finalize c.content;

  let vertex_type_to_tex _ = ""

  let to_attr_list = List.map2 (fun x y -> (x, string_of_float y))

  let svg_make_circle ?(attrs=[]) x y r =
    let l = to_attr_list ["cx";"cy";"r"] [x;y;r] in
    Node("circle",(l@attrs),[])

  let svg_make_line ?(attrs=[]) x1 y1 x2 y1 =
    let l = to_attr_list ["x1";"y1";"x2";"y2"] [x1;y1;x2;y2] in
    Node("circle",(l@attrs),[])

  let svg_make_path ?(attrs=[]) points ppos =
    let buffer = BatBuffer.create 50 in
    let l = if ppos=Absolute then BatString.uppercase else (fun x -> x) in
    let p = BatPrintf.bprintf buffer in
    let p2 s x y = p " %s %f %f" (l s) x y in
    let p4 s x1 y1 x2 y2 = p " %s %f %f %f %f" (l s) x1 y1 x2 y2 in
    let p6 s x1 y1 x2 y2 x3 y3 = p " %s %f %f %f %f %f %f" (l s) x1 y1 x2 y2 x3 y3 in
    let p7 s x1 x2 x3 x4 x5 x6 x7 = 
      p " %s %f %f %f %f %f %f %f" (l s) x1 x2 x3 x4 x5 x6 x7 in
    let writer = function
      MoveTo(x,y) -> p2 "m" x y
      | LineTo(x,y) -> p2 "l" x y
      | HLineTo(x,y) -> p2 "h" x y
      | VLineTo(x,y) -> p2 "v" x y
      | CurveTo(x1,y1,x2,y2,x3,y3) -> p6 "c" x1 y1 x2 y2 x3 y3
      | SCurveTo(x1,y1,x2,y2) -> p4 "s" x1 y1 x2 y2
      | QuadTo(x1,y1,x2,y2) -> p4 "s" x1 y1 x2 y2
      | SQuadTo(x,y) -> p2 "t" x y
      | EllipticalArc(x1,x2,x3,x4,x5,x6,x7) ->
        p7 "a" x1 x2 x3 x4 x5 x6 x7 
      | Close -> p " %s" (l "z")
    in
    List.iter writer points;
    Node("path",("d", BatBuffer.content buff)::attrs,[])

end
