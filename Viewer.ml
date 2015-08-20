open Net

(* module Viewer = struct *)

module MELLYSToDot =
  struct
    let rec generate_vert_dot output prefix r v = 
      let v_type = MELLYS.vertex_get_type v in
      let id = MELLYS.V.hash v in
      let text = match v_type with
        | MELLYS.One -> "1"
        | MELLYS.Bot -> "_|_"
        | MELLYS.Axiom -> "ax"
        | MELLYS.Cut -> "cut"
        | MELLYS.Par -> "par"
        | MELLYS.Tensor -> "x"
        | MELLYS.Contraction -> "?c"
        | MELLYS.Weakening -> "?w"
        | MELLYS.Dereliction -> "?d"
        | MELLYS.Sync -> "sync"
        | MELLYS.End -> "."
        | _ -> ""
      in
      match v_type with
        | MELLYS.BangBox | MELLYS.BotBox | MELLYS.YBox ->
          generate_box_dot output r v
        | _ ->
          Printf.ksprintf output
            "%s_%d [id=\"%s_%d\",label=\"%s\",shape=\"circle\"];\n"
            prefix id prefix id text

    and generate_box_dot output r v = 
      let id = MELLYS.V.hash v in
      let output' s = output ("  "^s) in
      let r' = MELLYS.box_content r v in  
      let label = match MELLYS.vertex_get_type v with
        | MELLYS.BangBox -> "!"
        | MELLYS.YBox -> "Y"
        | MELLYS.BotBox -> "_|_"
        | _ -> assert false in
      let prefix = Printf.sprintf "cluster_%d_node" id in
      Printf.ksprintf output "subgraph cluster_%d {\n" id;
      Printf.ksprintf output' "label = \"%s\"\n" label;
      MELLYS.iter_vertex (generate_vert_dot output' prefix r') r';
      MELLYS.iter_edges_e (generate_edge_dot output' prefix) r';
      Printf.ksprintf output "}\n"

    and generate_edge_dot output prefix e =
      let generate_port p = function
        | MELLYS.End | MELLYS.Sync | MELLYS.BangBox | MELLYS.BotBox | MELLYS.YBox -> "_"
        | MELLYS.One | MELLYS.Bot | MELLYS.Weakening | MELLYS.Dereliction -> "c"
        | MELLYS.Axiom | MELLYS.Cut -> if p=MELLYS.LeftPort then "w" else "e"
        | MELLYS.Par | MELLYS.Tensor | MELLYS.Contraction ->
          (match p with 
            | MELLYS.MainPort -> "c"
            | MELLYS.LeftPort -> "nw"
            | MELLYS.RightPort -> "ne"
            | _ -> "_"
          )
        in
      let src,dst = MELLYS.E.src e, MELLYS.E.dst e in
      let src_id,dst_id = MELLYS.V.hash src, MELLYS.V.hash dst in
      let src_t = MELLYS.vertex_get_type src in
      let dst_t = MELLYS.vertex_get_type dst in 
      let src_str = match src_t with
        | BangBox | BotBox | YBox ->
          Printf.sprintf "cluster_%d_node_0" src_id
        | _ -> 
          Printf.sprintf "%s_%d" prefix src_id in
      let l = MELLYS.E.label e in
      let tailport = generate_port l.src_port src_t in
      let headport = generate_port l.dst_port dst_t in
      let arrowhead = match src_t,dst_t with
        | (_,MELLYS.End) | (_,MELLYS.Sync) -> "none"
        | _ -> "normal" 
      in
      Printf.ksprintf output "%s -> %s_%d [id=\"edge_%d\",tailport=\"%s\",headport=\"%s\",arrowhead=\"%s\"];\n"
        src_str prefix dst_id (Hashtbl.hash e) tailport headport arrowhead
    
    
    let generate_net_dot output r =
      Printf.ksprintf output "digraph R {\n";
      Printf.ksprintf output "  node_0 [id=node_0,style=invisible];\n";
      let output' s = output ("  "^s) in
      MELLYS.iter_vertex (generate_vert_dot output' "node" r) r;
      MELLYS.iter_edges_e (generate_edge_dot output' "node") r;
      Printf.ksprintf output "}"
  end

(* end *)
