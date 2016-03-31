(* module Net = struct *)

module MakeINet : MakeINet_sig = struct
    type port = AuxPort of int | MainPort
    type label = {src_port : port; dst_port : port; l : T.edge_label}
    type vertex_type = T.vertex_type

    exception InvalidNetException of string

    module Vertex_t = struct
      type t = int * vertex_type
      let compare v v' = compare (fst v) (fst v')
      let hash = fst 
      let equal = (=)
    end

    module Edge_t = struct
      type t = label
      let hash = Hashtbl.hash
      let compare e e' = compare (hash e) (hash e')
      let default = {src_port=AuxPort(0);dst_port=AuxPort(0);l=T.default}
    end

    module Graphimpl =
      Graph.Imperative.Digraph.ConcreteBidirectionalLabeled(Vertex_t)(Edge_t)
    include Graphimpl
 
    exception EdgeFound of edge

    let count = ref 0
    let boxes = Hashtbl.create 30
    let end_vertex = (0,End)

    let find_edge_p_ iterator pred net =
      let f e = 
        if (pred e) then
          raise (EdgeFound(e))
        else
          ()
      in
      try
        iterator f net;
        raise Not_found
      with EdgeFound(e) -> e

    let make_vertex v_t = let () = incr count in (!count,v_t)

    let vertex_get_type v = snd v

    let madd_vertex net v_t = let v = make_vertex v_t in add_vertex net v; v

    let bind_box_content net v net' = Hashtbl.add boxes v net'
    let unbind_box_content net v = Hashtbl.remove boxes v
    let box_content net v = Hashtbl.find boxes v

    let add_vertex_premises_conclusions net v_t prems ccls =
      let v = madd_vertex net v_t in
      let rebranch_premises (e,p) =
        remove_edge_e net e;
        let l = {(E.label e) with dst_port=p} in
        add_edge_e net (E.create (E.src e) l v) in
      let add_conclusions l =
        let e = E.create v l end_vertex in
        add_edge_e net e;
        e in
      List.iter rebranch_premises prems;
      (v,List.map add_conclusions ccls)

    let premises net v =
      let filter_succ e l = match E.label e with 
        {src_port=AuxPort(_)} -> e::l
        | _ -> l in
      let filter_pred e l = match E.label e with
        {dst_port=AuxPort(_)} -> e::l
        | _ -> l in
      let l = fold_succ_e filter_succ net [] in
      fold_pred_e filter_pred net l

    let conclusion net v =
      let filter_succ e a = match E.label e with 
        {src_port=MainPort} -> e::l
        | _ -> l in
      let filter_pred e l = match E.label e with
        {dst_port=MainPort} -> e::l
        | _ -> l in
      let l = fold_succ_e filter_succ net [] in
      let l = fold_pred_e filter_pred net l in
      match l with 
         e::es -> e
        | [] -> raise (Not_found)

    let net_conclusions net = (premises net end_vertex)

    let merge net net' =
      let v_adder = function
        | (_,End) -> ()
        | v -> add_vertex net v in
      let e_adder e = add_edge_e net e in
      iter_vertex v_adder net';
      iter_edges_e e_adder net';
      clear net'
  end


(*
    let tensor r e r' e' =
      mix r r';
      let l = {src_port=MainPort; dst_port=EndPort;
        formula=Logic.MELL.Tensor((E.label e).formula, (E.label e').formula)} in
      match add_vertex_premises_conclusions r
        Tensor [(e,LeftPort);(e',RightPort)] [l] with
        | (v,[ccl]) -> (v,ccl)
        | _ -> assert false

    let par r e e' =
      let l = {src_port=MainPort; dst_port=EndPort;
        formula=Logic.MELL.Par((E.label e).formula, (E.label e').formula)} in
      match add_vertex_premises_conclusions r
        Par [(e,LeftPort);(e',RightPort)] [l] with
        | (v,[ccl]) -> (v,ccl)
        | _ -> assert false

    let cut r e r' e' =
      mix r r';
      match add_vertex_premises_conclusions r
        Cut [(e,LeftPort);(e',RightPort)] [] with
        | (v,[]) -> v
        | _ -> assert false

    let der r e =
      let l = {src_port=MainPort; dst_port=EndPort;
        formula=Logic.MELL.Whynot((E.label e).formula)} in
      match add_vertex_premises_conclusions r Dereliction [(e,MainPort)] [l] with
        | (v,[ccl]) -> (v,ccl)
        | _ -> assert false

    let weak r f =
      let l = {src_port=MainPort; dst_port=EndPort; formula=f} in
      match add_vertex_premises_conclusions r Weakening [] [l] with
        | (v,[ccl]) -> (v,ccl)
        | _ -> assert false

    let bang r e =
      let r' = create () in
      let fold_labels (i,ls) e' =
        if e'=e then
          (i,ls)
        else
          let l = {(E.label e') with src_port=AuxPort(i); dst_port=EndPort} in
          (i+1,l::ls) in
      let l = {(E.label e) with src_port=MainPort; dst_port=EndPort} in
      let r_ccls = List.filter ((<>) e) (net_conclusions r) in
      let ccls = l::(snd (List.fold_left fold_labels (1,[]) (List.rev r_ccls))) in
      add_vertex r' end_vertex;
      match add_vertex_premises_conclusions r' BangBox [] ccls with
        | (v,e::es) ->
          bind_box_content r' v r;
          (r',v,e, List.combine r_ccls es)
        | _ -> assert false

    let cntr r e e' =
      let l = {src_port=MainPort; dst_port=EndPort;
        formula=(E.label e).formula} in
      match add_vertex_premises_conclusions r Cut [(e,MainPort)] [l] with
        | (v,[ccl]) -> (v,ccl)
        | _ -> assert false

    let sync r edges =
      let set_ports i e =
        (e,AuxPort(i)) in
      let ccl_label i e =
        {(E.label e) with src_port=AuxPort(i); dst_port=EndPort} in
      let edges_p = List.mapi set_ports edges in
      let ccl_l = List.mapi ccl_label edges in
      add_vertex_premises_conclusions r Sync edges_p ccl_l
  end
*)

module MELLInteractionNet : REWRITABLE_INET = struct
  module MELLINetType : INET_TYPE = struct
    type vertex_type =
      | End
      | One
      | Bot
      | Par
      | Tensor
      | Contraction
      | Weakening
      | Dereliction
      | Promotion
    type label = Logic.MELL.formula
    let default = Logic.MELL.Bot 
  end

  module INetBase = MakeINet(MELLINetType)
  include INetBase

  let is_positive = function
    One | Tensor | Promotion -> true
    | _ -> false

  let is_redex e =
    match (vertex_get_type (E.src e), vertex_get_type (E.dst e)) with
      (Bot,One) | (Par,Tensor) | (Contraction,Promotion)
      | (Weakening,Promotion) | (Dereliction,Promotion) -> true
      | _ -> false

  let reduce net e =
    let src,dst = E.src e, E.dst e in
    let src_type,dst_type = vertex_get_type src, vertex_get_type dst in
    let ordered_premises v = match pred_e net v with 
      [x;y] when (E.label x).dst_port = AuxPort(0) -> x,y
      | [y;x] when (E.label x).dst_port = AuxPort(0) -> x,y 
      | _ -> assert false in
    let aux_reduce src dst src_type dst_type = 
      match (src_type,dst_type) with
        (Bot,One) -> 
          remove_vertex src;
          remove_vertex dst
        | (Par,Tensor) ->
          let e1,e2 = ordered_premises src in
          let e1',e2' = ordered_premises dst in
          remove_vertex src;
          remove_vertex dst;
          let (e1,e1') =
            if is_positive (E.src e1) then (e1',e1) else (e1,e1') in
          let (e2,e2') =
            if is_positive (E.src e2) then (e2',e2) else (e2,e2') in
          let new_e1 = 
              E.create (E.src e1) (E.label e1') (E.src e1') in
          let new_e2 =
              E.create (E.src e2) (E.label e2') (E.src e2') in
          add_edge_e new_e1;
          add_edge_e new_e2
        | (Contraction,Promotion) when (E.label e).dst_port = AuxPort(0) -> 
          let content = box_content net dst in
          let shift = function (_,v_t) -> let () = incr count in (!count,v_t) in
          let content' = map_vertex shift (copy content)  in
          let finder 
          merge net content;
          merge net content';


    
(* end *)
