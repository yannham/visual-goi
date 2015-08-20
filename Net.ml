(* module Net = struct *)

module MELLYS =
  struct
    type port = LeftPort | RightPort | MainPort | AuxPort of int | EndPort
    type vertex_type =
      | End
      | One
      | Bot
      | Axiom
      | Cut
      | Par
      | Tensor
      | Contraction
      | Weakening
      | Dereliction
      | Sync
      | BangBox
      | BotBox
      | YBox

    type label = {src_port : port; dst_port : port; formula : Logic.MELL.formula}

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
      let default = {src_port=EndPort;dst_port=EndPort;formula=Logic.MELL.Bot}
    end

    module Graphimpl =
      Graph.Imperative.Digraph.ConcreteBidirectionalLabeled(Vertex_t)(Edge_t)
    include Graphimpl
 
    exception EdgeFound of edge

    let count = ref 0
    let boxes = Hashtbl.create 30
    let end_vertex = (0,End)

    let find_edge_pred pred n =
      let f e =
        if (pred e) then
          raise (EdgeFound(e))
        else
          ()
      in
      try
        iter_edges_e f n;
        raise Not_found
      with EdgeFound(e) -> e

    let make_vertex v_t = let () = incr count in (!count,v_t)

    let vertex_get_type v = snd v

    let madd_vertex r v_t = let v = make_vertex v_t in add_vertex r v; v

    let bind_box_content r v r' = Hashtbl.add boxes v r'
    let unbind_box_content r v = Hashtbl.remove boxes v
    let box_content r v = Hashtbl.find boxes v

    let add_vertex_premises_conclusions r v_t prems ccls =
      let v = madd_vertex r v_t in
      let rebranch_premises (e,p) =
        remove_edge_e r e;
        let l = {(E.label e) with dst_port=p} in
        add_edge_e r (E.create (E.src e) l v) in
      let add_conclusions l =
        let e = E.create v l end_vertex in
        add_edge_e r e;
        e in
      List.iter rebranch_premises prems;
      (v,List.map add_conclusions ccls)

    let premises r v =
      let filter e l =
        if (E.dst e)=v then e::l else l
      in fold_edges_e filter r []

    let conclusions r v =
      let filter e l =
        if (E.src e)=v then e::l else l
      in fold_edges_e filter r []

    let net_conclusions r = premises r end_vertex

    let axiom f =
      let r = create () in
      let l = {src_port=LeftPort; dst_port=EndPort;
        formula=Logic.MELL.not_normal_form (Logic.MELL.Not(f))} in
      let l' = {src_port=RightPort; dst_port=EndPort;
        formula=Logic.MELL.not_normal_form f} in
      add_vertex r end_vertex; 
      let v,es = add_vertex_premises_conclusions r Axiom [] [l;l'] in
      match es with
        | [e;e'] -> (r,v,e,e')
        | _ -> assert false

    let one () =
      let r = create () in
      let l = {src_port=MainPort; dst_port=EndPort; formula=Logic.MELL.One} in
      add_vertex r end_vertex;
      let v,_ = add_vertex_premises_conclusions r One [] [l] in
      (r,v)

    let bot () =
      let r = create () in
      let l = {src_port=MainPort; dst_port=EndPort; formula=Logic.MELL.Bot} in
      add_vertex r end_vertex;
      let v,_ = add_vertex_premises_conclusions r Bot [] [l] in
      (r,v)

    let mix r r' =
      let v_adder = function
        | (_,End) -> ()
        | v -> add_vertex r v in
      let e_adder e = add_edge_e r e in
      iter_vertex v_adder r';
      iter_edges_e e_adder r';
      clear r'

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

(* end *)
