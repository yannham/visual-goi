open Lambda 
open Logic
open Net

(* module Translate = struct *)

let out_key = '#'

module UntypedLinearToMELLYS = 
  struct
    (* let finder_edge_var v e =
      match (MELLYS.E.src e),(MELLYS.E.label e),
            (MELLYS.E.dst e) with 
        | src,{formula=MELL.Not(MELL.Atom(_))},dst
          when (src=v && dst=MELLYS.end_vertex) -> true
        | _ -> false
        
    let finder_edge_out e =
      match (MELLYS.E.label e),(MELLYS.E.dst e) with
        | {formula=MELL.Not(MELL.Atom(_))},dst
          when dst=MELLYS.end_vertex -> false
        | _,dst when dst=MELLYS.end_vertex -> true
        | _ -> false
    *)

   let rec translate ctxt = function
      | Untyped.Var(x) -> 
        let r,v,ax_e_neg,ax_e_out = MELLYS.axiom (MELL.Atom(Char.escaped x)) in 
        Hashtbl.add ctxt x ax_e_neg;
        Hashtbl.add ctxt out_key ax_e_out;
        r 
      | Untyped.Const(_) -> 
        let r,_ = MELLYS.one () in
        let e_out = (match (MELLYS.net_conclusions r) with
          | [e] -> e
          | _ -> assert false) in
        Hashtbl.add ctxt out_key e_out;
        r
      | Untyped.Abs(x,t) ->
        let r = translate ctxt t in
        let e_var = (try Hashtbl.find ctxt x 
          with Not_found -> 
            failwith 
              (Printf.sprintf "Variables must be linearly bound, but %c is weakened" x)) in 
        let e_out = Hashtbl.find ctxt out_key in
        let _,e_out = MELLYS.par r e_var e_out in
        Hashtbl.replace ctxt out_key e_out;
        r 
      | Untyped.App(t1,t2) ->
        let ctxt' = Hashtbl.create 26 in
        let r1 = translate ctxt t1 in
        let r2 = translate ctxt' t2 in
        let ax,_,ax_e_neg,ax_e_out = MELLYS.axiom (MELL.Atom("app")) in
        let r1_e_out = Hashtbl.find ctxt out_key in
        let r2_e_out = Hashtbl.find ctxt' out_key in 
        let _,r2_e_out = MELLYS.tensor r2 r2_e_out ax ax_e_neg in 
        let _ = (MELLYS.cut r1 r1_e_out r2 r2_e_out) in
        let merger key value = Hashtbl.add ctxt key value in
        Hashtbl.iter merger ctxt';
        Hashtbl.replace ctxt out_key ax_e_out;
        r1

    let net_of_term t =
      let ctxt = Hashtbl.create 26 in
      translate ctxt t
  end

module SimplyTypedToMELLYS_CBN =
  struct
    (* let finder_edge_var v e =
      match (MELLYS.E.src e),(MELLYS.E.label e),
            (MELLYS.E.dst e) with 
        | src,{formula=MELL.Whynot(_)},dst
          when (src=v && dst=MELLYS.end_vertex) -> true
        | _ -> false
        
    let finder_edge_out e =
      match (MELLYS.E.label e),(MELLYS.E.dst e) with
        | {formula=MELL.Whynot(_)},dst
          when dst=MELLYS.end_vertex -> false
        | _,dst when dst=MELLYS.end_vertex -> true
        | _ -> false
    *)

    let finder_formula f e = (MELLYS.E.label e)=f

    let rec stype_to_formula = function 
      | SimplyTyped.Unit -> MELL.One
      | SimplyTyped.Arrow(t, t') ->
        let f,f' = stype_to_formula t, stype_to_formula t' in
        MELL.Par(MELL.Not(f), f')

   let rec translate ctxt = function    
      | SimplyTyped.TVar(x,t) -> 
        let f = stype_to_formula t in
        let r,v,ax_e_neg,ax_e_out = MELLYS.axiom f in
        let _,ax_e_neg = MELLYS.der r ax_e_neg in 
        Hashtbl.add ctxt x ax_e_neg;
        Hashtbl.add ctxt out_key ax_e_out; 
        r
      | SimplyTyped.TConst(i) ->
        let r,_ = MELLYS.one () in
        let e_out = (match (MELLYS.net_conclusions r) with
          | [e] -> e
          | _ -> assert false) in
        Hashtbl.add ctxt out_key e_out;
        r
      | SimplyTyped.TAbs(x,t,m) ->
        let r = translate ctxt m in
        let e_var = (try Hashtbl.find ctxt x 
          with Not_found ->
             snd (MELLYS.weak r (stype_to_formula t))) in
        let e_out = Hashtbl.find ctxt out_key in 
        let _,e_out = MELLYS.par r e_var e_out in
        Hashtbl.replace ctxt out_key e_out;
        Hashtbl.remove ctxt x;
        r
      | SimplyTyped.TApp(m,n) -> 
        let ctxt' = Hashtbl.create 26 in
        let f,f' = (match (SimplyTyped.type_of_term m) with
          | Arrow(t,t') when t=(SimplyTyped.type_of_term n) -> 
            stype_to_formula t, stype_to_formula t'
          | _ -> raise (Invalid_argument("Ill-typed application"))) in
        let r1 = translate ctxt m in
        let r2 = translate ctxt' n in
        let r1_e_out = Hashtbl.find ctxt out_key  in
        let r2_e_out = Hashtbl.find ctxt' out_key in
        let ax,_,ax_e_neg,ax_e_out = MELLYS.axiom f' in
        let r2,v_bang,r2_e_out,ccls_assoc = MELLYS.bang r2 r2_e_out in
        let _,r2_e_out = MELLYS.tensor r2 r2_e_out ax ax_e_neg in
        let _ = MELLYS.cut r1 r1_e_out r2 r2_e_out in
        let replace_vars_edges x e =
          if x<>out_key then
            let e = List.assoc e ccls_assoc in
            try
              let e' = Hashtbl.find ctxt x in
              let _,e_new = MELLYS.cntr r1 e' e in
              Hashtbl.replace ctxt x e_new;                 
            with Not_found ->
              Hashtbl.add ctxt x e
        in Hashtbl.iter replace_vars_edges ctxt';
        Hashtbl.replace ctxt out_key ax_e_out;
        r1

   let net_of_term m = translate (Hashtbl.create 26) m 
  end


module SimplyTypedToMELLYS_CBV =
  struct
    let net_of_term m = failwith "Not implemented"
  end

(* end *)
