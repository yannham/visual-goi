open Net

module type AbstractMachine = sig
  type state
  type term
  type code

  exception NoTransitionException of state

  val compile : term -> code
  val init : term -> state
  val transition : term * state -> state
  val is_final : term * state -> bool
end

module SIAM : sig
    type exp_sign =
      | Const 
      | Left of exp_sign
      | Right of exp_sign 
      | Pack of exp_sign * exp_sign
      | Y of exp_sign * exp_sign

    type stack_elt = Left | Right | Signature of exp_sign | Delta
    type stack = stack_elt list
    type dir = Up | Down | Stable

    type term = MELLYS.t 
    type code = term
    type token = {pos : MELLYS.edge; dir : dir; s : stack; e :
      exp_sign list}
    type token_state = (MELLYS.vertex * term) list * token
    type state = token_state list

    exception NoTransitionException of term * token_state
    exception IncorrectStateException of term * token_state 
 
    val token_transition : code -> token_state -> token_state
    val transition : code -> state -> state 
    val is_initial_token : token_state -> bool
    val is_final_token : token_state -> bool
    val is_initial : state -> bool
    val is_final : state -> bool

end = 
struct
    type exp_sign =
      | Const 
      | Left of exp_sign
      | Right of exp_sign 
      | Pack of exp_sign * exp_sign
      | Y of exp_sign * exp_sign

    type stack_elt = Left | Right | Signature of exp_sign | Delta
    type stack = stack_elt list
    type dir = Up | Down | Stable

    type term = MELLYS.t
    type code = term
    type token = {pos : MELLYS.edge; dir : dir; s : stack; e :
      exp_sign list}
    type token_state = (MELLYS.vertex * term) list * token
    type state = token_state list

    exception NoTransitionException of term * token_state
    exception IncorrectStateException of term * token_state 
   
    (* let compile t = [(-1,t)] *)

    let is_initial_token (bs,tkn) = match (bs, MELLYS.E.dst t.pos, t.dir) with 
      | [],MELLYS.end_vertex,Up -> true
      | _ -> false

    let is_final_token (bs,tkn) = match (bs, MELLYS.E.dst t.pos, t.dir) with 
      | [],MELLYS.end_vertex,Down -> true
      | _ -> false

    let is_initial = List.for_all is_initial_token
    let is_final = List.for_all is_final_token

    let token_transition term (stack,tkn) = 
      let t = match stack with 
        | [] -> term 
        | (_,t)::ts -> t in
      let source,dest = MELLYS.E.src tkn.pos, MELLYS.E.dst tkn.pos in
      let src_prems,src_concl = MELLYS.premises t source, MELLYS.conclusions t source in
      let dst_prems,dst_concl = MELLYS.premises t dest, MELLYS.conclusions t dest in
      match tkn.dir, MELLYS.vertex_type source, MELLYS.vertex_type dest with
        | Up,Axiom,_ -> 
          ( 
          try
            let new_pos = List.find ((<>) tkn.pos) src_concl in
            (term, (stack,{tkn with pos=new_pos; dir=Down}))
          with Not_Found ->
            raise (MELLYS.IllTypedNetException("An axiom node must have exactly"
            ^" two conclusions",t))
          )
        | Down,_,Cut->
          (
          try
            let new_pos = List.find((<>) e) dst_prems in
            (term, (stack,{tkn with pos=new_pos; dir=Up}))
          with Not_Found ->
            raise (MELLYS.IllTypedNetException("An cut node must have exactly"
            ^" two premises",t))
          )
        | Up,Par,_ ->
          (
          let finder p = fun e -> ((=) p e.dst_port) in
          let port,xs = match tkn.s with
            | Left::xs -> (MELLYS.LeftPort,xs)
            | Right::xs -> (MELLYS.RightPort,xs)
            | _ -> raise (NoTransitionException(term,(stack,tkn)))
          in
          (
          try
            let new_e = List.find (finder port) dst_prems in
            (term, (stack, {tkn with pos=new_pos; s = xs}))
          with Not_Found ->
            raise (MELLYS.IllTypedNetException("A par node must have"
            ^" exactly two premises on port 1 and 2 and one conclusion typed"
            ^" accordingly",t))
        | Down,_,Par ->
          (
          let side = match tkn.pos.dst_prt with
            | LeftPort -> Left
            | RightPort -> Right
            | _ -> raise (MELLYS.IllTypedNetException("A par node must have"
              ^" two premises on port LeftPort and RightPort",t))
          in
          match dst_concl with
            | [new_pos] -> (stack, {tkn with pos=new_pos; s=side::tkn.s})
            | _ -> raise (MELLYS.IllTypedNetException("A par node must have"
              ^" exactly one conclusion",t))
          )
        | Up,TensorType,_ ->
          (
          match tkn,src_prems with
            | {pos={label=Tensor(f,g)}; s=Left::xs},
              ([{dst_prt=1} as p;_]
               | [_;{dst_prt=1} as p]) 
            | {pos={label=Tensor(f,g)}; s=Right::xs},
              ([{dst_prt=2} as p;_]
               | [_;{dst_prt=2} as p]) -> 
              (stack, {tkn with pos=p; s=xs})
            | {s=(Left | Right)::xs},_ -> raise (MELLYS.IllTypedNetException("A
              tensor node must have exactly two premises on port 1 and 2 and one
              conclusion typed accordingly",t))
            | _ -> raise (NoTransitionException(term,(stack,tkn)))
          )
        | Down,_,TensorType ->
          (
          let where = if tkn.pos.dst_prt=1 then Left else Right in
          match dst_concl with
            | [p] -> (stack, {tkn with pos=p; s=where::tkn.s})
            | _ -> raise (MELLYS.IllTypedNetException("Tensor nodes must have
              exactly one conclusion",t))
          )
        | Up,ContractionType,_ ->
          (
          match tkn,src_prems with
            | {s=Signature(Left(esig))::xs},
              ([{dst_prt=1} as p;_]
               | [_;{dst_prt=1} as p])
            | {s=Signature(Right(esig))::xs},
              ([{dst_prt=2} as p;_]
               | [_;{dst_prt=2} as p]) ->
              (stack, {tkn with pos=p; s=Signature(esig)::xs}) 
            | {s=Signature(Left(_) | Right(_))::_}, _ -> 
              raise (MELLYS.IllTypedNetException("A contraction node must have
              exactly two premises and one conclusion all typed by the same
              formula",t))
            | _ -> raise (NoTransitionException(term,(stack,tkn)))
          )
        | Down,_,ContractionType ->
          (
          let p = match dst_concl with
            | [c] -> c
            | _ -> raise (MELLYS.IllTypedNetException("A contraction node must
              have exactly one conclusion",t)) in
          match tkn with
            | {pos={dst_prt=1}; s = Signature(esig)::xs} ->
              (stack, {tkn with pos=p; s=Signature(Left(esig))::xs}) 
            | {pos={dst_prt=2}; s = Signature(esig)::xs} ->
              (stack, {tkn with pos=p; s=Signature(Right(esig))::xs}) 
            | _ -> raise (NoTransitionException(t,(stack,tkn)))
          )
        | Up,DerelictionType,_ ->
          (
          let p = match src_prems with
            | [p] -> p 
            | _ -> raise (MELLYS.IllTypedNetException("A dereliction node must
              have exactly one premise",t)) in
          match tkn with
            | {s=Signature(Const)::xs} -> (stack, {tkn with pos=p; s=xs})
            | _ -> raise (NoTransitionException(term,(stack,tkn)))
          )
        | Down,_,DerelictionType ->
          (
          match dst_concl with
            | [c] -> (stack, {tkn with pos=c;s=Signature(Const)::tkn.s})
            | _ -> raise (MELLYS.IllTypedNetException("A dereliction must have
              exactly one conclusion",t))
          )
        | Up,SyncType,_ ->
          (
            let finder : Definitions.mellys_edge -> bool = function {dst_prt} when dst_prt=tkn.pos.src_prt -> true | _ -> false in
            let p = try List.find finder src_concl with
              Not_found -> raise (MELLYS.IllTypedNetException("A sync link must
              have the same premises and conclusions, connected on the same
              ports",t)) in
            (stack, {tkn with pos=p})
          )
        | Down,_,SyncType ->
          (
            let finder : Definitions.mellys_edge -> bool = function {src_prt} when src_prt=tkn.pos.dst_prt-> true | _ -> false in
            let p = try List.find finder dst_prems with
              Not_found -> raise (MELLYS.IllTypedNetException("A sync link must
              have the same premises and conclusions, connected on the same
              ports",t)) in
            (stack, {tkn with pos=p})
          )
        | Up,BangBox(t'),_ -> 
          (
          let finder : Definitions.mellys_edge -> bool = function {dst=0; dst_prt} when dst_prt=tkn.pos.src_prt -> true | _ -> false
          in
          let p = try List.find finder (MELLYS.net_conclusions t') with
            Not_found -> raise (MELLYS.IllTypedNetException("Mismatch between
            inner premises and conclusions of a bang box",t)) in
          match tkn with
            | {pos={src_prt=1}; s=Signature(esig)::xs} ->
              let d = if xs=[Delta] then Stable else tkn.dir in
              ((tkn.pos.src,t')::stack, {pos=p; dir=d; s=xs; e=esig::tkn.e})
            | {pos={src_prt}; s=Signature(Pack(esig,esig'))::xs}
            when src_prt <> 1 ->
              ((tkn.pos.src,t')::stack, {pos=p; dir=tkn.dir; s=Signature(esig)::xs; e=esig'::tkn.e})
            | _ -> raise (NoTransitionException(term,(stack,tkn)))
          )
        | Up,BotBox(t'),_ ->
          (
          let finder : Definitions.mellys_edge -> bool = function {dst=0; dst_prt} when dst_prt=tkn.pos.src_prt -> true | _ -> false
          in
          let p = try List.find finder (MELLYS.net_conclusions t') with
            Not_found -> raise (MELLYS.IllTypedNetException("Mismatch between
            inner premises and conclusions of a bot box",t)) in
          match tkn with
            | {pos={src_prt}} when src_prt <> 1 ->
              ((tkn.pos.src,t')::stack, {tkn with pos=p})
            | {pos={src_prt=1}} ->
              raise (MELLYS.IllTypedNetException("A bot box premise connected
              on port 1 must be a bottom",t))
            | _ -> raise (NoTransitionException(term,(stack,tkn)))
          )
        | Up,YBox(t'),_ ->
          (
          let finder : Definitions.mellys_edge -> bool = function {dst=0; dst_prt=1} -> true | _ -> false
          in
          let p = try List.find finder (MELLYS.net_conclusions t') with
            Not_found -> raise (MELLYS.IllTypedNetException("An net in a Y box must have
                  exactly two conclusions connected on port 1 and 2",t)) in
          match tkn with
            | {s=Signature(Y(_))::_} ->
              raise (NoTransitionException(term,(stack,tkn)))
            | {s=Signature(esig)::xs} ->
              ((tkn.pos.src,t')::stack, {pos=p; dir=tkn.dir; s=xs; e=esig::tkn.e})   
            | _ -> raise (NoTransitionException(term,(stack,tkn))) 
          )
        | Down,_,End ->
          if is_final_token (stack,tkn) then (stack,tkn) else
          (
          let i,t',cs = match stack with (i,_)::(_,t')::cs -> (i,t',cs)
            | _ -> raise (IncorrectStateException(t,(stack,tkn))) in
          let finder : Definitions.mellys_edge -> bool = function
            {dst=0; dst_prt} when dst_prt=tkn.pos.src_prt -> true 
            | _ -> false in
          let p = try List.find finder (MELLYS.net_conclusions t') with
            Not_found -> raise (MELLYS.IllTypedNetException("Mismatch between
            inner premises and conclusions of a bang box",t)) in
          let b_type = try List.assoc i (fst t') with
            Not_found -> raise (IncorrectStateException(t,(stack,tkn))) in
          match b_type,tkn with 
            | BangBox(_),{pos={dst_prt=1}; e=esig::es} ->
              (cs, {pos=p; dir=tkn.dir; s=Signature(esig)::tkn.s; e=es})
            | BangBox(_),
              {pos={dst_prt}; s=Signature(esig)::xs; e=esig'::es}
              when dst_prt <> 1 ->
              (cs, {pos=p; dir=tkn.dir; s=Signature(Pack(esig,esig'))::xs;
                e=es})
            | BotBox(_),_ -> 
              (cs, {tkn with pos=p})
            | YBox(_),{pos={dst_prt=1}; e=Y(esig,esig')::es} -> 
              let p = (match dst_prems with
                 | [{dst_prt=2} as p;_]
                 | [_;{dst_prt=2} as p] -> p
                 | _ -> raise (MELLYS.IllTypedNetException("An net in a Y box must have
                  exactly two conclusions connected on port 1 and 2",t))) in
              (stack, {pos=p; dir=Up; s=Signature(esig)::tkn.s; e=esig'::es})
            | YBox(_),{pos={dst_prt=2}; s=Signature(esig)::xs; e=esig'::es} ->
              let p = (match dst_prems with
                | [{dst_prt=1} as p;_]
                | [_;{dst_prt=1} as p] -> p
                | _ -> raise (MELLYS.IllTypedNetException("An net in a Y box must have
                  exactly two conclusions connected on port 1 and 2",t))) in
              (stack, {pos=p; dir=Up; s=xs; e=Y(esig,esig')::es})
            | _ -> raise (NoTransitionException(term,(stack,tkn)))
          )
      | Stable,_,_ -> (stack,tkn)
      | _ -> raise (IncorrectStateException(term,(stack,tkn)))

    let transition t = function _ -> raise (Failure("En travaux")) 
end
