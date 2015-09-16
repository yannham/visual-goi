open Net

(*module type AbstractMachine = sig
  type state
  type term

  exception NoTransitionException of state

  val init : term -> state
  val transition : term * state -> state
  val is_final : term * state -> bool
end*)

module SIAM : sig
    type exp_sign = Const 
                  | Left of exp_sign
                  | Right of exp_sign 
                  | Pack of exp_sign * exp_sign
                  | Y of exp_sign * exp_sign

    type stack_elt = Left | Right | Signature of exp_sign | Delta
    type stack = stack_elt list
    type dir = Up | Down | Stable

    type term = MELLYS.t 
    type code = (int * term) list
    type token = MELLYS.Edge.t * dir * stack * exp_sign list
    type state = token list

    exception NoTransitionException of term * state
    exception IncorrectCodeException of code 

    val compile : term -> code
    val token_transition : code -> token -> code * token
    val transition : code -> state -> code * state
    val is_initial : code -> state -> bool
    val is_final : code -> state -> bool

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

    type term = Definitions.mellys_net
    type code = term list
    type token = Definitions.mellys_edge * dir * stack * exp_sign
    type state = token list

    exception NoTransitionException of term * state
    exception IncorrectCodeException of term

    let mll_location f = function
      | Parr(g,g') when g=f -> Left
      | Parr(g,g') when g'=f -> Right
      | Tensor(g,g') when g=f -> Left
      | Tensor(g,g') when g'=f -> Right
      | _ -> raise Not_found

    let token_transition code (e,d,s,exp) = 
      let t = match code with 
        | [] -> raise (Failure("empty code"))
        | (_,t)::ts -> t in
      let source,dest = MELLYS.edge_source_dest t e in
      let i,j,f = e in
      let src_concl = MELLYS.vert_conclusions t i in
      let dst_concl = MELLYS.vert_conclusions t j in
      let src_prems = MELLYS.vert_premises t i in
      let dst_prems = MELLYS.vert_premises t j in
      match d,source,dest with
        | Up,AxiomType,_ -> 
          ( 
          try
            let new_e = List.find ((<>) e) src_concl in
            (code,(new_e,Down,s,exp))
          with Not_found -> raise (MELLYS.IllTypedNetException("An axiom node doesn't have proper conclusions",t))
          ) 
        | Down,_,CutType ->
          ( 
          try
            let new_e = List.find ((<>) e) dst_prems in
            (code,(new_e,Up,s,exp))
          with Not_found -> raise (MELLYS.IllTypedNetException("A cut node
            doesn't have proper premises",t))
          )
        | Up,ParrType,_ ->
          (
          let new_e,new_s = match f,s,src_prems with
            | Parr(f',g'),Left::xs,[( (_,_,h) as x);_] when h=f' -> (h,xs)
            | Parr(f',g'),Left::xs,[_; ( (_,_,h) as x)] when h=f' -> (h,xs)
            | Parr(f',g'),Right::xs,[( (_,_,h) as x);_] when h=g' -> (h,xs)
            | Parr(f',g'),Right::xs,[_; ( (_,_,h) as x)] when h=g' -> (h,xs)

            | Parr(f',g'),Left::xs,_ | Parr(f',g'),Left::xs,_ -> raise (MELLYS.IllTypedNetException("A
              parr node must have exactly two premises"))
            | _ -> raise (NoTransitionException(t,(e,d,s,exp)))
          in (new_e,d,exp,new_s)
          )
        | Down,_,ParrType ->
          (
          match dst_concl with
            | [( (_,_,Parr(g,g')) as new_e)] when g=f ->
              (code,(new_e,d,exp,Left::s))
            | [( (_,_,Parr(g,g')) as new_e)] when g'=f ->
              (code,(new_e,d,exp,Right::s))
            | [_] -> raise (MELLYS.IllTypedNetException("The conclusion of a parr
              is not typed by the parr of its premises",t))
            | _ -> raise (MELLYS.IllTypedNetException("Parr nodes must have
              exactly one conclusion",t))
          )
        | Up,TensorType,_ ->
          (
          let new_e,new_s = match f,s,src_prems with
            | Tensor(f',g'),Left::xs,[( (_,_,h) as x);_] when h=f' -> (h,xs)
            | Tensor(f',g'),Left::xs,[_; ( (_,_,h) as x)] when h=f' -> (h,xs)
            | Tensor(f',g'),Right::xs,[( (_,_,h) as x);_] when h=g' -> (h,xs)
            | Tensor(f',g'),Right::xs,[_; ( (_,_,h) as x)] when h=g' -> (h,xs)

            | Tensor(f',g'),Left::xs,_ | Tensor(f',g'),Left::xs,_ -> raise (MELLYS.IllTypedNetException("A
              tensor node must have exactly two premises"))
            | _ -> raise (NoTransitionException(t,(e,d,s,exp)))
          in (code,(new_e,d,exp,new_s))
          )
        | Down,_,TensorType ->
          (
          match dst_concl with
            | [( (_,_,Tensor(g,g')) as new_e)] when g=f -> (new_e,d,exp,Left::s)
            | [( (_,_,Tensor(g,g')) as new_e)] when g'=f -> (new_e,d,exp,Right::s)
            | [_] -> raise (MELLYS.IllTypedNetException("The conclusion of a tensor
              is not typed by the tensor of its premises",t))
            | _ -> raise (MELLYS.IllTypedNetException("Tensor nodes must have
              exactly one conclusion",t))
          )
        | Up,ContractionType,_ ->
          (
          match src_prems,s with
            | [p;_],Signature(Left(exp'))::xs | [_;p],Signature(Right(exp'))::xs -> 
              (code,(p,d,exp,Signature(exp')))
            | _,Signature(Left(_))::_ | _,Signature(Right(_))::_ -> raise (MELLYS.IllTypedNetException("A
              must have exactly two premises",t))
            | _ -> raise (NoTransitionException(t,(e,d,s,exp)))
          )
        | Down,_,ContractionType ->
          (
          let new_e,new_s = match dst_prems,dst_concl,s with
            | [p;_],[c],Signature(exp')::xs when p=e -> (c,Signature(Left(exp')))
            | [_;p],[c],Signature(exp')::xs when p=e -> (c,Signature(Right(exp')))
            | _,Signature(exp')::xs -> raise (MELLYS.IllTypedNetException("A
              contraction node must have exactly two premises and one conclusion
              typed by the same formula"))
            | _ -> raise (NoTransitionException(t, (e,d,s,exp)))
          in (code,(new_e,d,new_s,exp))
          )
        | Up,DerelictionType_ ->
          (
          match src_prems,s with
            | [p],Const::xs -> (code,(p,d,xs,exp))
            | _,Const::xs -> raise (MELLYS.IllTypedNetException("A dereliction
              node must have exactly one premise",t))
            | _ -> raise (NoTransitionException(t,(e,d,s,exp)))
          )
        | Down,_,DerelictionType ->
          (
          match dst_concl with
            | [c] -> (code,(c,d,Signature(Const)::s,exp))
            | _ -> raise (MELLYS.IllTypedNetException("A dereliction must have
              exactly one conclusion"))
          )
        | SyncType ->
          (
          )
        | Up,BangBox(r'),_ -> 
          (
          let inside_concl = net_conclusions r' in
          let target = match f with 
            | Bang(f') -> f'
            | _ -> f in
          let new_e = try List.find (function (_,_,h) -> h=target) inside_concl
          with Not_found -> raise (MELLYS.IllTypedNetException("Mismatch
            between conclusions of a Bang box and conclusions of inner
            net")) in
          match f,s with
            | Bang(_),Signature(exp')::xs,_ ->
              let new_d = if xs=[Delta] then Stable else d in
              ((i,r')::code,(new_e,new_d,xs,exp'::exp))
            | Whynot(_),Signature(Pack(exp1,exp2))::xs ->
              ((i,r')::code,(new_e,d,Signature(exp1)::xs,exp2::exp))
            | _ -> 
              raise (MELLYS.IllTypedNetException("A Bang box must have exactly
              one banged conclusion and zero or more whynot conclusions"))
          )
        | BotBox(r') ->
          (
            
          )
        | YBox(r') ->
          (
          )
    let is_final = fun x -> true
end
