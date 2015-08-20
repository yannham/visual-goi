(* module Lambda = struct *)

module type TYPED =
  sig
    type untyped_term 
    type type_t
    type term

    exception IllTypedException of term * string

    val type_of_term : term -> type_t
    val type_erasure : term -> untyped_term
  end

module type TYPED_INFER = 
  sig
    include TYPED

    val infer : untyped_term -> term 
  end

module Untyped =
  struct
    type term =
      | Var of char
      | Const of int
      | Abs of char * term
      | App of term * term
  end 

module SimplyTyped = 
  struct
    type untyped_term = Untyped.term
    
    type type_t = 
      | Unit 
      | Arrow of type_t * type_t
    
    type term =
      | TVar of char * type_t
      | TConst of int
      | TAbs of char * type_t * term
      | TApp of term * term
  
    type partial_type =
      | PUnknown
      | PUnit
      | PArrow of partial_type * partial_type

    exception IllTypedException of term * string
  
    let rec type_of_term = function
      | TVar(_,t) -> t 
      | TConst(_) -> Unit
      | TAbs(c,t,m) -> Arrow(t, type_of_term m)
      | TApp(m,n) ->
        (
          match (type_of_term m, type_of_term n) with 
            | Arrow(a,b), c when a=c -> b
            | _ -> raise (IllTypedException(TApp(m,n),"Type mismatch in application"))
        )

    let rec type_erasure = function 
      | TVar(c,_) -> Untyped.Var(c)
      | TConst(i) -> Untyped.Const(i)
      | TAbs(c,_,m) -> Untyped.Abs(c,type_erasure m)
      | TApp(m,n) -> Untyped.App(type_erasure m, type_erasure n)
  end

module ConcurrentRegion =
  struct
    type term =
      | Var of char
      | Const of int
      | Abs of char * term
      | App of term * term
      | Set of char * term
      | Get of char
      | Par of term * term
  end

(* end *)
