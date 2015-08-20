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

module Untyped : sig 
  (** Describe a term of untyped lambda calculus *)
  type term =
    | Var of char
    | Const of int
    | Abs of char * term
    | App of term * term
end 

module SimplyTyped : sig
  type type_t = 
    | Unit 
    | Arrow of type_t * type_t
  
  type term =
    | TVar of char * type_t
    | TConst of int
    | TAbs of char * type_t * term
    | TApp of term * term

  include TYPED with type untyped_term = Untyped.term
    and type type_t := type_t
    and type term := term 
end

(* module ConcurrentRegion : sig
  == Define a term of untyped concurrent lambda calculus with regions ==
  type untyped_term =
    | Var of char
    | Const of int
    | Abs of char * term
    | App of term * term
    | Set of char * term
    | Get of char
    | Par of term * term

  type value_type 
  type behavior 

  type 'a type_t =
    | Unit : value_type type_t
    | Behavior : behavior type_t
    | Arrow : value_type type_t * 'a type_t -> value_type type_t 
    | Bang : value_type type_t -> value_type type_t

  type context_var = 

  type 'a type_derivation = 
    | VarRule : 
  include TYPE_INFER with untyped_term = untyped_term  
end *)
