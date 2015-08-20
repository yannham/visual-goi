module MELL : sig
  (** Describe formulas of Multiplicative Exponential Linear Logic *)
  type formula =
    | One
    | Bot
    | Atom of string
    | Par of formula * formula
    | Tensor of formula * formula
    | Whynot of formula
    | Bang of formula
    | Not of formula

  (** Take a formula and push the negations to the atoms *)
  val not_normal_form : formula -> formula
end
