(* module Logic = struct *)

module MELL = 
  struct 
    type formula =
      | One
      | Bot
      | Atom of string
      | Par of formula * formula
      | Tensor of formula * formula
      | Whynot of formula
      | Bang of formula
      | Not of formula

    let rec not_normal_form = function
      | Not(One) | Bot -> Bot
      | Not(Bot) | One -> One
      | Not(Atom(_)) as f -> f
      | Atom(_) as f -> f
    
      | Not(Par(f,g)) -> Tensor(not_normal_form (Not(f)), not_normal_form (Not(g)))
      | Not(Tensor(f,g)) -> Par(not_normal_form (Not(f)), not_normal_form (Not(g)))
      | Not(Whynt(f)) -> Bang(not_normal_form (Not(f)))
      | Not(Bang(f)) -> Whynot(not_normal_form (Not(f)))
      | Not(Not(f)) -> not_normal_form f
    
      | Par(f,g) -> Par(not_normal_form f, not_normal_form g) 
      | Tensor(f,g) -> Tensor(not_normal_form f, not_normal_form g) 
      | Whynot(f) -> Whynot(not_normal_form f) 
      | Bang(f) -> Bang(not_normal_form f)
  end

(* end *
