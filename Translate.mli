module UntypedLinearToMELLYS : sig
  val net_of_term : Lambda.Untyped.term -> Net.MELLYS.t 
end

module SimplyTypedToMELLYS_CBV : sig
  val net_of_term : Lambda.SimplyTyped.term -> Net.MELLYS.t
end

module SimplyTypedToMELLYS_CBN : sig
  val net_of_term : Lambda.SimplyTyped.term -> Net.MELLYS.t
end
