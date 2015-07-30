(** Signature of an agent based system. *)

module type System = sig

  type agent_id             = int
  type var
  type trait

  (* Structural equality is used on this type *)
  type trait_type

  type rule
  type rule_id              = int

  val var_agent             : var -> agent_id
  val set_var_agent         : agent_id -> var -> var
  val trait_vars            : trait -> var list
  val trait_type            : trait -> rule -> trait_type

  val rule_id               : rule -> rule_id
  val is_created            : agent_id -> rule -> bool
  val iter_effects          : (trait -> unit) -> rule -> unit
  val iter_preconditions    : (trait -> unit) -> rule -> unit
  val iter_agents           : (agent_id -> unit) -> rule -> unit

  module Trait : Map.OrderedType with type t = trait

  (* Printing functions : for debug purposes *)

  val print_trait : rule -> Format.formatter -> trait -> unit
  val rule_name : rule -> string

end


module Utils (S : System) = struct

  open S
  open Util

  module TrMap = ExtMap   (Trait)
  module TrSet = Set.Make (Trait)

  type eff  = trait
  type pre  = trait

  type 'a pre_map = 'a TrMap.t
  type 'a eff_map = 'a TrMap.t


  let fold_preconditions   f a t = fold_from_iter iter_preconditions  f a t
  let fold_effects         f a t = fold_from_iter iter_effects        f a t
  let fold_agents          f a t = fold_from_iter iter_agents         f a t

end
