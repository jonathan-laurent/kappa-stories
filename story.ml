module Make (S : AgentBased.System) = struct

  module AbsUtils = AgentBased.Utils(S)

  open S
  open AbsUtils
  open Util

  (* An event is a rule where local ids of agents are renamed so they
     corresponds to physical entities *)

  type event_id = int
    
  type event = int * rule

  module Relation = MapSet (Int) (ISet)

  type relation = Relation.t

  type story = {
    events : event int_map ;
    prec   : relation ;
  }

  let dump_dot fmt s = 
    let pr msg = Format.fprintf fmt msg in

    let gen_node _ (id, r) = 
      pr "{ %d [label=\"%s\"] }@;" id (rule_name r) in

    let gen_arr src dest = 
      pr "%d -> %d@;" src dest in

    pr "@[<v 2>digraph G{@;" ;
    pr "rankdir=\"TB\";@;" ;
    pr "ranksep=.3;@;" ;

    Imap.iter gen_node s.events ;
    pr "@;";
    Relation.iter_bindings gen_arr s.prec ;
    pr "}@]@."

end
