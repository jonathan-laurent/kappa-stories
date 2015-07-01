(** MODEL.ml : a Kappa model
 *  Only informations relevant to causal analysis is kept *)

open SiteGraph
open Util


type model = {
	signature   : Signature.t ;
	init        : init_decl list ;
	rules       : rule list ;
	observables : SiteGraph.t string_map
}

and init_decl = {
	max_instances : init_max_instances ;
	init_rule     : rule
}

and init_max_instances =
	| One
	| Unlimited

(* The morphism preserves agent identifiers *)

and rule = {
	name : string ;
	lhs : site_graph ;
	rhs : site_graph ;
}




(* Rule compilation process *)

type action = 
	| CreateAgent of (agent_ty * agent_id)
	| RemoveAgent of (agent_id)
	| Bind        of (port_id * port_id)
	| Mod         of (port_id * site_int_state)
	| Break       of (link)
	| BreakSemi   of (port_id)



let create_default_agent (s : Signature.t) = failwith ""

	
	







 
