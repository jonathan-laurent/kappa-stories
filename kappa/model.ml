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
  refn : int ;         (* Refinment number *)
  lhs  : site_graph ;
  rhs  : site_graph ;
}


let dump fmt m = 

  let pr msg = Format.fprintf fmt msg in
  let print_rule r =
	let printer = SimplePrinter.SiteGraph.fprintf in
	pr "'%s' " r.name ;
	pr "%a -> %a@;" printer r.lhs printer r.rhs in
	
  pr "@[<v>" ;
  List.iter print_rule m.rules  ;
  pr "@]@."

	
	







 
