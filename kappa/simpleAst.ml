module SG  = SiteGraph
module Sig = Signature

open Util
open Lens

type sgraph = agent int_map

and agent = {
	ty    : string ;
	sites : site list ;		
}

and site = {
	name : string ;
	int_state : string option ;
	lnk_state : link
}

and link =
  | Lnk_value of int
  | Lnk_free
  | Lnk_any
  | Lnk_some
  | Lnk_type of port_ty

and port_ty = string (* type of agent *)
  		    * string (* port *)

and port = int * string


(* This is used to generate link numbers. When a link is encountered
 * for the first time, it is given a new identifier, and the former is 
 * remembered *)


type links_handler = 
	{ memorized : (SG.Link.t, int) Hashtbl.t ;
	  mutable next_id : int
	} 

let create_links_handler () = {
	memorized = Hashtbl.create 100 ;
	next_id   = 1 
}

let get_link_id lh lnk = 
	try Hashtbl.find lh.memorized lnk
	with Not_found -> 
		let id = lh.next_id in
		Hashtbl.add lh.memorized lnk id ;
		lh.next_id <- id + 1 ;
		id



(* [lh] : links handler stands for the links handler *)

let dump_port (sign : Sig.t) lh p = 

	let open SG.Site in

	let p_sign        = p |> signature in
	let int_state_opt = p |. int_state in
	let p_id          = p |> port_id   in
	
	{ name      = p_sign |> Sig.Site.name ;
	  int_state = int_state_opt |> Option.map
			(fun is -> p_sign |> Sig.Site.name_of_int_state is) ;
	  	 
	  lnk_state = 
	  	match (p |. lnk_state) with
	  		| Free -> Lnk_free
			| Any -> Lnk_any
			| Bound_some -> Lnk_some
			| Bound_ty port_ty -> Lnk_type (Sig.port_ty_name port_ty sign)
			| Bound p' -> Lnk_value (get_link_id lh (SG.Link.make p_id p'))
	}
	
	
let dump_agent sign lh ag = 
	{ ty = ag |> SG.Agent.signature |> Sig.Agent.ty_name ;
	  sites = 
	  	SG.Agent.fold (fun p acc -> dump_port sign lh p :: acc) ag []
	  	|> List.rev
	}


let dump sg =
	let lh = create_links_handler () in
	let sign = sg |> SG.signature in
	
	SG.fold_agents 
		(fun ag acc -> Imap.add (ag |> SG.Agent.id) (dump_agent sign lh ag) acc) 
		sg 
		Imap.empty






(* ************************************************************************

   Utilities

** ********************************************************************  *)

let compile_links mixture = 

	(* Already encountered links *)
	let t1 = Hashtbl.create 100 in
	
	(* port -> port table *)
	let t2 = Hashtbl.create 100 in
	
	let register_link i p = 
		try 
			let dst = Hashtbl.find t1 i in
			Hashtbl.add t2 p dst ;
			Hashtbl.add t2 dst p
		with Not_found ->
			Hashtbl.add t1 i p in
				
	mixture |> Imap.iter (fun a_id a ->
		a.sites |> List.iter
		(fun s -> match s.lnk_state with
			| Lnk_value i -> register_link i (a_id, s.name)
			| _ -> () )
		
		) ;
	
	printf "{%d}" ( Hashtbl.length t2) ;
	fun p -> try Some (Hashtbl.find t2 p) with Not_found -> None





