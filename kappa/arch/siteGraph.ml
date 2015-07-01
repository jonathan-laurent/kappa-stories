(* *)

module type ORDERED_T = Map.OrderedType
module type ANY_T     = sig type t end


type agent_id          = int
type agent_ty          = NTable.id
type site_id           = NTable.id
type site_int_state    = NTable.id
type port              = (agent_id * site_id)

type 'a ntable         = 'a NTable.t



module type SITE_T = sig

	type t
	
	type int_state = site_int_state
	type lnk_state = 
		| Free
		| Bound_to_any
		| Bound_to_some
		| Bound of port
		
	val lnk_state : t lnk_state lens
	val int_state : t int_state lens	
	
	type signature = {
		id : site_id ;
		name : string ;
		states : unit ntable
	}
	
	val signature : t -> signature
	
	val port : t -> port

	val create : parent:agent_id -> signature:signature -> t
	
end


module type AGENT_T = sig

	type t
	type id = agent_id
	type ty = agent_ty
	
	type signature = {
		ty : ty ;
		ty_name : string ;
		sites : Site.signature ntable
	}
	
	val site : Site.id -> t Site.t lens	
	val site_named : string -> t Site.t lens
	
	val create : signature -> t
	val signature : t -> signature

end


module type SITE_GRAPH_T = struct

	type t
	
	type signature = Agent.signature ntable
	
	val create : signature -> t
	
	val signature : t -> signature
	
	val agent : Agent.id -> t Agent.t lens
	
	val fold : (Agent.t -> 'acc -> 'acc) -> t -> 'acc -> 'acc
	
	val add_agent : Agent.ty -> t -> (t, Agent.id)
	
end
		
		


module Site : SITE_T = struct

	


end	




end


(* 
 * AL : Agent Labels
 * PL : Ports Labels
 * AA : Agent Annotations
 * PA : Ports Annotations
*)

module Decorate (SG : S) (AL : ORDERED_T) (PL : ORDERED_T) (AA : ANY_T) (PA : ANY_T)
	= struct
	
	
end


open Util

type 'a table = 'a Table.t


type agent_id = int
type site_id  = int
type port = (agent_id * site_id)


module Site = struct
	
	type int_state = int
	
	type lnk_state = 
		| Free
		| Bound_to_any
		| Bound_to_some
		| Bound of port
		
	type signature = {
		id : agent_id ;
		name : string ;
		states : unit table
	}
	
	type t = {
		int_state : int_state ;
		lnk_state : lnk_state 
	}

end


module Agent = struct

	type ty = int

	type t = { id : agent_id ; ty : ty ; sites : Site.t int_map }
	
	type signature = {
		ty : ty ;
		ty_name : string ;
		sites : Site.signature table
	}

end


type agent = Agent.t
	
type signature = Agent.signature table


module PLmap   = Map.Make(PL)
module ALmap   = Map.Make(AL)
module PortMap = I2map
module AgMap   = Imap


type t = {
	signature : signature ;
	agents : agent int_map ;
	
	pl_map : port PLmap.t ;
	al_map : port ALmap.t ;
	pa_map : PA.t PortMap.t ;
	aa_map : AA.t AgMap.t
}
	
	
	(*
	val create : signature -> t
	
	val is_consistent : t -> bool
	*)







end












