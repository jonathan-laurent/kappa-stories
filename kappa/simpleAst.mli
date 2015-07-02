(** Minimal structures and utilities to represent Kappa expressions. *)

open Util

type agent_id = int

(** [ty] stands for "type" *)
type agent_ty = string

type site_name = string

type int_state = string

type port_ty = agent_ty * site_name

type port_id = agent_id * site_name

type link =
  | Lnk_value of int
  | Lnk_free
  | Lnk_any
  | Lnk_some
  | Lnk_type of port_ty

type site = {
	name : site_name ;			   
	int_state : int_state option ; (** Internal state, or [None] if unspecified *)
	lnk_state : link ;			   (** Linking state  *)
}

type agent = {
	ty    : agent_ty ;
	sites : site list ;		
}
  
(** A kappa site-graph : each agent identifier is mapped to an agent *)
type sgraph = agent int_map




(** Translates an object of type [SiteGraph.t] to the corresponding object of type 
    [sgraph] *)
val dump : SiteGraph.t -> sgraph

(** Takes a site-graph and returns a closure which maps a port [p] to 
	[Some p'] if [p] is linked to [p'], [None] otherwise *)
val compile_links : sgraph -> (port_id -> port_id option)
