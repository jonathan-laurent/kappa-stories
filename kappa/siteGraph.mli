
(** A kappa site graph. This module provides a reasonably efficient 
	persistent data structure for site graphs.
    This structure might not be adapted to work with mixtures as partial specification of
    agent and sites is allowed. *)

open Lens


(** {2 Type aliases and sub-components} *)


(**  Some type aliases *)


type signature       = Signature.t
type site_signature  = Signature.Site.t
type agent_signature = Signature.Agent.t

type agent_id        = int
type agent_ty        = Signature.Agent.ty
type site_id         = Signature.Site.id
type site_int_state  = Signature.Site.int_state option
type port_ty         = Signature.port_ty
type port_id         = (agent_id * site_id)

(** Maps of port identifiers *)

module PortMap : (Util.EXT_MAP_S with type key = port_id)


(** Modules for sub-components *)


module Link : sig

	type t
	
	val compare : t  -> t -> int
	
	val make : port_id -> port_id -> t
	
	val ports : t -> (port_id * port_id)
end

type link = Link.t

module Site : sig

	type t
	
	type int_state = site_int_state
	
	and lnk_state = 
		| Free
		| Any
		| Bound_some
		| Bound_ty of port_ty
		| Bound    of port_id
		
	val signature : t -> site_signature
		
	val lnk_state : (t, lnk_state) lens
	
	val int_state : (t, int_state) lens
	
	val port_id : t -> port_id

	val create : agent_id -> site_signature -> t
	
	val link : t -> link option
	
end

type site = Site.t

module Agent : sig

	type t
	
	val create : agent_id -> agent_signature -> t

	val id : t -> agent_id
	
	val site : site_id -> (t, Site.t) lens	
	
	val signature : t -> agent_signature
	
	val fold : (Site.t -> 'acc -> 'acc) -> t -> 'acc -> 'acc
	
	val iter : (Site.t -> unit) -> t -> unit
	
	val add_site : site_id -> t -> t

    val has_site : site_id -> t -> bool

end

type agent = Agent.t



(** {2 Definition and operations on site graphs } *)

type t

type site_graph = t
	(** To use when the [SiteGraph] module is opened. *)


val create : signature -> t
	
val signature : t -> signature

val agent : agent_id -> (t, agent) lens

val agent_exists : agent_id -> t -> bool

val port : port_id -> (t, site) lens
	
val fold_agents : (agent -> 'acc -> 'acc) -> t -> 'acc -> 'acc

val fold_ports : (site -> 'acc -> 'acc) -> t -> 'acc -> 'acc

val iter_ports : (site -> unit) -> t -> unit

val add_agent : ?id:agent_id -> agent_ty -> t -> (t * agent_id)
	(** Adds an agent in the site graph. It is possible to specify
		manually its identifier by providing the [?id] argument. If it
		is already used, no agent is added. The added agent is completely unspecified 
		(it has no sites).
	*)
	
val remove_agent : agent_id -> t -> t
	(** Removes an agent along with all the links connected to it *)



(** Operations on links *)

val link_exists : link -> t -> bool

val add_link : link -> t -> t

val remove_link : link -> t -> t 

val are_bound : port_id -> port_id -> t -> bool

val bind : port_id -> port_id -> t -> t

val unbind : port_id -> port_id -> t -> t

val unbind_semi : port_id -> t -> t 
	
val port_ty : port_id -> t -> port_ty
















