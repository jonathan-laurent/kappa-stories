(** The signature of a site graph.  *)


(** Signature of the sub components of a site graph : agents and sites *)

module Site : sig

	type t

	type id
	type int_state
	
	type name            = string
	type int_state_name  = string

	type builder = name * int_state_name list


	val create : id -> builder -> t
	
	val id     : t -> id
	val name   : t -> name
	
	val def_int_state : int_state
	
	val has_states : t -> bool
	
	val name_of_int_state : int_state -> t -> int_state_name
	
	val int_state_named : int_state_name -> t -> int_state
	

	
end


module Agent : sig

	type ty
	type ty_name = string
	
	type builder = ty_name * Site.builder list
	
	type t
	
	val ty      : t -> ty
	
	val ty_name : t -> ty_name
	
	val create : ty -> builder -> t
	
	val site : Site.id -> t -> Site.t
	
	val iter : (Site.t -> unit) -> t -> unit

	val site_named :  Site.name -> t -> Site.t
	
end


(** The type of a site graph's signature *)

type t

(** A signature can be constructed from an embedded list based object *)

type builder = Agent.builder list

val create : builder -> t

(** Agent signatures can be accessed by type or by type name *)

val agent : Agent.ty -> t -> Agent.t

val agent_named : Agent.ty_name -> t -> Agent.t


(** A port type consists in an agent type and a site identifier. 
  * It is used when specifying a typed partial link for instance *)

type port_ty      = Agent.ty      * Site.id

type port_ty_name = Agent.ty_name * Site.name

val port_ty_name : port_ty -> t -> port_ty_name

val port_ty_named : port_ty_name -> t -> port_ty




