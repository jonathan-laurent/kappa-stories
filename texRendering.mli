(** LaTex rendering of small Kappa expressions with Mlpost. *)

open SimpleAst
open Mlpost

(** {2 Parameters and positioning policies} *)

(** Angles are expressed in _degrees_. *)
type angle = float

(** User positioning policy :
    Association list giving the site positioning policy of each agent type *)
type user_pos_policy = (agent_ty * ag_sites_angles) list

(** Association list giving the angular position of each site on an agent *)
and ag_sites_angles = (site_name * angle) list

(** Positioning constraints for an agent *)
type ag_pos_constrs = ag_pos * ag_sites_angles
and  ag_pos = Point.t (** Position of the center of an agent (relative to complex) *)

(** A closure mapping an agent id to its positionning constraints *)
type pos_constrs = {
	ag_pos_constrs : agent_id -> ag_pos_constrs ; (** Positioning constraints for an agent *)
	complexes_list : int list list; (** List of connected components *)
}
	
	
(** Drawing parameters *)
	
type ag_params = {
	 ag_label : string  ;
	 ag_color : Color.t ;
	 ag_diam  : Num.t   ; (** Diameter *)
}	
			  
and site_params = {
	s_label : string  ;
	s_color : Color.t ;
}

and params = {
	ag_dist     : Num.t ;  (** Distance between the center of two neighbors *)
	site_diam   : Num.t ;
	dev_angle   : angle ;  (** Deviation angle. See below. *)
	pos_policy  : user_pos_policy ;
	ag_params   : agent_id -> agent_ty -> ag_params ;
	site_params : agent_id -> agent_ty -> site_name -> site_params
}
(** The deviation angle is used so two agents that can be connected to an other one
    via the same site are not displayed in the same position. *)


(** {2 Standard parameters} *)

(** Default parameters *)
val def_params : user_pos_policy -> params

(** Parameters for testing purposes *)
val my_params : params


(** {2 Rendering functions} *)

(** In order use [render] on single mixture, some positioning constraints
	have to be computed from the list of all mixtures which will be rendered, using [compute_pos_constrs] *)


(** Computes some positioning constraints from a list of agents *)
val compute_pos_constrs : sgraph list -> params -> pos_constrs

(** Renders a site-graph as a mps file *)
val render : filename:string -> pos_constrs -> sgraph -> params -> unit



