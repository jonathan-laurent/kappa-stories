(* LaTex rendering of small Kappa expressions *)

open SimpleAst
open Util
open Mlpost
open Num
open Point
open Command
open Color


(** Angles are expressed in _degrees_. *)
type angle = float

(** User positioning policy :
    Association list giving the site positioning policy of each agent type *)
type user_pos_policy = (agent_ty * ag_sites_angles) list

(** Association list giving the angular position of each site on an agent *)
and ag_sites_angles = (site_name * angle) list

(** Positioning constraints for an agent *)
type ag_pos_constrs = ag_pos * ag_sites_angles
and  ag_pos = Point.t (** Position of the center of an agent *)

(** A closure mapping an agent id to its positionning constraints *)
type pos_constrs = agent_id -> ag_pos_constrs
	
	
	
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


let def_ag_params id name = {
	ag_label = name ;
	ag_color = Color.white ;
	ag_diam  = cm 1.
}

let def_site_params ag_id ag_type site_name = {
	s_label = site_name ;
	s_color = Color.white
}

let def_params pos_policy = {
	ag_dist = cm 1.7 ;
	site_diam = cm 0.3 ;
	dev_angle = 30. ;
	pos_policy = pos_policy ;
	ag_params = def_ag_params ;
	site_params = def_site_params
}


let my_pos_policy = [ ("A", [("x", 0.) ; ("y", 0.)]) ]

let my_params = def_params my_pos_policy









let agent_types_map (mixtures : sgraph list) : agent_ty int_map = 
	mixtures |> List.fold_left (fun mixt acc ->
		mixt |> Imap.fold (fun ag_id ag acc ->
			Imap.add ag_id ag.ty acc
			) acc
		) Imap.empty
		

(* Returns the set of all agent types encountered in a list of mixtures *)
let agent_types_set (mixtures : sgraph list) : SSet.t = 
	mixtures 
	|> agent_types_map |> Imap.to_list 
	|> List.map snd |> sset_of_list



let iter_mixture f mixt = 
	mixt |> Imap.iter ( fun ag_id ag ->
			ag.sites |> List.iter ( fun s ->
				f ag_id ag s
			)
		)

let iter_mixtures f mixtures = 
	mixtures |> List.iter (fun mixt ->
		iter_mixture f mixt
	)
	

(* Function to add a (key, value) pair to an hash table only if its is not in it *)

let hashtbl_set_add t k v = 
	try if not (List.mem v (Hashtbl.find_all t k)) then raise Not_found
	with Not_found -> Hashtbl.add t k v


(* Takes a list of mixtures
	Returns a function which maps an agent type to the list of the sites 
	belonging to this agent *)

let make_sites_list (mixtures : sgraph list) : (agent_ty -> site_name list) = 

	let t = Hashtbl.create 100 in
	mixtures |> iter_mixtures (fun _ ag s ->
		let sn = s.name in
		hashtbl_set_add t ag.ty sn
	) ;
	fun ty -> Hashtbl.find_all t ty
	

(* Gives the type of a port in a mixture *)

let ty_of_port ((ag_id, s) : port_id) (mixture : sgraph) : port_ty = 
	let ag_ty = (Imap.find ag_id mixture).ty in
	(ag_ty, s)
	

let cut_angle i n = (float_of_int i) *. (360. /. float_of_int n)
	


(* Makes a closure which maps a port to the list of its neighbours 
   (relative to a set of mixtures) *)

let make_neigh_map (mixtures : sgraph list) : (port_id -> port_id list) = 
	
	let t = Hashtbl.create 100 in
	
	mixtures |> List.iter (fun mixt -> 
		let ag_neigh = SimpleAst.compile_links mixt in
		mixt |> iter_mixture (fun ag_id ag s ->
			ag_neigh (ag_id, s.name) |> Option.map_default () (fun dest ->
				hashtbl_set_add t (ag_id, s.name) dest
			)	
		)	
	) ;
	
	(*printf "##\n" ;
	Hashtbl.iter (fun (id, s) (id', s') -> printf "(%d, %s) to (%d, %s)" id s id' s' ) t ;
	printf "##\n" ; *)
	
	fun port -> Hashtbl.find_all t port










(* Returns a closure mapping an agent type to its sites angles.
	This is done by completing the user specified policy ([params.pos_policy]) *)

let sites_angles_policy mixtures params : (agent_ty -> ag_sites_angles) = 

	let map = Smap.of_list (params.pos_policy) in
	
	let sites_list = make_sites_list mixtures in
	
	let map = SSet.fold (fun ag_ty acc ->
	
		let user_conf = try Smap.find ag_ty map with _ -> [] in
		if user_conf = [] then
			(* Generates a config *)
			let sl = sites_list ag_ty in
			let n = List.length sl in
			let conf = List.mapi (fun i s -> (s, cut_angle i n)) sl in
			Smap.add ag_ty conf	acc
			
		else begin
			(* Checks that the user provided all the sites *)
			sites_list ag_ty |> List.iter (fun s -> 
				assert (List.mem_assoc s user_conf)
			) ;
			acc 
		end
			
	) ((agent_types_set mixtures) : SSet.t) map in
	
	fun ag_ty -> Smap.find ag_ty map
		
	
let rotate (al : float) (asas : ag_sites_angles) = 
	List.map (fun (s, a) -> (s, a +. al) ) asas

	

	
	

exception Found_anchor of string * port_id

let compute_pos_constrs mixtures params = 

	(* Agents are sorted by 
		1. Order of appearance of their type in the [pos_constrs] structure
		2. Number of links attached
		3. Id of the agent
	*)
	
	let neigh            =  make_neigh_map       mixtures        in
	let agent_types_map  =  agent_types_map      mixtures        in
	let ag_pos_constrs   =  sites_angles_policy  mixtures params in
	let sites_list       =  make_sites_list      mixtures        in
	
	let ty_of_id ag_id   = Imap.find ag_id agent_types_map       in
	
	let ag_priority = 
		let map = 
			params.pos_policy 
			|> List.mapi (fun i (ag_ty,_) -> (ag_ty, i))
			|> Smap.of_list in
		let default_order = List.length params.pos_policy in
		fun ag_ty -> try Smap.find ag_ty map with Not_found -> default_order in
		
	let ag_weight ag_id = 
		sites_list (ty_of_id ag_id)
		|> List.map (fun s -> List.length (neigh (ag_id, s)))		 
		|> Util.sum_list in
	
	let ags = 
		agent_types_map |> Imap.to_list |> List.map (fun (ag_id, ag_ty) ->
			(ag_priority ag_ty, (ag_weight ag_id, ag_id)) ) in
		
	let cmp = 
		Util.make_lex_compare compare 
			(Util.make_lex_compare compare compare) in
		
	(* Order in which agents are processed *)
	let ags = ags |> List.sort cmp  |> List.map (fun (_, (_, id)) -> id) in
	
	
	
	(*  Point.t * site_pos_constrs  *)
	let t = Hashtbl.create 100 in
	
	let process ag_id = 
	
		let ag_ty = ty_of_id ag_id in
		let ag_pos_constrs = ag_pos_constrs ag_ty in
	
		(* Searching for an anchor : a agent linked to it and which is 
		   not positioned yet *)
		   
		try
			sites_list ag_ty |> List.iter (fun s ->
			
				neigh (ag_id, s) |> List.iter (fun (dest_id, dest_s) ->
					if Hashtbl.mem t dest_id then 
						raise (Found_anchor (s, (dest_id, dest_s)))
				)
			) ;
			
			(* No anchor is found : put to the origin *)
			Hashtbl.add t ag_id (Point.origin, ag_pos_constrs) ;
			
			
		with Found_anchor (from_s, (dest_id, dest_s)) -> begin
		
			let (anch_pos, anch_sites_angl) = Hashtbl.find t dest_id in
			let anch_angl = List.assoc dest_s anch_sites_angl in
			
			let pos = Point.add anch_pos 
				(Point.dir anch_angl |> Point.scale (params.ag_dist)) in
				
			let cur_angl = List.assoc from_s ag_pos_constrs in
			let wanted_angl = anch_angl +. 180. in
			let delta_angl = wanted_angl -. cur_angl in
			
			(* printf "[%d] anchored to [%d] by site [%s] \n" ag_id dest_id dest_s ; *)
				
			Hashtbl.add t ag_id (pos, rotate delta_angl ag_pos_constrs)
		
		end in
	
	
	List.iter process ags ;
	fun ag_id -> Hashtbl.find t ag_id
		   
		 
	
	
		



let render ~filename pos_constrs mixture params = 

	let port_to_label (i, p) = sprintf "%d_%s" i p in

	
	let site_box (ag_id, ag_ty, ag_diam, s_name, site_angle) = 
		let s_params = params.site_params ag_id ag_ty s_name in
		let orig = (Point.scale (multf 0.5 ag_diam) (dir site_angle)) in
		let path = Path.shift orig (Shapes.circle params.site_diam) in
		let cmd = seq [Path.fill ~color:white path ; Path.draw path] in
		Box.pic ~name:(port_to_label (ag_id, s_name)) cmd in
		
	
	let agent_box (ag_id, ag) = 
	
		let (ag_pos, sites_angles) = pos_constrs ag_id in
		let ag_params = params.ag_params ag_id ag.ty in
	
		(* For each  site : (name, angle) *)
		let sites = ag.sites |> List.map (fun s ->
			(ag_id, ag.ty, ag_params.ag_diam, s.name, List.assoc s.name sites_angles) )
			|> List.map site_box in
			
			
		let agent_body = Box.path (Shapes.circle ag_params.ag_diam) in
		
		Box.shift ag_pos (Box.group ([agent_body] @ sites)) in
		
		
	let ags = Box.group (mixture |> Imap.to_list |> List.map agent_box) in
	let cmd = Box.draw ags in
	
	Metapost.emit filename cmd ;
	Metapost.dump 
           (*~prelude:(Metapost.read_prelude_from_tex_file "main.tex")*)
           ~pdf:true filename
	










