type 'a ntable = 'a NTable.t

module Site = struct

	type id             = NTable.id
	type name           = string
	type int_state      = NTable.id
	type int_state_name = string
	
	type builder = string		(* name of the site *)
				 * string list  (* name of its states *)

	type t = {
		id : id ;
		name : name ;
		states : unit ntable
	}
	
	let create id (name, states) = 
		{ id      = id ;
		  name    = name ;
		  states  = 
		  	let make_gen name = (name, fun _ -> ()) in
		  	NTable.create (List.map make_gen states)
		}
	
	let id     t = t.id
	let name   t = t.name
	let states t = t.states
	
	let def_int_state = NTable.id_zero
	
	let has_states t = not (NTable.is_empty t.states)
	
	let name_of_int_state id sign = NTable.name_of_id id sign.states
	
	let int_state_named name sign = NTable.id_of_name name sign.states

end


module Agent = struct

	type ty = NTable.id
	type ty_name = string
	
	type builder = string			  (* type name *)
				 * Site.builder list  (* sites *)
	
	type t = {
		ty : ty ;
		ty_name : string ;
		sites : Site.t ntable
	}
	
	(*  (string, string list) list *)
	
	let create ty_id (ty_name, site_builders)  = 
		{ ty      = ty_id ;
		  ty_name = ty_name ;
		  sites   = 
		  	let make_gen ((site_name, _) as builder) = 
		  		(site_name, fun site_id -> Site.create site_id builder) in
		  
		  	NTable.create (List.map make_gen site_builders)
		}

	
	let ty      t = t.ty
	let ty_name t = t.ty_name
	let sites   t = t.sites
	
	let site site_id ag_sign = 
		ag_sign.sites |> NTable.read site_id

	let site_named site_name ag_sign = 
		ag_sign.sites |> NTable.read_named site_name
		
	let iter f t = NTable.iter f t.sites
	
end


type t = Agent.t ntable

type builder = Agent.builder list

let create agent_builders = 
	let make_gen ((name, _) as builder) = 
		(name, fun id -> Agent.create id builder) in
	NTable.create (List.map make_gen agent_builders)



let agent ag_ty sign = NTable.read ag_ty sign

let agent_named ag_name sign = NTable.read_named ag_name sign

type port_ty = Agent.ty * Site.id
type port_ty_name = Agent.ty_name * Site.name


let port_ty_named (ag_ty_name, site_name) sign = 

	let ag_ty = sign |> NTable.id_of_name ag_ty_name in
	let site_id = sign |> agent ag_ty |> Agent.site_named site_name |> Site.id in
	(ag_ty, site_id)


let port_ty_name (ag_ty, site_id) sign = 

	let ag_name   = sign    |> NTable.name_of_id ag_ty in
	let ag_sign   = sign    |> NTable.read ag_ty in
	let site_name = ag_sign |> Agent.sites |> NTable.name_of_id site_id in
	
	(ag_name, site_name)









