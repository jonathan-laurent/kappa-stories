open SiteGraph
open Util
open Model
open Lens

type int_state = Signature.Site.int_state

type 'a port_id_map = 'a PortMap.t

type lnk_state = 
	| Free
	| Bound of port_id


type feature =
	| BoundTo of port_id * port_ty


(* agents = (tested_agents) U (created_agents) *)

type rule = {
	id           : int ;
	name         : string ;
	features     : feature list ;
	
	agents       : agent_ty int_map ;
	created      : int_set ;
	deleted      : int_set ;
	
	tested_ag    : int_set ;
	
	tested_int_states : int_state port_id_map ;
	tested_lnk_states : lnk_state port_id_map ;
	
	mod_int_states : int_state port_id_map ;
	mod_lnk_states : lnk_state port_id_map ;
}


(* Event : rule + renaming of agents *)



(* If an site can have only one state, its 'int' variable never appears *)

let compile (sign : Signature.t) (r : Model.rule) : rule = 

	(* Recovers the list of identifiers involved in both sides *)
	
	let log_agents sg acc = 
		fold_agents (fun ag acc ->
			let ty = ag |> Agent.signature |> Signature.Agent.ty in
			let id = ag |> Agent.id in
			Imap.add id ty acc
		) sg acc in
	
	let agents = Imap.empty |> log_agents r.lhs |> log_agents r.rhs in
	
	let created            = ref (ISet.empty) in
	let deleted            = ref (ISet.empty) in
	let tested_int_states  = ref (PortMap.empty) in
	let tested_lnk_states  = ref (PortMap.empty) in
	let mod_int_states     = ref (PortMap.empty) in
	let mod_lnk_states     = ref (PortMap.empty) in
	
	let addm (k : int * Signature.Site.id) v m = m := PortMap.add k v !m     in
	let adds e s   = s := ISet.add  e   !s     in
	
	(* When an agent is created, the states of its sites have to
	   be initialized to their default values, and their linking state to free *)
	let init_agent id =
		let ty = Imap.find id agents in
		let ag_sign = sign |> Signature.agent ty in
		ag_sign |> Signature.Agent.iter (fun s_sign ->
			let module SS = Signature.Site in
			let port = (id, SS.id s_sign) in
			if SS.has_states s_sign then 
				addm port SS.def_int_state mod_int_states ;
			addm port Free mod_lnk_states
		) in
		
		
	(* Iter the states of the variables of an agent. If an assignment is not
	   encountered in [%_old], then it is added in [%] *)
	let process_agent ints ints_old lnks lnks_old ag =
	 
		let add_if_new k v t t_old = 
			if not (PortMap.mem_binding (k, v) !t_old) then
				addm k v t in
				
		ag |> Agent.iter (fun s ->
			let port_id = s |> Site.port_id in
			(match s |. Site.int_state with
				| None    -> ()
				| Some st -> add_if_new port_id st ints ints_old					
			);
			(match s |. Site.lnk_state with
				| Site.Free -> add_if_new port_id Free lnks lnks_old
				| Site.Any  -> ()
				| Site.Bound dst -> add_if_new port_id (Bound dst) lnks lnks_old
				| _ -> assert false (* Not implemented *) 
			)
		) in
		
	let test_agent = process_agent 
		tested_int_states
		(ref PortMap.empty)
		tested_lnk_states
		(ref PortMap.empty) in
	
	let mod_agent = process_agent
		mod_int_states
		tested_int_states
		mod_lnk_states
		tested_lnk_states in
		
		
	let compile_agent id = 
		match agent_exists id r.lhs, agent_exists id r.rhs with
			(* agent is transformed *)
			| true, true -> 
				begin
					test_agent (r.lhs |. agent id) ;
					mod_agent  (r.rhs |. agent id)
				end		
			
			(* agent is destroyed *)
			| true, false  -> 
				(* TODO : handle side effects *)
				adds id deleted
				
			| false, true  -> 
				let ag = r.rhs |. agent id in
				begin
					adds id created ; 
					init_agent id ; 
					test_agent ag
				end
			| false, false -> assert false in
			
	
	let ag_ids_set = agents |> Imap.to_list |> List.map fst |> iset_of_list in
	
	ISet.iter compile_agent ag_ids_set ;
	
	{ id           = 0 ;
	  name         = r.name ;
	  features     = [] ;
	
	  agents       = agents ;
	  created      = !created ;
	  deleted      = !deleted ;
	  
	  tested_ag    = ISet.diff (ag_ids_set) !created ;

	  tested_int_states = !tested_int_states ;
	  tested_lnk_states = !tested_lnk_states ;
	
	  mod_int_states = !mod_int_states ;
	  mod_lnk_states = !mod_lnk_states }
		
		



let print fmt sign r = 
	let open Format in
	let open Signature in
	let pr msg = fprintf fmt msg in
	
	
	let str_of_port (id, site) = 
		let ag_ty = Imap.find id r.agents in
		let name = sign |> agent ag_ty |> Agent.site site |> Site.name in
		sprintf "(%d, %s)" id name in
		
	let str_of_int_state (id, site) st = 
		let ag_ty = Imap.find id r.agents in
		sign |> agent ag_ty |> Agent.site site |> Site.name_of_int_state st in
		
	let str_of_lnk_state = function
		| Free -> "free"
		| Bound p -> "bound" ^ (str_of_port p) in
		
	pr "%d : '%s'\n" r.id r.name ;
	pr "Agents :" ;
	r.agents |> Imap.iter (fun id ty -> 
		let ty_s = sign |> agent ty |> Agent.ty_name in
		pr " (%d : %s) " id ty_s
	);
	pr "\n" ;
	pr "Created :" ; 
	r.created |> ISet.iter (fun id -> pr " %d " id) ;
	pr "\n" ;
	pr "Deleted :" ;
	r.deleted |> ISet.iter (fun id -> pr " %d " id) ;
	pr "\n" ;
	
	let print_vars ints lnks =
		ints |> PortMap.iter (fun p st ->
			pr " int%s = %s\n" (str_of_port p) (str_of_int_state p st)
		) ;
		lnks |> PortMap.iter (fun p st ->
				pr " lnk%s = %s\n" (str_of_port p) (str_of_lnk_state st)
		) in
		
	pr "Tested :\n" ; 
	print_vars r.tested_int_states r.tested_lnk_states ;
	pr "Modified :\n" ; 
	print_vars r.mod_int_states r.mod_lnk_states ;
	pr "\n"
	(*pr "%d\n" (PortMap.cardinal r.mod_int_states)*)


