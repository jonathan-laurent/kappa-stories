(* *)

open Util
open Lens
open Signature


type signature       = Signature.t
type site_signature  = Signature.Site.t
type agent_signature = Signature.Agent.t

type agent_id        = int
type agent_ty        = Signature.Agent.ty
type site_id         = Signature.Site.id
type site_int_state  = Signature.Site.int_state option
type port_ty         = Signature.port_ty
type port_id         = (agent_id * site_id)

module PortMap = ExtMap (
	struct
		type t = port_id
		let compare = compare
	end)



module Link = struct

	include UnorderedPair (struct 
		type t = port_id 
		let compare = compare
	end)
	   
	let ports = to_ordered_pair
	
end

module Site = struct
	
	type t = {
		int_state : int_state ;
		lnk_state : lnk_state ;
		parent    : agent_id ;
		signature : site_signature 
	}
	
	and int_state = site_int_state
	
	and lnk_state = 
		| Free
		| Any
		| Bound_some
		| Bound_ty of port_ty
		| Bound    of port_id
		
		
	(* This lens should NOT be accessible from outside :
	 * A bad modification would put the SG in an inconsistet state *)
	 
	let lnk_state = {
		get_ = (fun s -> s.lnk_state) ;
		set_ = (fun v s -> {s with lnk_state = v})
	}
	
	let int_state = {
		get_ = (fun s -> s.int_state) ;
		set_ = (fun v_opt s -> 
			(*assert ( v_opt |> Option.map_default true (fun is ->
				is = Signature.Site.def_int_state 
				|| NTable.is_valid_id is s.signature.Signature.Site.states)) ;
				*)
			{s with int_state = v_opt} )
	}
	

	let port_id s = (s.parent, s.signature |> Signature.Site.id)
	
	let create parent sign = 
		{ int_state = None ;
		  signature = sign ;
		  lnk_state = Free ;
		  parent = parent 
		}
		
	let link s = match s.lnk_state with
		| Bound target -> Some (Link.make (port_id s) target)
		| _ -> None
		
	let signature s = s.signature

end	


module SIdMap = Map.Make(struct type t = site_id let compare = compare end)
type 'a site_id_map = 'a SIdMap.t

module Agent = struct

	type t = { 
		id : agent_id ; 
		signature : agent_signature ; 
		sites : Site.t site_id_map 
	}
	
	let site_lens id = {
		get_ = (fun a -> 
			assert (SIdMap.mem id a.sites) ;
			SIdMap.find id a.sites ) ;
			
		set_ = (fun v a -> 
			{a with sites = SIdMap.add id v a.sites} )
	}
	
	let site = site_lens
	
	let create id sign = {
		id = id;
		signature = sign ; 
		sites = SIdMap.empty
	}
	
	
	let add_site site_id ag = 
	
		let site_sign = ag.signature |> Signature.Agent.site site_id in
		let new_site = Site.create ag.id site_sign in
		
		(site_lens site_id ^= new_site) ag

    let has_site s_id ag = SIdMap.mem s_id ag.sites
		
		
	
	let signature a = a.signature
	
	let fold f a init = SIdMap.fold (fun key s acc -> f s acc) a.sites init
	
	let iter f a = fold (fun s _ -> f s) a ()
	
	let id t = t.id
	
end







(* Definition of a site-graph *)

type agent = Agent.t
type site = Site.t
type link = Link.t


type t = {
	signature : signature ;
	agents : agent int_map ;
	next_agent_id : int
}

type site_graph = t

let signature sg = sg.signature

let agent id = {
	get_ = (fun sg -> 
			assert (Imap.mem id sg.agents) ;
			Imap.find id sg.agents ) ;
			
	set_ = (fun v sg -> 
			assert (Imap.mem id sg.agents) ;
			{sg with agents = Imap.add id v sg.agents})
}

let port (agent_id, site_id) = (agent agent_id |-- Agent.site site_id)


let agent_exists id sg = Imap.mem id sg.agents

let add_agent ?id ty sg = 

	let id, next_id = match id with
		| None -> sg.next_agent_id, sg.next_agent_id + 1
		| Some id -> assert (not (agent_exists id sg)) ; 
					 id, max (id + 1) (sg.next_agent_id)
	in

	let sign = sg.signature |> Signature.agent ty in
	let ag = Agent.create id sign in

	({ sg with
		next_agent_id = next_id ;
		agents = Imap.add id ag sg.agents
	 }, id)


let create sign = {
	signature = sign ;
	agents = Imap.empty ;
	next_agent_id = 0
}


let fold_agents f sg init = Imap.fold (fun key ag acc -> f ag acc) sg.agents init

let fold_ports f sg init = 
	let process_agent ag acc = Agent.fold f ag acc in
	fold_agents process_agent sg init

let iter_ports f sg = 
  fold_ports (fun s _ -> f s) sg ()


(* WARNING : Border effect *)

let remove_agent id sg =
	
	let process_port (p : Site.t) (sg_acc : t) : t = 
		match p |. Site.lnk_state with
			| Site.Bound ((dst, _) as dst_p) when dst = id -> 
				( (port dst_p |-- Site.lnk_state ) ^= Site.Free ) sg_acc
				
			| _ -> sg_acc
	in
	
	let unsafe_remove sg_acc = {sg_acc with agents = Imap.remove id sg_acc.agents} in
	
	unsafe_remove (fold_ports process_port sg sg)
	


let link_exists lnk sg = 

	let (p, p') = Link.ports lnk in
	
	match sg |. (port p |-- Site.lnk_state) with
		| Site.Bound p'' -> p' = p''
		| _ -> false

let are_bound p p' sg = link_exists (Link.make p p') sg

let unsafe_remove_link lnk sg = 

	let free_port p sg = ((port p |-- Site.lnk_state) ^= Site.Free) sg in
	let (p, p') = Link.ports lnk in
	(free_port p |- free_port p') sg

let remove_link lnk sg = 

	if link_exists lnk sg then unsafe_remove_link lnk sg else sg



let unbind p p' sg = remove_link (Link.make p p') sg


let unbind_semi p sg = 

	let p_lnk_state = port p |-- Site.lnk_state in
	
	match sg |. p_lnk_state with
		| Site.Bound p' -> unsafe_remove_link (Link.make p p') sg
		| _ -> (p_lnk_state ^= Site.Free) sg
		

let add_link lnk sg = 

	let (p, p') = Link.ports lnk in
	let sg = (unbind_semi p |- unbind_semi p') sg in
	
	let add_dir p p' sg = ((port p |-- Site.lnk_state) ^= Site.Bound p') sg in
	
	(add_dir p p' |- add_dir p' p) sg
	
	
let bind p p' sg = add_link (Link.make p p') sg

let port_ty (ag_id, site_id) sg =
  let ag_ty = sg |. agent ag_id |> Agent.signature |> Signature.Agent.ty in
  (ag_ty, site_id)
