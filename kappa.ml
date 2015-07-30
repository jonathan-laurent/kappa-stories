(** Representation of a Kappa model suited for static generation of stories. *)

open Util

type agent_id    = SiteGraph.agent_id
type agent_type  = SiteGraph.agent_ty
type site_id     = SiteGraph.site_id
type int_state   = Signature.Site.int_state
type signature   = Signature.t

type var_type = 
| Ex
| Int of site_id
| Lnk of site_id

type var = agent_id * var_type

type 'ag generic_trait = 
| Agent     of 'ag * bool
| Free      of ('ag * site_id) 
| Link      of (('ag * site_id) * ('ag * site_id))
| IntState  of ('ag * site_id) * int_state

type trait_type  =  agent_type generic_trait
type trait       =  agent_id   generic_trait

let var_agent (a, _) = a
let set_var_agent a (_, f) = (a, f) 

let trait_vars = function
  | Agent (a, _) -> [(a, Ex)]
  | Free (a, s) -> [(a, Lnk s)]
  | Link ((a, s), (a', s')) -> [(a, Lnk s); (a', Lnk s')]
  | IntState ((a, s), _) -> [(a, Int s)]


let mup x y = 
  if x <= y then (x, y) else (y, x)

module Trait = struct
  type t = trait
  let compare = compare
end

type rule_id = int

type rule = {
  id        : rule_id ;
  name      : string ;
  signature : signature ;

  agents    : agent_type int_map ;
  tested    : int_set ; 
  created   : int_set ;
  deleted   : int_set ;

  pres      : trait array ;
  effs      : trait array ;  
}

let rule_id r = r.id
let rule_name r = r.name
let is_created a r = ISet.mem a r.created
let iter_effects f r = Array.iter f r.effs
let iter_preconditions f r = Array.iter f r.pres
let iter_agents f r = Imap.iter (fun aid _ -> f aid) r.agents


let trait_type t r = 
  let agent_type id = Imap.find id r.agents in
  match t with
  | Agent (a, b) -> Agent (agent_type a, b)
  | Free (a, s) -> Free (agent_type a, s)
  | Link ((a, s), (a', s')) -> Link ((agent_type a, s), (agent_type a', s'))
  | IntState ((a, s), f) -> IntState ((agent_type a, s), f)




let compile (sign : Signature.t) (id : rule_id) (r : Model.rule) =

  let open SiteGraph in
  let open Lens in
  let open Model in

  (* Recovers the list of identifiers involved in both sides *)
  
  let log_agents sg acc = 
	fold_agents (fun ag acc ->
	  let ty = ag |> Agent.signature |> Signature.Agent.ty in
	  let id = ag |> Agent.id in
	  Imap.add id ty acc
	) sg acc in
  
  let agents = Imap.empty |> log_agents r.lhs |> log_agents r.rhs in

  let module TSet = Set.Make 
        (struct
          type t = trait
          let compare = compare
        end) in

  let created = ref (ISet.empty) in
  let deleted = ref (ISet.empty) in

  let pres = ref TSet.empty in
  let effs = ref TSet.empty in

  let addi x s = s := ISet.add x !s in
  let addt x s = s := TSet.add x !s in

  
  (* When an agent is created, the states of its sites that are not mentioned have to
	 be initialized to their default values, and their linking state to free *)
  let init_agent ag =
    let id = Agent.id ag in
	let ty = Imap.find id agents in
	let ag_sign = sign |> Signature.agent ty in
	ag_sign |> Signature.Agent.iter (fun s_sign ->
	  let module SS = Signature.Site in
	  let port = (id, SS.id s_sign) in
      if not (Agent.has_site (Signature.Site.id s_sign) ag) then
        begin
	      if SS.has_states s_sign then 
            addt (IntState (port, SS.def_int_state)) effs ;
          addt (Free port) effs
        end
	) in
  
  
  (* Iter the states of the variables of an agent. If a trait is not
	 encountered in [t_old], then it is added in [t] *)
  let process_agent traits old_traits ag =
	
    let add_if_new t =
      if not (TSet.mem t !old_traits) then
        addt t traits in
	
	ag |> Agent.iter (fun s ->
	  let port_id = s |> Site.port_id in
	  (match s |. Site.int_state with
	  | None    -> ()
	  | Some st -> add_if_new (IntState (port_id, st))
	  );
	  (match s |. Site.lnk_state with
	  | Site.Free -> add_if_new (Free port_id)
	  | Site.Any  -> ()
	  | Site.Bound dst -> add_if_new (Link (mup port_id dst)) ;          
	  | _ -> assert false (* Not implemented *) 
	  )
	) in

  let test_agent = process_agent pres (ref TSet.empty) in
  
  let mod_agent = process_agent effs pres in
  
  let compile_agent id = 
	match agent_exists id r.lhs, agent_exists id r.rhs with
	(* agent is transformed *)
	| true, true -> 
	  begin
		test_agent (r.lhs |. agent id) ;
		mod_agent  (r.rhs |. agent id)
	  end		
		
	(* agent is destroyed *)
	| true, false  -> assert false
	  (* TODO : handle side effects *)
	  (* addi id deleted *)
		
	| false, true  -> 
	  let ag = r.rhs |. agent id in
	  begin
		addi id created ; 
		init_agent ag ; 
		mod_agent ag
	  end
	| false, false -> assert false in


  let add_agent_trait b set ag = 
    addt (Agent (ag, b)) set in

  let symmetrize_link set = function
    | Link (src, dest) -> 
      if src <> dest then
        addt (Link (dest, src)) set 
    | _ -> () in
        
  
  let ag_ids_set = agents |> Imap.to_list |> List.map fst |> iset_of_list in
  
  ISet.iter compile_agent ag_ids_set ;

  let tested = ISet.diff ag_ids_set !created in
  
  (* Adding symmetrical links *)
  TSet.iter (symmetrize_link effs) !effs ;

  (* Adding agent existence traits *)
  ISet.iter (add_agent_trait true  pres) tested   ;
  ISet.iter (add_agent_trait false effs) !deleted ;
  ISet.iter (add_agent_trait true effs)  !created ;

  let array_of_set = TSet.elements |- Array.of_list in
  
  { id           = id ;
	name         = r.name ;
    signature    = sign ;
	
	agents       = agents ;
	created      = !created ;
	deleted      = !deleted ;
    tested       = tested ;
	
    pres = array_of_set !pres ;
    effs = array_of_set !effs ;
  }




let print_trait r fmt =
  let open Signature in
  let pr msg = Format.fprintf fmt msg in
  let sign = r.signature in

  let str_of_port (id, site) = 
    let ag_ty = Imap.find id r.agents in
    let name = sign |> agent ag_ty |> Agent.site site |> Site.name in
    sprintf "(%d, %s)" id name in
  
  let str_of_int_state (id, site) st = 
	let ag_ty = Imap.find id r.agents in
	sign |> agent ag_ty |> Agent.site site |> Site.name_of_int_state st in

  function
    | Agent (a, true)  -> pr "ex %d"       (a)
    | Agent (a, false) -> pr "del %d"      (a)
    | Free p           -> pr "free %s"     (str_of_port p)
    | Link (src, dest) -> pr "bound %s %s" (str_of_port src) (str_of_port dest)
    | IntState (p, st) -> pr "int %s = %s" (str_of_port p)   (str_of_int_state p st)




(* Printing utilities *)

let print sign fmt r =
  let open Format in
  let open Signature in

  let pr msg = fprintf fmt msg in

  let print_trait t = 
    print_trait r fmt t ;
    pr "@;" in

  pr "@[<v>" ;
  pr "[%d] %s@;%s@;" r.id r.name Log.dline ;
  pr "Agents :" ;
  r.agents |> Imap.iter (fun id ty -> 
	let ty_s = sign |> agent ty |> Agent.ty_name in
	pr " (%d : %s) " id ty_s
  );
  pr "@;" ;
  pr "Created :" ; 
  r.created |> ISet.iter (fun id -> pr " %d " id) ;
  pr "@;" ;
  pr "Deleted :" ;
  r.deleted |> ISet.iter (fun id -> pr " %d " id) ;
  pr "@;" ;
  pr "@[<v 2>Preconditions :@;" ; 
  r.pres |> Array.iter print_trait ;
  pr "@]@;" ;
  pr "@[<v 2>Effects :@;" ; 
  r.effs |> Array.iter print_trait ;
  pr "@]@;@;@]@."
	
