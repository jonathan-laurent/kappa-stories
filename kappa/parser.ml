(* In the whole file, 'tr' stands for 'translate' *)

open Model
open Ast
open List
open Lens
open Util

module SG   = SiteGraph
module Sig  = Signature


(* Builds the common signature of all the site graphs of the model
 * from the %agent declaration list of a Kappa file *)

let tr_signature (ast_agents : Ast.agent list) = 
	
	let site_builder ast_site = 
		(fst ast_site.port_nme, List.map fst ast_site.port_int) in
	
	let agent_builder (name_wp, ast_sites) = 
		(fst name_wp, List.map site_builder ast_sites) in
	
	let builder = List.map agent_builder ast_agents in
	
	Signature.create builder
	


(* This structure maps a link id to a list of port ids
 * involved in this link. When the structure is built, we expect
 * each list to contain exactly 2 elements *)

type lnk_id = int

type lnks_acc = (SG.port_id list) int_map

let empty_links_acc = Imap.empty

let register_link (p : SG.port_id) (l : lnk_id) acc = 
	Imap.add l (p :: (Imap.lookup_def ~def:[] l acc)) acc
	
	


(* Each agent of the ast is decorated with an [int option]. If [Some id]
   is attached, it forces the agent to receive the identifier [id].
   We use this so when generating the lhs and the rhs of 
   a rule, preserved agents share the same id *)

let tr_site_graph sign (ast_sg : (Ast.agent * int option) list) =

	let tr_site ag_id ag_sign (ast_site : Ast.port) (sg, lnks) = 
		
		let site_name = fst (ast_site.port_nme) in
		
		let site_sign = ag_sign |> Sig.Agent.site_named site_name in
		
		let site_id = site_sign |> Sig.Site.id in
		
		(* Creates the site *)
		let sg = (SG.agent ag_id ^%= SG.Agent.add_site site_id) sg in
		let port_id = (ag_id, site_id) in
		
		(* Sets its internal state *)
		let sg = match ast_site.port_int with
			| [] -> sg
			| int_wp :: [] -> 
				let st = Some (site_sign |> Sig.Site.int_state_named (fst int_wp)) in			
				((SG.port port_id |-- SG.Site.int_state) ^= st) sg
			| _ -> failwith "invalid ast : two sites were given to a single agent" in
		
		
		(* Sets its linking state *)
		let update_lnk_state st = 
			((SG.port port_id |-- SG.Site.lnk_state) ^= st) sg, lnks in
	
		match fst (ast_site.port_lnk) with
			| LNK_SOME    -> update_lnk_state SG.Site.Bound_some
			| LNK_ANY     -> update_lnk_state SG.Site.Any
			| FREE        -> sg, lnks
			| LNK_VALUE l -> sg, register_link port_id l lnks
			
			| LNK_TYPE (p, a) -> 
			
				let port_ty = Signature.port_ty_named (fst p, fst a) sign in
				update_lnk_state (SG.Site.Bound_ty port_ty) in


	(* [id_opt] : [None] if the identifier can be automatically chosen, 
	 * [Some id] if [id] has to be used *)
	
	let tr_agent ((ty_wp, ast_sites), id_opt) (sg, lnks) =

		let ag_sign = sign |> Sig.agent_named (fst ty_wp) in
		
		let (sg, ag_id) = sg |> SG.add_agent ?id:id_opt (Sig.Agent.ty ag_sign) in
		
		List.fold_right (tr_site ag_id ag_sign) ast_sites (sg, lnks) in
		
	
	let sg = SiteGraph.create sign in
	
	let sg, (lnks : lnks_acc) = List.fold_right tr_agent ast_sg (sg, empty_links_acc) in
	
	let process_link _ l sg = match l with
		| p :: p' :: [] -> SG.bind p p' sg
		| _ -> failwith "wrong link id"
		
	in
	
	Imap.fold process_link lnks sg




(* The longest prefixe rule is used *)

(* [longest_common_prefix eq (l, l') = (pl, tl), (pl', tl')] with 
   [l = pl @ tl] and [l' = pl' @ tl'] and [pl] is the longest prefix common 
   to [l] and [l'] for the relation [eq]  
*)
let rec longest_common_prefix eq = function
	| x::xs, y::ys when eq x y ->
		let (pl, tl), (pl', tl') = longest_common_prefix eq (xs, ys) in
		(x::pl, tl), (y::pl', tl')
	| l, l' -> ([], l), ([], l')



let tr_rule sign (rule_name_wp_opt, rule_wp) =

	let rule = fst rule_wp in 

	let are_compatible (nm_wp, sites) (nm_wp', sites') = 
		let site_name p = fst (p.port_nme) in
		(fst nm_wp) = (fst nm_wp')
		&& (List.map site_name sites) = (List.map site_name sites') in
	
	let (p_lhs, t_lhs), (p_rhs, t_rhs) = 
		longest_common_prefix are_compatible (rule.lhs, rule.rhs) in
		
	let label_agents labf ags = List.map (fun ag -> (ag, Some (labf ()))) ags in
	
	let counter = Util.new_counter 1 in
	let lhs = label_agents counter (p_lhs @ t_lhs) in
	let rhs = label_agents (Util.new_counter 1) p_rhs
			@ label_agents counter t_rhs in
			
	{ lhs = tr_site_graph sign lhs ;
	  rhs = tr_site_graph sign rhs ;
	  name = match rule_name_wp_opt with
	  	| None -> "Please give a name to ALL your rules, dude..."
	  	| Some n_wp -> fst n_wp
	}



let tr_rules sign ast = List.map (tr_rule sign) ast.rules



(* Keeps only the observables whose expression is of the form |X| where X is 
   a pattern *)

let tr_observables sign ast =

	let tr_vardef (name_wp, expr_wp) = 
		match fst expr_wp with
			| KAPPA_INSTANCE mixt ->
				(* We want the ids of the agents to be decided automatically *)
				let mixt = List.map (fun a -> a, None) mixt in
				Some (fst name_wp, tr_site_graph sign mixt)
			| _ -> None in
	
	Smap.of_list (Util.map_and_filter tr_vardef ast.variables)



let tr_init_decl name_gen sign (name_wp_opt, init_t , _) = match init_t with
	| INIT_MIX (_, mixt) -> 
	
		let mixt = List.map (fun a -> a, None) mixt in
	
		let rule = { 
			name = Option.map_default (name_gen ()) fst name_wp_opt ;
			lhs  = SiteGraph.create sign ;
			rhs  = tr_site_graph sign mixt } in
	
		Some { max_instances = Unlimited ; init_rule = rule }

	| INIT_TOK _ -> None
	
	
let tr_init sign ast = 
	
	let name_gen = 
		let get_fresh = Util.new_counter 1 in
		fun () -> sprintf "init_%d" (get_fresh ()) in
		
	Util.map_and_filter (tr_init_decl name_gen sign) ast.init
		


let tr_model ast = 
	let sign = tr_signature ast.signatures in

	{ signature   = sign ;
	  observables = tr_observables sign ast ;
	  init        = tr_init sign ast ;
	  rules       = tr_rules sign ast ;
	}
	

let parse_files files = 
	Ast.init_compil() ;
	List.iter (fun f ->
		KappaLexer.compile Format.std_formatter f)
		files ;
	tr_model (!Ast.result)


