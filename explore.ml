(** Static generation of stories.
    The provided algorithm can be applied on a wider range of systems than Kappa
    models, described by the signature [AgentBased.System]. *)

module Make (S : AgentBased.System) = struct

  module AbsUtils = AgentBased.Utils  (S)
  module PStory   = PartialStory.Make (S)
  module Story    = Story.Make        (S)

  open S
  open Util
  open Lens
  open AbsUtils
  open PStory
  open Story


  (* To raise when the current partial story is inconsistent.
     Caught by [BranchAndCut] *)
  exception Bottom

  (* To raise when the observable is unreachable and so there is no story *)
  exception Unreachable

  exception Great


  (** Creating and manipulating environments *)

  (* A closure which maps a type of trait
     to the list of its possible origins, an 'origin' being given by a
     rule and one of its effect.  *)
  type traits_dict = trait_type -> source_ty list

  type environment = {
    rules       : rule int_map ;
    tdict       : traits_dict ;
    stories     : story list ;
    size_limit  : int ;
    next_id     : int ;
  }

 let compile_traits (rs : rule list) : traits_dict = 
	let table = Hashtbl.create 1000 in
	rs |> List.iter (fun r ->
	  r |> iter_effects (fun eff ->
		Hashtbl.add table (trait_type eff r) (rule_id r, eff)
	  )
	) ;
	fun t_ty -> Hashtbl.find_all table t_ty	  

  let generate_id env = 
    let id = env.next_id in
    id, { env with next_id = id + 1 }


  let environment ?(size_limit=20) rules  = {
    rules = Imap.of_list (List.map (fun r -> (rule_id r, r)) rules) ;
    tdict = compile_traits rules ;
    stories = [] ;
    size_limit = size_limit ;
    next_id = 0 ;
  }

  let env_rule env rid = Imap.find rid env.rules


  let print env fmt ps = (print_pstory (env_rule env)) fmt ps

  let print' env ps = Log.print (print_pstory (env_rule env)) ps


  (** Creating ndoes and partial stories *)

  let init_branches env rule =

    let gen_id = Util.new_counter 0 in

    let process_trait pre acc =
      let trait_ty = trait_type pre rule in
      let sources = env.tdict trait_ty in
      let new_items = sources |> List.map (fun s ->
        { id = gen_id () ;
          trait = pre ;
          source_ty = s ;
          impl = [] ;
          excl = [] ;
        }
      ) in
      acc @ new_items in

    let items =
      rule |> fold_preconditions process_trait [] |> Array.of_list in

    let choices = items |> Array.fold_left (fun acc it ->
      ItSetPreMap.add it.trait it.id acc
    ) ItSetPreMap.empty in

    let remaining =
      rule |> fold_preconditions TrSet.add TrSet.empty in

    let item_disqualified = ISetImap.empty in

    {items ; choices ; remaining ; item_disqualified }



  (* Returns a new node instanciating [rule], with identifier [id] *)
  let init_node env rule_id id = 
    let rule = Imap.find rule_id env.rules in
    let incoming_arrows = TrMap.empty in
    let outgoing_arrows = [] in
    let branches = init_branches env rule in         
    {id ; rule ; incoming_arrows ; outgoing_arrows ; branches}



  let initial_pstory env id rule_id = 

    let ps = 
      { id = id ;
        cost = Cost.default ;
        nodes = Imap.empty ;
        agent_eqv = AgEqv.empty ;
        rules_instances = ISetImap.empty ;
        next_node_id = 0 }  in

    snd (add_node (init_node env rule_id) ps)





  (** Consistency checks *)


  let is_agent_consistent ps = 
    (*Log.put (sprintf "Agents are inconsistent : %b" (AgEqv.is_bottom ps.agent_eqv)) ;
    AgEqv.dump Format.std_formatter ps.agent_eqv ;*)
    not (AgEqv.is_bottom ps.agent_eqv)


  let scheduling_constrs ps nodes arrows =
    let writtenBy = Hashtbl.create 100 in
    let onArrow   = Hashtbl.create 100 in

    nodes |> List.iter (fun n ->
      n.rule |> iter_effects (fun e ->
        Eqv.gvars_of_gtrait n.id ps.agent_eqv e |> List.iter (fun v ->
          Hashtbl.add writtenBy v n.id
        )
      )
    ) ;

    arrows |> List.iter (fun ((srcn, eff), (destn, pre)) ->
      let gvars = zip 
        (Eqv.gvars_of_gtrait srcn ps.agent_eqv eff)
        (Eqv.gvars_of_gtrait destn ps.agent_eqv pre) in
      gvars |> List.iter (fun (v, v') ->
        assert (v = v') ;
        Hashtbl.add onArrow v (srcn, destn)
      )
    ) ;

    let prec_constrs = 
      arrows |> List.map (fun ((srcn, _), (destn, _)) -> (srcn, destn)) in

    let int_constrs = 
      let q = Queue.create () in
      onArrow |> Hashtbl.iter (fun gvar (src, dest) ->
        Hashtbl.find_all writtenBy gvar |> List.iter (fun n ->
          let open Ordsat in
          if n <> src && n <> dest then
            Queue.push {x = n ; a = src ; b = dest} q
        )
      ) ;
      Util.list_of_queue q in

    prec_constrs, int_constrs


  let is_time_consistent ps nodes arrows =
    Ordsat.sat (scheduling_constrs ps nodes arrows)


  let nodes_of_arrows ps arrs = 
    let id_set = arrs |> List.fold_left (fun acc ((src, _), (dest, _)) ->
      ISet.add src (ISet.add dest acc)
    ) ISet.empty in
    ISet.fold (fun nid acc -> 
      (ps |. node nid) :: acc
    ) id_set []



  let is_consistent ps arrows =
    is_agent_consistent ps &&
      ( let nodes = nodes_of_arrows ps arrows in 
        is_time_consistent ps nodes arrows )

  (* Performs a quick logarithmic time consistency check.  Agent
     coherence is tested globally.  Time coherence is tested for :
     Nodes : successors of [src] and predecessors of [dest] Arrows :
     outgoing from [src] and incoming to [dest] *)
  let is_locally_consistent ps src_id dest_id =
    let src = ps |. node src_id and dest = ps |. node dest_id in
    is_consistent ps (outgoing_arrows_list src @ incoming_arrows_list dest)
      
  (* A full blown consistency check. *)
  let is_globally_consistent ps = 
    is_consistent ps (all_arrows ps)
    


  (** Making a choice *)


  (* Does nothing if it does not exist *)
  let remove_choice nid it ps = 
    let brs = ps |. node nid |. branches in
    let pre = brs.items.(it).trait in
    ps |> (node nid |-- branches |-- choices) ^%= ItSetPreMap.remove pre it


  (* Forces a choice. If the choice is absent, it removes all the alternatives *)
  let force_choice nid it ps = 
    let brs = ps |. node nid |. branches in
    let pre = brs.items.(it).trait in
    let open ItSetPreMap in
    ps |> (node nid |-- branches |-- choices) ^%= 
        (fun c -> if mem pre it c 
          then add pre it (remove_all pre c)
          else remove_all pre c)


  (* Makes a choice *)
  let rec choose 
      (env : environment)
      (ps : partial_story) 
      (nid : node_id) 
      (it : item_id)
      (merge : node_id option) =

    let brs = ps |. node nid |. branches in
    let pre = brs.items.(it).trait in
    let open ItSetPreMap in
    if mem pre it brs.choices then

      (* Discards other possibilities *)
      let ps = ps |> (node nid |-- branches |-- choices) ^= 
                       remove_all pre brs.choices
                  |> (node nid |-- branches |-- remaining) ^%= 
                       TrSet.remove pre in

      (* Computes the destination of the new arrow and 
         creates a new node if needed *)

      let r_id, eff = brs.items.(it).source_ty in

      let source_id, ps = 
        match merge with
        | None -> add_node (init_node env r_id) ps
        | Some source_id -> source_id, ps in

      let ps = add_arrow ((source_id, eff), (nid, pre)) ps in

      (* Eliminates the choices that are no longer valid *)

      let excluded = brs.items.(it).excl
      and implied  = brs.items.(it).impl in

      let ps = excluded |> List.fold_left (fun ps e ->
        remove_choice nid e ps
      ) ps in

      let ps = implied |> List.fold_left (fun ps i ->
        force_choice nid i ps
      ) ps in

      ps      

    (* The choice does not exist *)
    else assert false





  (** Interface for [BranchAndCut.Make] *)

  type env = environment
  type s   = partial_story



  (** Propagation *)

  let try_clean_item nid it ps =
    if ISet.is_empty (item_applicants nid it ps) then
      begin
        Log.put "Cleaning." ;
        remove_choice nid it ps
      end
    else ps


  let simplify_item changed env nid it ps =
    let iappls = item_applicants nid it ps in
    ISet.fold (fun destid_code ps ->
      let destid_opt = node_opt destid_code in
      let ps' = choose env ps nid it destid_opt in
      let destid = Option.default (last_created ps') destid_opt in
      if is_locally_consistent ps' nid destid then begin
        ps
        end
        
      else begin
        changed := true ;
        (*Log.put "With :" ;
        print' env ps' ;*)
        Log.put (sprintf "Removing item %d of %d {%d}." it nid destid_code) ;
        ps |> (node nid |-- branches |-- item_disqualified) ^%=
            ISetImap.add it destid_code
      end
    ) iappls ps 
    |> try_clean_item nid it 


  let fast_cut env ps = 
    (*print' env ps ;
    Log.put "Doing fast cuts." ; *)
    let changed = ref false in
    let ps = 
      ps |> fold_choices (fun (nid, pre) ps ->
        let tappls = trait_applicants nid pre ps in
        ISet.fold (fun it ps ->

          let ps = simplify_item changed env nid it ps in
          (* print' env ps ; *)
          ps

        ) tappls ps
      ) ps in
    ps, !changed


  let simplify env ps = 
    Log.put "Simplifying" ;
    ps |> fold_choices (fun (nid, pre) (ps, changed) ->
      match nbranches nid pre ps with
      | 0 -> raise Bottom
      | 1 -> 
        let appls = trait_applicants nid pre ps in
        assert (ISet.cardinal appls = 1) ;
        let it = ISet.choose appls in
        let merge = node_opt (ISet.choose (item_applicants nid it ps)) in
        (choose env ps nid it merge, true)
      | _ -> (ps, changed)
        
    ) (ps, false)


  let rec propagate env ps =
    (*print' env ps ;*)
    let ps, simplified = simplify env ps in
    if simplified then 
      propagate env ps
    else
      let ps, cut = fast_cut env ps in
      if cut then propagate env ps else ps        
        





  (** Branching *)


  module ChoiceCost = struct
    (* Cost of a choice : branching factor *)
    type t = int
    let  le = (<=)
  end

  let best_branche ps = 
    let costF (nid, pre) = nbranches nid pre ps in
    Util.min_from_fold fold_choices costF ChoiceCost.le ps

  let branch env ps = 
    let (nid, pre) = best_branche ps in
    let tappls = trait_applicants nid pre ps in

    ISet.fold (fun it acc ->
       let iappls = item_applicants nid it ps in
       ISet.fold (fun destid_code (bs, env) ->
         
         let destid_opt = node_opt destid_code in
         let cost = (ps.id, if destid_opt = None then 1 else 0) in
         let id, env = generate_id env in
         { (choose env ps nid it destid_opt) with id ; cost } :: bs, env
         
       ) iappls acc
    ) tappls ([], env)
  



  module Cost = PStory.Cost
  
  let cost ps = ps.cost

  let is_bottom env ps =
    size ps > env.size_limit || not (is_globally_consistent ps)

  let is_solution env ps = no_more_choices ps



  let to_story env ps = 
    let arrows = all_arrows ps in
    let nodes = nodes_of_arrows ps arrows in
    let prec_g = 
      match Ordsat.solution (scheduling_constrs ps nodes arrows) with
      | None -> assert false (* The story is invalid *)
      | Some g  -> g in
    
    let empty_story = {
      events = Imap.empty ;
      prec = Relation.empty
    } in

    let module PGraph = Ordsat.PGraph in

    let process_node id n s = 
      let succs = 
        try PGraph.succ prec_g id  
        with Invalid_argument _ -> [] in
      { events = Imap.add id (id, n.rule) s.events ;
        prec = List.fold_right (Relation.add id) succs s.prec
      } in

    Imap.fold process_node ps.nodes empty_story



  let add_solution ps env = 
    let story = to_story env ps in
    let env = {env with stories = story :: env.stories} in
    let n = List.length env.stories in
    let filename = sprintf "out/%d.dot" n in
    print_in_file filename Story.dump_dot story ;
    print_endline (sprintf "Generated : %s." filename) ;
    env

end

