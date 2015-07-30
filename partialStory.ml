module Make (S : AgentBased.System) = struct

  module AbsUtils = AgentBased.Utils(S)

  open S
  open AbsUtils
  open Util

  module ItSetPreMap = MapSet (Trait) (ISet)
  module ISetImap    = MapSet (Int)   (ISet)


  (** Nodes *)

  type node_id = int

  type item_id = int

  type 'a global = node_id * 'a

  type agent_gid = agent_id global

  type gvar = var global

  type gtrait = trait global

  (* The _contribution arrow_ [((srcn, eff), (destn, pre))] means that the trait 
     [pre] checked by [destn] is lastly written by the effect [eff] of
     [srcnn].
     Each such arrow induces a reverse precedence arrow. 
     TODO *)
  type arrow = gtrait * gtrait

  (* The type of the source of an arrow *)
  type source_ty = rule_id * eff


  (* An (choice) item [it] in node [n] indicates that the trait [it.trait] of
     [n] can be the destination of a contributive arrow from a
     source of type [it.source_ty] *)
  type item = {
    id        : item_id ;
    trait     : pre ;
    source_ty : source_ty ;

    (* Some compatibility informations between choice items *)
    impl : item_id list ;
    excl : item_id list
  }

  (* Describes the way a node can be branched in a partial story *)
  type node_branches = {

    (* Contains all choice items, indexed by id *)
    items : item array ;

    (* Maps each precondition to the set of possible items. *)
    choices :  ItSetPreMap.t ;

    (* Traits of the node remaining to be explained *)
    remaining : TrSet.t ;

    (* Nodes that are disqualified for realizing an item.
       The entry (-1) refers to the creation of a new node. 
       See [__new_node__]. *)
    item_disqualified : ISetImap.t ;
  }

  type node = {

	id : node_id ;

	rule : rule ;
    
    (* Incoming arrows *)
    incoming_arrows : gtrait pre_map ;

    (* Incoming arrows *)
    outgoing_arrows : (eff * gtrait) list ;

    (* Branches. TODO
       Invariant : keys of [arrows] union 
       [branches.remaining] = set of preconditions of [rule] *)
    branches : node_branches ;
  }



  (* See [node.item_disqualified] *)

  let __new_node__ : node_id = -1

  let node_opt x = 
    if x = __new_node__ then None else Some x




  (** Keeping track of equivalent agents *)


  (* To keep track of equivalent agents*)

  module AgEqv = Equiv.Make 
    (struct 

      type t = agent_gid
      let compare = compare

      (* Two distinct agents from the same rule can't be merged. *)
      let category (node_id, _) = node_id

      let print fmt (nid, agid) = Format.fprintf fmt "(%d, %d)" nid agid

     end) 
    (struct let exn_on_bottom = false end)

  type agent_eqv = AgEqv.t



  (** Influences the way partial stories are ordered in a
      priority queue *)

  module Cost = struct

    (* 1. Identifier of the direct ancestor (decreasing order)
       2. Higher cost if creation of a new node *)

    type t = int * int 

    let compare : t -> t -> int = 
      Util.make_lex_compare 
        (Util.make_rev_order compare)
        compare 

    let le x y = compare x y <= 0

    let default = (0, 0)

  end


  type partial_story = {

    id : int ;
    cost : Cost.t ;
    agent_eqv : agent_eqv ;
    next_node_id : int ;

    (* node_id -> node *)
    nodes : node int_map ;

    (* Returns all the nodes instanciating a given rule.
       rule_id -> node_id *)
    rules_instances : ISetImap.t ;
  }	

    




  (** Some lenses to access these structures easily *)

  open Lens

  module ImapLens  = LensExtMap (Imap)
  module TrMapLens = LensExtMap (TrMap)

  let imap_lens     k =  ImapLens.make  k
  let trmap_lens    k =  TrMapLens.make k

  let nodes = {
    get_ = (fun ps -> ps.nodes) ;
    set_ = (fun ns ps -> {ps with nodes = ns})
  }

  let rules_instances = {
    get_ = (fun ps -> ps.rules_instances) ;
    set_ = (fun v ps -> {ps with rules_instances = v})
  }

  let node k = nodes |-- imap_lens k

  let item it n = n.branches.items.(it)

  let outgoing_arrows = {
    get_ = (fun n -> n.outgoing_arrows) ;
    set_ = (fun arrs n -> {n with outgoing_arrows = arrs})
  }

  let incoming_arrows = {
    get_ = (fun n -> n.incoming_arrows) ;
    set_ = (fun iarrs n -> {n with incoming_arrows = iarrs})
  }

  let branches = {
    get_ = (fun n -> n.branches) ;
    set_ = (fun b n -> {n with branches = b})
  }

  let choices = {
    get_ = (fun b -> b.choices) ;
    set_ = (fun v b -> {b with choices = v})
  }

  let remaining = {
    get_ = (fun b -> b.remaining) ;
    set_ = (fun v b -> {b with remaining = v}) ;
  }

  let item_disqualified = {
    get_ = (fun b -> b.item_disqualified) ;
    set_ = (fun v b -> {b with item_disqualified = v}) ;
  }

  let incoming_arrow pre = incoming_arrows |-- trmap_lens pre




  (** Easier access to arrows *)   

  let incoming_arrows_list dest : arrow list = 
    let q = Queue.create () in
    dest.incoming_arrows |> TrMap.iter (fun pre (src_id, eff) ->
      Queue.push ((src_id, eff), (dest.id, pre)) q
    ) ;
    Util.list_of_queue q

  let outgoing_arrows_list src : arrow list = 
    src.outgoing_arrows |> List.map (fun (eff, (dest_id, pre)) ->
      (src.id, eff), (dest_id, pre)
    )

  let all_arrows (ps : partial_story) = 
    Imap.fold (fun _ n acc ->
      (outgoing_arrows_list n) @ acc) ps.nodes []






  (* Utilities to update the [agent_eqv] structure while
     a partial story is modified and to query it. *)
  module Eqv = struct
      
    let add_node eqv (node : node) =
      node.rule |> fold_agents (fun ag eqv ->
        (* Mark agents created by the node so they can't be created twice *)
        let marked = is_created ag node.rule in
        AgEqv.add ~marked (node.id, ag) eqv
      ) eqv
          
    let gags_of_gtrait (nid, trait : gtrait) : agent_gid list =
      trait |> trait_vars |> List.map (fun v ->
        (nid, var_agent v)
      )

    let add_arrow eqv (src, dest : arrow) =
      zip (gags_of_gtrait src) (gags_of_gtrait dest) 
      |> List.fold_left (fun eqv (v, v') ->
        AgEqv.union v v' eqv
      ) eqv
      
    let gvars_of_gtrait node_id ag_eqv trait : gvar list =
      trait |> trait_vars |> List.map (fun v -> 
        let (nid, aid) = AgEqv.find (node_id, var_agent v) ag_eqv in
        (nid, set_var_agent aid v)
      )
      
  end




  (** Update functions *)

  let add_node (initF : node_id -> node) ps =
    let id = ps.next_node_id in
    let n  = initF id        in
    let ps = ps
       |> node id ^= n
       |> rules_instances ^%= ISetImap.add (n.rule |> rule_id) id
       |> (fun ps -> { ps with 
             next_node_id = id + 1 ;
             agent_eqv = Eqv.add_node ps.agent_eqv n }) in
    id, ps

  let add_arrow (arr : arrow) ps = 
    let (src, eff), (dest, pre) = arr in
    ps |> (node dest |-- incoming_arrow pre) ^= (src, eff)
       |> (node src |-- outgoing_arrows) ^%= (fun ias ->
          (eff, (dest, pre)) :: ias)
       |> (fun ps -> {ps with agent_eqv = Eqv.add_arrow ps.agent_eqv arr})





  (** Query functions and utilities *)

  let fold_choices f acc ps = 
    Imap.fold (fun _ (n : node) acc ->
      TrSet.fold (fun pre acc ->
        f (n.id, pre) acc
      ) n.branches.remaining acc
    ) ps.nodes acc

  let no_more_choices = fold_choices (fun _ _ -> false) true

  let item_applicants nid it ps = 
    let node = ps |. node nid in
    let rid, _  = node.branches.items.(it).source_ty in
    ISet.diff 
      (ISet.add __new_node__  (ISetImap.find_all rid ps.rules_instances))
      (ISetImap.find_all it node.branches.item_disqualified)


  let trait_applicants nid pre ps = 
    let brs = ps |. node nid |. branches in
    brs.choices |> ItSetPreMap.find_all pre 

  let nbranches nid pre ps =
    let appls = trait_applicants nid pre ps in
    ISet.fold (fun a sum ->
      sum + ISet.cardinal (item_applicants nid a ps)
    ) appls 0

      
  let size ps = Imap.cardinal ps.nodes

  let last_created ps = ps.next_node_id - 1






(** Printing utilities *)


  let print_pstory rulesF fmt ps =
    let pr x = Format.fprintf fmt x in
    pr "@[<v>%s@;PS %d@;%s@;" Log.dline ps.id  Log.dline ;

    let print_item (n : node) (itid : item_id) =
      let it = n.branches.items.(itid) in
      let source_rid, eff = it.source_ty in
      let source_r = rulesF source_rid in
      pr "%d. (%a)" itid (print_trait source_r) eff ;
      pr " from %s " (rule_name source_r) ;
      let applicants = 
        item_applicants n.id itid ps |> ISet.elements in
      let print_applicant _ i = 
        if i = __new_node__ then pr "*" else pr "%d" i in
      Util.print_list' "{ " " ; " " }" print_applicant fmt applicants in

    let print_incoming_arrow r pre (src_id, eff) =
      let source_r = (ps |. node src_id).rule in
      pr "[%a] <-- [%a] of %d@;" 
        (print_trait r) pre
        (print_trait source_r) eff
        src_id in

    let print_node id (n : node) =
      assert (id = n.id) ;
      
      pr "%d [%s]@;%s@[<v 4>@;" n.id (rule_name n.rule) Log.line ;

      n.incoming_arrows |> TrMap.iter (print_incoming_arrow n.rule) ;

      pr "@;";

      n.branches.choices |> ItSetPreMap.iter (fun pre poss ->
        pr "@[<v 4>[%a]@;" (print_trait n.rule) pre ;
        let applicants = trait_applicants n.id pre ps |> ISet.elements in
        Util.print_list_newline (fun _ i -> print_item n i) fmt applicants ;
        pr "@]@;"
      ) ;

      pr "@]@;"

    in

    Imap.iter print_node ps.nodes;

    pr "@]@."
      


end



