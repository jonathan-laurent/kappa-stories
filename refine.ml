open Model
open Util
open SiteGraph
open ContactMap
open Lens


let refine_rule (cm : contact_map) (r : rule) =

  let gen_id = Util.new_counter 0 in
  let gen_rule_id = Util.new_counter 0 in
  
  let wildcard_opts = 
    let q = Queue.create () in
    r.lhs |> iter_ports (fun p ->
      match p |. Site.lnk_state with
      | Site.Bound_some -> 
        let pid = Site.port_id p in
        Queue.add (pid, neighbors (port_ty pid r.lhs) cm) q
      | _ -> ()
    ) ;
    Util.list_of_queue q in

  let module SiteMap = ExtMap 
        ( struct
          type t = site_id 
          let compare = compare
          end ) in

  let rec generate_agents ags = function
    | [] -> [ags]
    | (src_id, pos) :: wopts -> 

      List.fold_right (fun (dest_agty, dest_s) acc ->
        let merging = 
          Imap.fold (fun ag_id (ag_ty, sites) acc ->
            if ag_ty = dest_agty && not (SiteMap.mem dest_s sites) then
              let ags' = 
                Imap.add ag_id (ag_ty, SiteMap.add dest_s src_id sites) ags in
              generate_agents ags' wopts @ acc
            else acc
          ) ags [] in

        let with_new_agent = 
          let ag_id = gen_id () in
          let ags' = 
            Imap.add ag_id (dest_agty, SiteMap.singleton dest_s src_id) ags  in
          generate_agents ags' wopts in

        with_new_agent @ merging @ acc
      ) pos [] in


  let refine_sg_with ags sg = 
    Imap.fold (fun _ (ag_ty, sites) sg ->
      let sg, id = add_agent ag_ty sg in
      SiteMap.fold (fun new_ag_site destp sg ->
        let sg = sg |> agent id ^%= Agent.add_site new_ag_site in
        match sg |. port destp |. Site.lnk_state with
        | Site.Bound_some -> bind (id, new_ag_site) destp sg
        | _ -> sg
      ) sites sg
    ) ags sg in

  let refine_with ags =
    { r with
      lhs = refine_sg_with ags r.lhs ;
      rhs = refine_sg_with ags r.rhs } in

  let rename_rules = List.map (fun r -> {r with refn = gen_rule_id ()}) in

  let opts = generate_agents Imap.empty wildcard_opts in
  rename_rules (List.map refine_with opts)
       

let refine (m : model) =
  let cm = ContactMap.compute m in
  { m with 
    rules = List.concat (List.map (refine_rule cm) m.rules) }
