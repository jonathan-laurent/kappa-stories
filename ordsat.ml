(** An event identifier *)
type elt = int

(** [(a, b)] represents the constraint :
    [a] has to come strictly before [b] *)
type prec_constr = elt * elt

(** The constraint [c] has to be interpreted as :
    [a] has to come strictly before [b] and
    [x] cannot happen between [a] and [b] (inclusive) *)
type int_constr = {
  x : elt ;
  a : elt ;
  b : elt ;
}


module PGraph = Graph.Persistent.Digraph.ConcreteBidirectional
  (struct
    type t = elt
    let compare = compare
    let equal = (=)
    let hash x = x
   end)

module PGraphOper = Graph.Oper.P       (PGraph)
module PGraphDfs  = Graph.Traverse.Dfs (PGraph)

open PGraph
open PGraphOper
open PGraphDfs

exception Cycle
  

let rec graph_of_pcs = function
  | [] -> PGraph.empty
  | (x, y) :: css -> 
    add_edge (graph_of_pcs css) x y

let pcs_of_ics ics = 
  ics |> List.map (fun c -> (c.a, c.b))


let solution (pcs, ics) =

  let g = pcs @ (pcs_of_ics ics) 
  |> graph_of_pcs 
  |> transitive_closure in

  let rec simpl g changed rem = function
    | [] -> (g, changed, rem)
    | ic :: ics -> 
      
      (* The constraint is satisfied so we discard it *)
      if mem_edge g ic.x ic.a || mem_edge g ic.b ic.x then
        simpl g true rem ics

      (* One litteral of the disjonction is false so the other has to be true *)
      else if mem_edge g ic.a ic.x then
        simpl (add_edge g ic.b ic.x) true rem ics
      else if mem_edge g ic.x ic.b then
        simpl (add_edge g ic.x ic.a) true rem ics

      (* Otherwise, we keep the constraint as it is *)
      else
        simpl g changed (ic :: rem) ics in

  let rec full_simpl (g, ics) = 
    let g, changed, ics = simpl g false [] ics in
    if changed then full_simpl (transitive_closure g, ics)
    else (g, ics) in

  let rec sat_aux (g, ics) = 
    if has_cycle g then None
    else match full_simpl (g, ics) with
    | g, [] -> 
      if has_cycle g then None else Some (transitive_reduction g)
    | g, ic :: ics ->
      ( match sat_aux (add_edge g ic.x ic.a, ics) with
      | None -> sat_aux (add_edge g ic.b ic.x, ics)
      | sol -> sol ) in

  sat_aux (g, ics)
      
      

let sat (pcs, ics) = match solution (pcs, ics) with
  | None ->  false
  | Some _ -> true
        

let tests () = 

  let pcs = [(3,4) ; (3,2) ; (5,4) ; (5,1) ; (6,2) ; (6,1)] in
  let pcs = (2,5) :: pcs in
  let ics = [
    {x=5;a=6;b=2};
    {x=6;a=3;b=4};
    {x=3;a=6;b=1};
  ] in

  Printf.printf "SAT : %b\n" (sat (pcs, ics))

  
  
  



