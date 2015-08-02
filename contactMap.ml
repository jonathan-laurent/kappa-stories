open Model
open SiteGraph
open Lens

type port_type = Signature.port_ty

module G = Graph.Imperative.Graph.Concrete
  ( struct
    type t = port_type
    let compare = compare
    let equal = (=)
    let hash = Hashtbl.hash
    end
  )

type contact_map = G.t

let compute (m : model) =
  let g = G.create ~size:100 () in

  let process_pattern sg = 
    let open Site in
    sg |> iter_ports (fun p ->
      match link p with
      | None -> ()
      | Some l -> 
        let src, dest = Link.ports l in
        G.add_edge g (port_ty src sg) (port_ty dest sg)
    ) in

  let process_rule r = 
    process_pattern r.lhs ;
    process_pattern r.rhs in
    
  m.rules |> List.iter process_rule ;
  g


let neighbors pty cm = 
  try G.succ cm pty
  with Invalid_argument _ -> []
