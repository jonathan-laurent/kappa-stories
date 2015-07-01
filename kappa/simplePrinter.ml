open Util
open Format
open SimpleAst

let rec print_list print_elem f = function
	| [] -> ()
	| x::[] -> print_elem f x
	| x::xs -> print_elem f x ; fprintf f ", " ; print_list print_elem f xs


let print_int_state f = function
	| None -> ()
	| Some s -> fprintf f "_%s" s


let print_lnk_state f = function
	| Lnk_free         -> ()
	| Lnk_value i      -> fprintf f "!%d" i
	| Lnk_any          -> fprintf f "?"
	| Lnk_some         -> fprintf f "!_"
	| Lnk_type (a, s)  -> fprintf f "!%s.%s" s a
	

let print_site f s = fprintf f "%s%a%a" 
					s.name 
					print_int_state s.int_state
					print_lnk_state s.lnk_state

let print_agent f ag = fprintf f "%s(%a)" ag.ty (print_list print_site) ag.sites


let print_sgraph f s = 
	print_list print_agent f (s |> Imap.to_list |> List.map snd)
	

module SiteGraph = struct

	let fprintf f sg = print_sgraph f (SimpleAst.dump sg)

end
