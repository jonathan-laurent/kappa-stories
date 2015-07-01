(** MAIN.ml : program main loop *)

open Util
open Parser
open SiteGraph
open SimplePrinter

let usage_msg = "A program to generate storybooks from Kappa files"

let opt_dump_latex = ref false
let opt_input_files = Queue.create ()

let options = [
	"--dump-latex", Arg.Set opt_dump_latex, "Dump the result of the algorithm in the Latex format"
	]


let main () = 
	Arg.parse options (fun f -> Queue.push f opt_input_files) usage_msg ;
	let model = Parser.parse_files (Util.list_of_queue opt_input_files) in
	
	let open Model in
	
	let print_rule r = 

		let printer = SimplePrinter.SiteGraph.fprintf in
		printf "%s : \n" r.name ;
		printf "%a -> %a\n" printer r.lhs printer r.rhs ;
		printf "\n\n" ;
		let compiled = Rule.compile (model.signature) r in
		Rule.print Format.std_formatter model.signature compiled in
		
		(* let lhs, rhs = SimpleAst.dump r.lhs,  SimpleAst.dump r.rhs in
		let open TexRendering in
		let renderF = make_rendering [lhs ; rhs] my_params in
		render "test" renderF lhs my_params in *)
		
	List.iter print_rule (model.rules @ List.map (fun t -> t.init_rule) model.init)
	
	
let _ = main ()
