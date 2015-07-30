(** MAIN.ml : program main loop *)

open Util
open Parser
open SiteGraph
open SimplePrinter
open Generation

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
		let compiled = Kappa.compile (model.signature) 0 r in
		Kappa.print model.signature  Format.std_formatter compiled in
		
(*
		let lhs, rhs = SimpleAst.dump r.lhs,  SimpleAst.dump r.rhs in
		let open TexRendering in
		let renderF = compute_pos_constrs [lhs ; rhs] my_params in
		render ~filename:("test") renderF rhs my_params in *)
		
	List.iter print_rule (model.rules) (* @ List.map (fun t -> t.init_rule) model.init) *)
	(* Metapost.dump ~pdf:true "tmp" *)
	


let stories () = 

  Arg.parse options (fun f -> Queue.push f opt_input_files) usage_msg ;

  let model = Parser.parse_files (Util.list_of_queue opt_input_files) in
  let module Gen = Generation.Make (Kappa) in
  let open Model in
  
  let genid = new_counter 0 in
  let id_of_name = Hashtbl.create 100 in
  let compile_rule r = 
    let id = genid () in
    Hashtbl.add id_of_name r.name id ;
    Kappa.compile model.signature id r in

  Log.put "Compiling rules..." ;

  let rules = List.map compile_rule model.rules in

  let print_rules fmt rs = 
    rs |> List.iter (fun r ->
      Kappa.print model.signature fmt r 
    ) in

  print_in_file "rules.txt" print_rules rules ;


  let (obs, _) = Smap.min_binding model.observables in
  let obs_id = Hashtbl.find id_of_name obs in
  Gen.run rules obs_id

	
let _ = stories ()
