open Util
open Parser
open Model
open TexRendering
open Printf
open Mlpost
open Sys

(** MAIN.ml : program main loop *)

let usage_msg = "A program to render rules from Kappa file"

let output_directory = ref "output"
let prefix = ref ""
let params = ref ""
let generate_pdf = ref false
let input_files = Queue.create ()

let options = [
	"-o", Arg.Set_string output_directory, "Output directory";
	"-p", Arg.Set_string prefix, "Output files prefix";
	"-s", Arg.Set_string params, "Params";
	"--pdf", Arg.Unit (fun () -> generate_pdf := true), "Generate a PDF"
	]

let main () =
	print_string "Tex Renderer\n" ;
	let params = my_params in
	Arg.parse options (fun f -> Queue.push f input_files) usage_msg ;
	let model = Parser.parse_files (Util.list_of_queue input_files) in
	Sys.command ("mkdir \""^(!output_directory)^"\"");
	Sys.chdir !output_directory;
	let tex_file = open_out ("main.tex") in
	Printf.fprintf tex_file "\\documentclass[10pt]{article}
\\usepackage{graphics}  
\\begin{document}  
\\begin{center}
\\begin{tabular}{| c | c | c |}
\\hline\n";
	let i = ref 1 in
	let render_rule r = 
		let lhs, rhs = SimpleAst.dump r.lhs,  SimpleAst.dump r.rhs in
		let renderF = compute_pos_constrs [lhs ; rhs] params in
		let file1 = sprintf "%s%s_lhs" !prefix r.name
		and file2 = sprintf "%s%s_rhs" !prefix r.name in
		render ~filename:file1 renderF lhs params;
 		render ~filename:file2 renderF rhs params;
		Printf.fprintf tex_file "%s & \\includegraphics{%s} & \\includegraphics{%s} \\\\\n\\hline\n" r.name file1 file2;
		i := (!i+1) in
		
	List.iter render_rule (model.rules);
	Metapost.dump  ~pdf:true ("tmp");
	Printf.fprintf tex_file "\\end{tabular}\n\\end{center}\n\\end{document}";
	close_out tex_file;

	if !generate_pdf then (Sys.command ("pdflatex main.tex"); ())

let _ = main ()
