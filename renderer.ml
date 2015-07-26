open Util
open Parser
open Model
open TexRendering
open Printf
open Mlpost
open Sys

(** MAIN.ml : program main loop *)

open Equiv


(*module P = struct
	let exn_on_bottom = false
end
module E = struct
  type t = int*int
  let compare = (Util.make_lex_compare compare compare)
  let category t = let (i1,i2) = t in i1+i2
end
module Eq = Equiv.Make (E) (P)
let print_couple c = let (i1,i2) = c in Printf.printf "(%i,%i)\n" i1 i2
let print_bool b = print_string (string_of_bool b)
let test () =
	print_string "\n\n";
	print_string "TEST";
	let e = Eq.empty in
	let e = Eq.add ~marked:true (1,1) e in
	let e = Eq.add ~marked:true (1,2) e in
	let e = Eq.add ~marked:false (1,3) e in
	let e = Eq.add ~marked:true (3,1) e in
	let e2 = Eq.union (1,1) (1,3) e in
	let e3 = Eq.union (1,1) (3,1) e2 in
	print_couple (Eq.find (1,3) e2);
	print_bool (Eq.is_bottom e3);
	print_string "\n\n"*)

(*type sexpr_params =
	| Param of string*(sexpr_params list)
	| Value of string

let parse_sexpr s = match s with*)

let usage_msg = "A program to render rules from Kappa file"

let output_directory = ref "output"
let prefix = ref ""
let sexpr = ref ""
let generate_pdf = ref false
let input_files = Queue.create ()

let options = [
	"-o", Arg.Set_string output_directory, "Output directory";
	"-p", Arg.Set_string prefix, "Output files prefix";
	"-s", Arg.Set_string sexpr, "Params";
	"--pdf", Arg.Unit (fun () -> generate_pdf := true), "Generate a PDF"
	]

let main () =
	print_string "Tex Renderer\n" ;
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

	(*let s = SExpr.parse_string sexpr in
	let set_params sexpr params = match sexpr with
		| Atom str -> 
		| Expr (Atom str)::lst -> set_params lst ()
		| _ -> params
	in let params = set_params sexpr my_params in*)

	let params = my_params in
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


