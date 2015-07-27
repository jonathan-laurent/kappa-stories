open Util
open Parser
open Model
open TexRendering
open Printf
open Mlpost
open Sys
open SExpr

(** MAIN.ml : program main loop *)

(*open Equiv
module P = struct
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

type sexpr_params =
	| Param of string*(sexpr_params list)
	| Value of string

let rec parse_sexpr s = match s with
	| Expr [] -> []
	| Expr ((Atom str)::lst) -> [Param (str, List.fold_left (fun acc elem -> acc@(parse_sexpr elem)) [] lst)]
	| Expr (expr::lst) -> (parse_sexpr expr)@(List.fold_left (fun acc elem -> acc@(parse_sexpr elem)) [] lst)
	| Atom str -> [Value str]

let parse_sexprs lst = List.fold_left (fun acc elem -> acc@(parse_sexpr elem)) [] lst

let usage_msg = "A program to render rules from Kappa file"

let output_directory = ref "output"
let prefix = ref ""
let params_command = ref ""
let generate_pdf = ref false
let input_files = Queue.create ()

let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = String.create n in
  really_input ic s 0 n;
  close_in ic;
  (s)

let options = [
	"-o", Arg.Set_string output_directory, "Output directory";
	"-p", Arg.Set_string prefix, "Output files prefix";
	"--pdf", Arg.Unit (fun () -> generate_pdf := true), "Generate a PDF";
	"--", Arg.Rest (fun txt -> params_command := String.concat " " [!params_command; (load_file txt)]), "Params files"
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

	let s = parse_sexprs (SExpr.parse_string !params_command) in
	let make_pos_policy sexpr acc = match sexpr with
		| Param (name, [Value value]) -> (name, float_of_string value)::acc
		| _ -> acc in
	let set_pos_policy sexpr policy = match sexpr with
		| Param (name, lst) -> let e = (name, List.fold_left (fun acc elem -> make_pos_policy elem acc) [] lst)
			in e::policy
		| _ -> policy in
	let set_param sexpr params = match sexpr with
		| Param (name, [Value value]) ->
			if name = "ag_dist" then {params with ag_dist = Num.cm (float_of_string value)}
			else if name = "dev_angle" then {params with dev_angle = float_of_string value}
			else params
		| Param (name, values) ->
			if name = "pos_policy" then {params with pos_policy = List.fold_left (fun acc elem -> set_pos_policy elem acc) params.pos_policy values}
			else params
		| _ -> params in
	let params = List.fold_left (fun acc elem -> set_param elem acc) my_params s in

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


