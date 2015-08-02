(** MAIN.ml : program main loop *)

open Util
open Refine

let usage_msg = "A program to generate storybooks from Kappa files."

let opt_dump_latex = ref false
let opt_nstories   = ref 1
let opt_input_files = Queue.create ()

let options = [
  "-n" , Arg.Set_int opt_nstories, "Sets the number of stories to generate (default : 1)" ;
  (* "--dump-latex", Arg.Set opt_dump_latex, "Dump the result of the algorithm in the Latex format" *)
]


let print_rules m fmt rs = 
  rs |> List.iter (fun r ->
    Kappa.print m.Model.signature fmt r 
  )


let main () = 

  Arg.parse options (fun f -> Queue.push f opt_input_files) usage_msg ;

  (*Queue.add "tests/abc-cflow.ka" opt_input_files ;*)

  let model = Parser.parse_files (Util.list_of_queue opt_input_files) in

  print_in_file "original.ka" Model.dump model ;

  let model = Refine.refine model in

  print_in_file "refined.ka" Model.dump model ;
  
  let rules, obs = Kappa.compile_model model in

  print_in_file "rules.txt" (print_rules model) rules ;

  let module Gen = Generation.Make (Kappa) in

  ignore (Gen.run rules obs ~stories_limit:(!opt_nstories))

   
let _ = 
  try main ()
  with _ -> 
    printf "Fatal error\n" ;
    Printexc.print_backtrace stdout
