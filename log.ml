open Util

(*let put = print_endline*)
let put _ = ()


let dline = 
  "================================================================================="
let line  = 
  "---------------------------------------------------------------------------------"


exception Terminated
let counter = ref 10
let tick () = 
  decr counter ;
  if !counter <= 0 then raise Terminated

let outdir = "out"
let outn = ref 1

let print printF x = 
  let filename = sprintf "%s/%d" outdir !outn in 
  Util.print_in_file filename printF x ;
  print_endline (sprintf "Generated : %d" !outn) ;
  incr outn
