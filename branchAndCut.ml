module type S = sig

  type s
  type env

  module Cost : sig
    type t
    val le : t -> t -> bool
  end

  val cost : s -> Cost.t
  val propagate : env -> s -> s

  val is_solution : env -> s -> bool
  val is_bottom   : env -> s -> bool

  val branch : env -> s -> s list * env

  val add_solution : s -> env -> env

  exception Bottom

  val print : env -> Format.formatter -> s -> unit

end


module Make (S : S)  = 
struct

  type s   = S.s
  type env = S.env

  open S
  open Util

  module Spq = Leftistheap.Make
    (struct
      type t = s
      let le s s' = Cost.le (cost s) (cost s')
     end)

  let dump env s = print env Format.std_formatter s 

  let rec visit env ss =
    try 
      let s, ss = Spq.extract_min ss in
      try
        Log.put "Visit node." ;
        let s = propagate env s in
        if is_bottom env s then
          visit env ss
        else if is_solution env s then 
          visit (add_solution s env) ss
        else 
          begin
            Log.put "Branching." ;
            let branches, env = branch env s in
            visit env (List.fold_right Spq.insert branches ss)
        end
      with Bottom -> visit env ss
          
    with Leftistheap.Empty -> env

  let run env s_list = 
    visit env (List.fold_right Spq.insert s_list (Spq.empty))

end
