(** UTIL.ml : some general purpose utilities *)



(** Some operators *)

let (|>) x f = f x
let (|-) f g x = g (f x)

let identity x = x

module Option = struct

  let map f = function
	| None -> None 
	| Some x -> Some (f x)
	  
  let map_default def f = function
	| None -> def
	| Some x -> f x
	  
  let default def = function
	| None -> def
	| Some x -> x

end

let rec map_and_filter f = function
	| [] -> []
	| x::xs -> 
		( match f x with
			| None -> map_and_filter f xs
			| Some a -> a :: map_and_filter f xs
		)


let sum_list = List.fold_left (+) 0

let rec zip l l' = match l, l' with
  | [], _ | _, [] -> []
  | x::xs, y::ys -> (x, y) :: (zip xs ys)

let rec print_list begF sepF endF print_elem fmt l = 
  begF fmt ;
  let rec aux = function
    | [] -> endF fmt ;
    | x :: [] -> print_elem fmt x ; endF fmt
    | x :: xs -> 
      print_elem fmt x ; 
      sepF fmt ; 
      aux xs in
  aux l

let print_list' begC sepC endC print_elem fmt l = 
  let compile c fmt = Format.fprintf fmt "%s" c in 
  print_list 
    (compile begC)
    (compile sepC)
    (compile endC)
    print_elem fmt l

let print_list_newline print_elem fmt l =
  print_list
    (fun _ -> ())
    (fun fmt -> Format.fprintf fmt "@;")
    (fun fmt -> Format.fprintf fmt "@;")

    print_elem fmt l

let printf  = Format.printf
let sprintf = Format.sprintf

module Int = struct
	type t = int
	let compare = compare
end

module Int2 = struct
	type t = int * int
	let compare = compare
end

let make_lex_compare cmp1 cmp2 (x, y) (x', y') =
	let r = cmp1 x x' in
	if r = 0 then cmp2 y y'
	else r

let make_rev_order cmp x y = - (cmp x y)


(** Some dictionnaries *)



module type EXT_MAP_S = sig

  include Map.S
	
  val of_list : (key * 'a) list -> 'a t
	
  val to_list : 'a t -> (key * 'a) list
	
  val of_array : (key * 'a) array -> 'a t
	
  val lookup_def : def:'a -> key -> 'a t -> 'a
	
  val mem_binding : (key * 'a) -> 'a t -> bool

  val update : key -> ('a -> 'a) -> 'a t -> 'a t

end

module ExtMap (X : Map.OrderedType) : 
  EXT_MAP_S with type key = X.t  =
struct

	include Map.Make (X)
	
	let of_list l = List.fold_left 
		(fun dict (key, elem) -> add key elem dict)
		empty
		l
		
	let to_list m = fold (fun k v acc -> (k, v) :: acc) m [] |> List.rev
		
	let of_array a = of_list (Array.to_list a)
	
	(** A lookup operation returning [def] if the element is not found *)
	
	let lookup_def ~def k t  = 
		try find k t with Not_found -> def 
		
	let mem_binding (k, v) t = 
		try find k t = v with Not_found -> false

    let update k f m = add k (f (find k m)) m

end

module type MAP_SET_S = sig

  type t
  type key
  type elt
  type elt_set

  val empty : t
  val find_all : key -> t -> elt_set
  val remove_all : key -> t -> t
  val mem : key -> elt -> t -> bool
  val add : key -> elt -> t -> t
  val remove : key -> elt -> t -> t

  val iter : (key -> elt_set -> unit) -> t -> unit
  val iter_bindings : (key -> elt ->  unit) -> t -> unit

end

module MapSet (Key : Map.OrderedType) (Set : Set.S) : 
  (MAP_SET_S 
   with type key := Key.t with type elt := Set.elt with type elt_set := Set.t) 
  =
struct

  module M = Map.Make(Key)

  type t = Set.t M.t

  let empty = M.empty

  let find_all k t = 
    try M.find k t with Not_found -> Set.empty

  let remove_all k t = M.remove k t 

  let mem k e t = Set.mem e (find_all k t)

  let add k e t = M.add k (Set.add e (find_all k t)) t

  let remove k e t = M.add k (Set.remove e (find_all k t)) t

  let iter f t = M.iter f t

  let iter_bindings f r = M.iter (fun k s -> Set.iter (fun e -> f k e) s) r

end


module Imap  = ExtMap (Int)
module I2map = ExtMap (Int2)
module Smap  = ExtMap (String)

type 'a string_map = 'a Smap.t
type 'a int_map    = 'a Imap.t
type 'a int2_map   = 'a I2map.t


(** Sets *)

module SSet = Set.Make(String)
module ISet = Set.Make(Int)

type int_set = ISet.t

let sset_of_list = List.fold_left (fun acc x -> SSet.add x acc) SSet.empty
let iset_of_list = List.fold_left (fun acc x -> ISet.add x acc) ISet.empty

(** Functions on Queues *)

let list_of_queue q = List.rev (Queue.fold (fun acc x -> x :: acc) [] q)

(** Functions on Stacks *)

let rec list_of_stack s = match (Stack.copy s) with
  | s when (Stack.length s) = 0 -> []
  | s -> let e = Stack.pop s in e::(list_of_stack s)


(** Unordered pairs *)

module UnorderedPair(Ord : Map.OrderedType) = struct

  (* The order of the ports doesn't matter.  By convention, the
	 first element is ALWAYS *)

  type t = (Ord.t * Ord.t)
	
  let make x y = if Ord.compare x y <= 0 then (x, y) else (y, x)
	  
  let to_ordered_pair p = p
	
  let compare = compare

end




let new_counter first_id = 
  let c = ref first_id in 
  fun () -> 
	begin 
	  let r = !c in
	  incr c ;
	  r
	end


let fold_from_iter iter = 
  fun f acc t ->
    let a = ref acc in
    iter (fun e -> a := f e !a) t ;
    !a


let min_from_fold fold costF le = 

  (* [None] is +oo *)
  let le_opt x y = match x, y with
    | _, None    -> true
    | None, _    -> false
    | Some x, Some y -> le x y in

  fun t ->
    let min_opt = 
      fold (fun e (least, least_e) ->
        let cost_e = Some (costF e) in
        if le_opt cost_e least then (cost_e, Some e) else (least, least_e)
      ) (None, None) t in

    match snd min_opt with
    | None -> assert false
    | Some x -> x


let print_in_file filename printF x = 
  let c = open_out filename in
  let fmt = Format.formatter_of_out_channel c in
  printF fmt x ;
  close_out c ; 
