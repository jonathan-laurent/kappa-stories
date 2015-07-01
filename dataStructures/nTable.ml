open Util
open List

type 'a t = {
	arr            : 'a array ;
	name_of_id_arr : string array ;
	id_of_name_map : int string_map
}

type id   = int
type name = string


module Id = struct
	type t = id
	let compare = compare
end


let id_zero = 0

let name_of_id id t = t.name_of_id_arr.(id)

let id_of_name name t = Smap.find name t.id_of_name_map

let is_valid_id id t = id <= Array.length t.arr

let length t = Array.length (t.arr)

let read id t = t.arr.(id)

let read_named name t = read (id_of_name name t) t


let iter f t = Array.iter f t.arr


let is_consistent t =
	Array.length t.arr = Array.length t.name_of_id_arr

let create l =
	let arr_f = Array.of_list (map snd l) in
	let arr   = Array.mapi (fun i f -> f i) arr_f in
	let noi   = Array.of_list (map fst l) in
	let ion   = Smap.of_array (Array.mapi (fun id name -> (name, id)) noi) in
	
	{ arr = arr ; name_of_id_arr = noi ; id_of_name_map = ion } 	
	
	
let is_empty t = Array.length t.arr = 0

(*

exception E
let dummy_arr = Array.init 0 (fun _ -> raise E)

let dummy = {
	arr = dummy_arr ;
	name_of_id_arr = dummy_arr ;
	id_of_name_map = SMap.empty
}

*)
