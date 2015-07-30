(** Structure representing an equivalence relation. *)
module type ELEM = sig
  type t
  val compare : t -> t -> int
  val category : t -> int
  val print : Format.formatter -> t -> unit
end

module type PARAMS = sig
  val exn_on_bottom : bool
end

(** This exception is sent as soon a structure becomes inconsistent *)
exception Bottom

module type S = sig
  type t
  type elem

  (** The empty structure. *)
  val empty : t

  (** Tests if an element appears in the structure. O(log n) *)
  val mem : elem -> t -> bool

  (** Adds a new element, alone in its equivalence class. O(log n) *)
  val add : ?marked:bool -> elem -> t -> t

  (** Such that [find i t = find j t] if and only if [i] and [j] belong 
      to the same equivalence class. *)
  val find : elem -> t -> elem

  (** Merges the equivalence classes of two elements *)
  val union : elem -> elem -> t -> t

  (* Adds the constraint that two elements cannot be equivalent. 
      If this constraint is violated at some point on [t], 
      then [is_bottom t = true]
  val assert_disjoint : elem -> elem -> t -> t *)

  (* Marks an element. If more than one element of a same equivalence 
      class of [t] is marked, then  [is_bottom t = true]
  val mark : elem -> t -> t *)

  (** Checks if the structure is inconsistent. 
      See [mark] and [unmergeable]. *)
  val is_bottom : t -> bool

  val dump : Format.formatter -> t -> unit

end

module Make (E : ELEM) (P : PARAMS) = struct

	module M = Map.Make (E)

	(* Tarjan's algorithm *)

	type infos = {
		depth : int;
		has_mark : bool;
        is_marked : bool;
	}

	type t = { 
	  c: infos M.t; (* ranks *)
	  mutable father: E.t M.t; (* mutable to allow path compression *)
	  mark_error: bool;
	}
		  
	let empty = 
	  { c = M.empty; father = M.empty; mark_error = false; }
	
	let mem elem t = M.mem elem t.c

	let add ?(marked=false) elem t =
		if not (mem elem t) then
	  	  { t with c = M.add elem {depth = 0; has_mark = marked; is_marked = marked} t.c }
		else t
		
	let rec find_aux f i = 
	  let fi = try M.find i f with Not_found -> i in
	  if (E.compare fi i) = 0 then 
		f, i
	  else 
		let f, r = find_aux f fi in 
		let f = M.add i r f in
		f, r
		  
	let find x h = 
	  let f,r = find_aux h.father x in h.father <- f; r

	let union x y h = 
	  let rx = find x h in
	  let ry = find y h in
	  if E.compare rx ry <> 0 then begin
		let rxc = M.find rx h.c in
		let ryc = M.find ry h.c in
		let mark = rxc.has_mark || ryc.has_mark in
		let error = rxc.has_mark && ryc.has_mark in
		(if error && P.exn_on_bottom then raise Bottom) ;
        (* (if error then Format.printf "Error : %a %a\n" E.print rx E.print ry); *)
		if rxc.depth > ryc.depth then
		  { c = M.add rx {rxc with has_mark = mark} h.c;
			father = M.add ry rx h.father;
			mark_error = h.mark_error || error; }
		else if rxc.depth < ryc.depth then
		  { c = M.add ry {ryc with has_mark = mark} h.c;
			father = M.add rx ry h.father;
			mark_error = h.mark_error || error; }
		else
		  { c = M.add rx {depth = rxc.depth + 1; has_mark = mark; is_marked = rxc.is_marked} h.c;
		    father = M.add ry rx h.father;
			mark_error = h.mark_error || error; }
	  end else
		h

	let map_keys m = M.fold (fun k v acc -> k :: acc) m [] |> List.rev

	let eq_classes h =
		let rec eq_classes_ acc elems = match elems with
			| [] -> acc
			| e::lst -> let rep = find e h in
			if M.mem rep acc then
				let elems_class = e::(M.find rep acc) in
				let acc = M.add rep elems_class acc in
				eq_classes_ acc lst
			else
				let elems_class = [e] in
				let acc = M.add rep elems_class acc in
				eq_classes_ acc lst
		in
		eq_classes_ M.empty (map_keys h.c)

	let doublons lst =
		let rec dl acc lst = match acc, lst with
			| true, _ -> true
			| false, (a::b::lst) -> dl (a = b) (b::lst)
			| false, _ -> false
		in dl false lst

	let is_bottom h = 
      if h.mark_error then true else 
        begin
		  let classes = eq_classes h in
		  let representants = map_keys classes in
		  let rec bottom_class acc reps = match acc, reps with
			| true, _ -> true
			| false, r::lst -> let elems = M.find r classes in
				               let elems = List.map E.category elems in
				               let elems = List.sort compare elems in
				               bottom_class (doublons elems) lst
			| false, [] -> false
		  in bottom_class false representants
	    end

    let dump fmt t =
      let pr msg = Format.fprintf fmt msg in
      let classes = eq_classes t in
      let reps = map_keys classes in
      let rec print_class cl = match cl with
        | [] -> ()
        | e::lst -> 
          begin
            pr "%a" E.print e;
            let info = M.find e t.c in
            if info.is_marked then pr "(X)";
            pr "\n"; print_class lst 
          end in
      let rec print_classes reps = match reps with
        | [] -> ()
        | e::lst -> 
          begin
            pr "%a" E.print e ;
            pr " :\n";
            print_class (M.find e classes); 
            pr "\n"; print_classes lst
          end
      in print_classes reps

end
