open Util (* (|-) composition operator *)


type ('t, 'p) lens = {
	get_ : 't -> 'p ;
	set_ : 'p -> 't -> 't
}


let set v t l = l.set_ v t

let get t l = l.get_ t

let modify l f t = set (f (get t l)) t l

let compose l1 l2 = {
  get_ = l2.get_ |- l1.get_;
  set_ = l1.set_ |- modify l2
}

(* Infix operators *)

let (|.)         = get
let (^=) l v     = fun a -> set v a l
let (^%=)        = modify
let (|--) l1 l2  = compose l2 l1
let (--|)        = compose



module LensExtMap (Map : EXT_MAP_S) = struct

  type 'a t = 'a Map.t
  type key = Map.key

  let make ?def (k : key) = 
    
    let set = (fun v m -> Map.add k v m) in
    let get = 
      match def with 
      | None -> (fun m -> assert (Map.mem k m) ; Map.find k m)
      | Some def -> (fun m -> Map.lookup_def ~def:def k m) in

    { get_ = get ; set_ = set }


end
