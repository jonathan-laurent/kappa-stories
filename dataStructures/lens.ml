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
