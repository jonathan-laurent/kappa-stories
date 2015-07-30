module type ELEM = sig
  type t
  val compare : t -> t -> int
end

module type PRIORITY = sig
  type t
  val le : t -> t -> bool
end


exception Empty

module type S = sig

  type t
  type elem
  type priority

  val empty : t
  val is_empty : t -> bool

  val put : bool
  val remove : elem -> t -> t

  val min : t -> elem
  val extract_min : t -> priority * elem * t

end

module Make (E : ELEM) (P : PRIORITY) 
  : (S with type elem := E.t with type priority := P.t)

