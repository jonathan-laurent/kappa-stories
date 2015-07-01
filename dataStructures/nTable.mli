(** An immutable table whose elements are indexed both by their name and 
    an integer identifier. *)

type 'a t

type id
type name = string

module Id : Map.OrderedType with type t = id

val id_zero : id

val name_of_id : id -> 'a t -> name
val id_of_name : name -> 'a t -> id

val is_valid_id : id -> 'a t -> bool

val length : 'a t -> int
val read : id -> 'a t -> 'a
val read_named : name -> 'a t -> 'a

val iter : ('a -> unit) -> 'a t -> unit

val is_empty : 'a t -> bool

val create : (name * (id -> 'a)) list -> 'a t
