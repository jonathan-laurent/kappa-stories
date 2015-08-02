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


  (** A quick consistency check. 
      If [quick_check t = false], then [is_bottom t = true] but
      the converse is not true. *)
  val quick_check : t -> bool


  val dump : Format.formatter -> t -> unit

end

module Make (E : ELEM) (P : PARAMS) : S with type elem := E.t

