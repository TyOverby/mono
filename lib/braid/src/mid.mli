open! Core

module Node : sig
  type !'a t

  val sexp_of : 'a t -> 'a -> Sexp.t

  module Packed : sig
    type 'a node := 'a t
    type t = T : 'a node -> t

    include Comparable.S_plain with type t := t
  end
end

type t

val empty : t
val const : ?name:string -> ?sexp_of:('a -> Sexp.t) -> t -> 'a -> t * 'a Node.t

val map
  :  ?name:string
  -> ?sexp_of:('r -> Sexp.t)
  -> t
  -> 'a Node.t
  -> f:('a -> 'r)
  -> t * 'r Node.t

val map2
  :  ?name:string
  -> ?sexp_of:('r -> Sexp.t)
  -> t
  -> 'a Node.t
  -> 'b Node.t
  -> f:('a -> 'b -> 'r)
  -> t * 'r Node.t

val map3
  :  ?name:string
  -> ?sexp_of:('r -> Sexp.t)
  -> t
  -> 'a Node.t
  -> 'b Node.t
  -> 'c Node.t
  -> f:('a -> 'b -> 'c -> 'r)
  -> t * 'r Node.t

val map4
  :  ?name:string
  -> ?sexp_of:('r -> Sexp.t)
  -> t
  -> 'a Node.t
  -> 'b Node.t
  -> 'c Node.t
  -> 'd Node.t
  -> f:('a -> 'b -> 'c -> 'd -> 'r)
  -> t * 'r Node.t

module Expert : sig
  type lookup = { f : 'a. 'a Node.t -> 'a Low.Node.t }

  type ops =
    { value : 'a. 'a Node.t -> unit -> 'a
    ; has_value : 'a. 'a Node.t -> unit -> bool
    ; incr_refcount : 'a. 'a Node.t -> unit -> unit
    ; decr_refcount : 'a. 'a Node.t -> unit -> unit
    ; mark_dirty : 'a. 'a Node.t -> unit -> unit
    }

  val lower : t -> Low.t * lookup

  val add
    :  ?name:string
    -> ?sexp_of:('a -> Sexp.t)
    -> ?priority:bool
    -> ?computes_after:Node.Packed.t list
    -> t
    -> depends_on:Node.Packed.t list
    -> compute:(ops -> me:'a Node.t -> unit -> 'a)
    -> t * 'a Node.t
end
