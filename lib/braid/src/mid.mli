open! Core

module Priority : sig
  type kind = Switch
  type t

  val neutral : t
  val reward : kind -> t
  val punish : kind -> t
end

module Node : sig
  type !'a t

  val sexp_of : 'a t -> 'a -> Sexp.t

  module Packed : sig
    type 'a node := 'a t
    type t = T : 'a node -> t [@@unpacked]

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

val if_
  :  ?sexp_of:('a -> Sexp.t)
  -> t
  -> bool Node.t
  -> then_:'a Node.t
  -> else_:'a Node.t
  -> t * 'a Node.t

module Expert : sig
  type lookup = { f : 'a. 'a Node.t -> 'a Low.Node.t }

  val lower : t -> Low.t * lookup

  val add
    :  ?name:string
    -> ?sexp_of:('a -> Sexp.t)
    -> ?priority:Priority.t
    -> t
    -> depends_on:Node.Packed.t list
    -> compute:(Low.t -> lookup -> me:'a Node.t -> unit -> 'a)
    -> t * 'a Node.t
end
