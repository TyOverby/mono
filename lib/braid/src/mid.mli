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
  -> then_effects:Node.Packed.t list
  -> else_:'a Node.t
  -> else_effects:Node.Packed.t list
  -> t * 'a Node.t

val state
  :  ?sexp_of:('a -> Sexp.t)
  -> ?name:string
  -> t
  -> init:'a
  -> t * 'a Node.t * (('a -> 'a) -> unit) Node.t

val state'
  :  ?sexp_of:('a -> Sexp.t)
  -> ?name:string
  -> t
  -> init:'a Node.t
  -> t * 'a Node.t * (('a -> 'a) -> unit) Node.t

val on_stabilization1 : t -> 'a Node.t -> f:('a -> unit) -> t * unit Node.t
val on_stabilization0 : t -> f:(unit -> unit) -> t * unit Node.t

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

  val add2
    :  t
    -> fa:(t -> 'b Node.t Lazy.t -> t * 'a Node.t)
    -> fb:(t -> 'a Node.t -> t * 'b Node.t)
    -> t * 'a Node.t * 'b Node.t
end
