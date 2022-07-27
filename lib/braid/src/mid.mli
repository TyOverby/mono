open! Core

module Node : sig
  type 'a t

  module Packed : sig
    type 'a node := 'a t
    type t = T : 'a node -> t

    include Comparable.S_plain with type t := t
  end
end

type t

val empty : t

type ops =
  | T :
      { eval : 'a. 'a Node.t -> 'env -> 'a
      ; incr_refcount : 'a. 'a Node.t -> 'env -> unit
      ; decr_refcount : 'a. 'a Node.t -> 'env -> unit
      ; mark_dirty : 'a. 'a Node.t -> 'env -> unit
      ; env : 'env
      }
      -> ops

val add
  :  ?name:string
  -> ?sexp_of:('a -> Core.Sexp.t)
  -> ?priority:bool
  -> t
  -> depends_on:Node.Packed.Set.t
  -> compute:(ops -> unit -> 'a)
  -> t * 'a Node.t
