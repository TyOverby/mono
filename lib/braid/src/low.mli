open! Core

type t [@@deriving sexp_of]

val debug : t -> string

module Node : sig
  type env := t
  type 'a t [@@immediate]
  type packed = T : 'a t -> packed [@@unboxed]

  val incr_refcount : env -> _ t -> unit
  val decr_refcount : env -> _ t -> unit
  val is_dirty : env -> _ t -> bool
  val mark_dirty : env -> _ t -> unit
  val has_value : env -> _ t -> bool
  val read_value : env -> 'a t -> 'a
  val write_value : env -> 'a t -> 'a -> unit
end

val create : length:int -> t
val next_id : t -> 'a Node.t

val prepare
  :  t
  -> ?sexp_of:('a -> Core.Sexp.t)
  -> ?name:string
  -> compute:(unit -> 'a)
  -> depends_on:Node.packed array
  -> depended_on_by:Node.packed array
  -> 'a Node.t
  -> unit

val stabilize : t -> unit
