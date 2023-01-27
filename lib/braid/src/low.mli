open! Core

type t [@@deriving sexp_of]

val debug : t -> string

module Info : sig
  type t [@@immediate]

  val of_int : int -> t
  val to_string : ?verbose:bool -> t -> string
  val init : t

  (* getters *)
  val is_dirty : t -> bool
  val has_value : t -> bool
  val has_cutoff : t -> bool
  val refcount : t -> int
  val is_referenced : t -> bool

  (* setters *)
  val set_dirty : t -> t
  val set_clean : t -> t
  val set_has_value : t -> t
  val set_has_cutoff : t -> t
  val incr_refcount : t -> t
  val decr_refcount : t -> t
end

module Node : sig
  type env := t
  type 'a t [@@immediate]
  type packed = T : 'a t -> packed [@@unboxed] [@@immediate]

  val incr_refcount : env -> _ t -> unit [@@inline always]
  val decr_refcount : env -> _ t -> unit [@@inline always]
  val is_dirty : env -> _ t -> bool [@@inline always]
  val mark_dirty : env -> _ t -> unit [@@inline always]
  val has_value : env -> _ t -> bool [@@inline always]
  val read_value : env -> 'a t -> 'a [@@inline always]
  val write_value : env -> 'a t -> 'a -> unit [@@inline always]
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

val finalize : t -> unit
val stabilize : t -> unit
