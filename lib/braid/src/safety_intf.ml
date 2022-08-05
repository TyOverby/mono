open! Core

module type S = sig
  module Obj_array : sig
    type t

    val init_empty : int -> t
    val get_some : t -> int -> Obj.t
    val set_some : t -> int -> Obj.t -> unit
    val set_some_int_assuming_currently_int : t -> int -> Obj.t -> unit
    val set_some_assuming_currently_int : t -> int -> Obj.t -> unit
    val set_some_int : t -> int -> Obj.t -> unit
  end

  module Array : sig
    type 'a t [@@deriving sexp_of]

    val empty : 'a t
    val of_array : 'a array -> 'a t
    val init : int -> f:(int -> 'a) -> 'a t
    val create : len:int -> 'a -> 'a t
    val set : 'a t -> int -> 'a -> unit
    val get : 'a t -> int -> 'a
    val length : _ t -> int
  end
end
