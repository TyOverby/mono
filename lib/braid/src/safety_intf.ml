open! Core

module type S = sig
  module Option_array : sig
    type 'a t

    val init : int -> f:(int -> 'a option) -> 'a t
    val set_some : 'a t -> int -> 'a -> unit
    val set : 'a t -> int -> 'a option -> unit
    val get : 'a t -> int -> 'a option
    val is_some : 'a t -> int -> bool
    val get_some : 'a t -> int -> 'a
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
