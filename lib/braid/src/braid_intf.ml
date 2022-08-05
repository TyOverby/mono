open! Core

module type Universe = sig
  module Value : sig
    type 'a t
  end

  val is_computed : 'a Value.t -> (unit -> bool) Staged.t
  val value_exn : 'a Value.t -> (unit -> 'a) Staged.t
  val unsafe_value : 'a Value.t -> (unit -> 'a) Staged.t
  val stabilize : unit -> unit
  val watch : 'a Value.t -> (unit -> unit) Staged.t
  val unwatch : 'a Value.t -> (unit -> unit) Staged.t
  val set : 'a Value.t -> ('a -> unit) Staged.t
end
