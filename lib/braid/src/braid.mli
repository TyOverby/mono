open! Core

include module type of struct
  include High
end

module Var : sig
  type 'a t

  val read : 'a t -> 'a Value.t
end

val var : 'a -> 'a Var.t t

module type Universe =
  Braid_intf.Universe with module Value := Value with module Var := Var

val compile : 'a t -> 'a * (module Universe)

module Private : sig
  module Low = Low
  module Mid = Mid
  module High = High
end
