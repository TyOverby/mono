open! Core

include module type of struct
  include High
end

module type Universe = Braid_intf.Universe with module Value := Value

val compile : 'a t -> 'a * (module Universe)

module Private : sig
  module Low = Low
  module Mid = Mid
  module High = High
end
