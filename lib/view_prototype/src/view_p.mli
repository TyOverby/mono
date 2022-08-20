open! Core
open! Import
module Constants = Constants

module Theme : sig
  type t

  (* val current : t Computation.t *)
  (* val set_temporarily: t Value.t -> inside:('a Computation.t) -> 'a Computation.t *)

  val override_constants : t -> f:(Constants.t -> Constants.t) -> t
end

val background_color : Theme.t -> string
val foreground_color : Theme.t -> string
val button : Theme.t -> string -> on_click:unit Effect.t -> string

module Expert : Expert_intf.S with module Theme := Theme
