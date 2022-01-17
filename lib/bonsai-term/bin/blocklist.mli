module Ucharr = Uchar
open Core
open! Bonsai_term
open! Notty
open! Notty.Infix

module Id : sig
  type t [@@deriving equal, sexp]

  include Comparator.S with type t := t
end

module Model : sig
  type 'a t =
    { children : 'a Map.M(Id).t
    ; focus : Id.t option
    }
  [@@deriving sexp, equal]
end

type maybe_do_thing =
  | Continue of unit Ui_effect.t
  | Consumed_by of unit Ui_effect.t

val effect_of_maybe_do_thing : maybe_do_thing -> unit Ui_effect.t

module Actions : sig
  type 'a t =
    { focus_up : maybe_do_thing
    ; focus_down : maybe_do_thing
    ; clear_focus : unit Ui_effect.t
    ; insert_under_cursor : 'a -> maybe_do_thing
    ; set : Id.t list -> 'a list -> maybe_do_thing
    }

  val always_continue : _ t
  val from_map : ((Id.t -> 'a t) -> 'a t) -> 'a t Map.M(Id).t -> 'a t
end

val component
  :  (module Bonsai.Model with type t = 'a)
  -> ('a Model.t * ((Id.t -> 'a Actions.t) -> 'a Actions.t)) Bonsai.Computation.t
