open! Core
open! Notty
open! Notty.Infix
module Incr = Ui_incr
module Term = Notty_unix.Term
module Bonsai = Bonsai
module Event = Event

type t

type view =
  { width : int
  ; height : int
  ; image : I.t Lazy.t
  ; cursor : (int * int) option
  }

type event_handler := Event.t -> unit Ui_effect.t

val create
  :  ?clock:Ui_incr.Clock.t
  -> ((int * int) Bonsai.Value.t -> (view * event_handler) Bonsai.Computation.t)
  -> t

val loop : t -> unit
