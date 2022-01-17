open! Core
open! Notty
open! Notty.Infix
module Term = Notty_unix.Term
module Event = Event
module Handle = Handle

type view = Handle.view =
  { width : int
  ; height : int
  ; image : I.t Lazy.t
  ; cursor : (int * int) option
  }
