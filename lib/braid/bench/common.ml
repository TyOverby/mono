open! Core

type ('i, 'r) t =
  { set_input : 'i -> unit
  ; get_output : unit -> 'r
  ; stabilize : unit -> unit
  }
