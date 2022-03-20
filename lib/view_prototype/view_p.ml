open! Core

module type Env = sig
  type escape
  type ext
  type button_click

  type 'a t =
    { inner : 'a
    ; ext : ext
    }
end

module type S = sig
  module Env : Env
  open! Env

  type kind =
    | Escape of escape
    | Hbox of t list
    | Vbox of t list
    | Button of
        { on_click : Env.button_click
        ; text : string
        }

  and t = kind Env.t
end

module View_t (Env : Env) : S = struct
  module Env = Env
  open! Env

  type kind =
    | Escape of escape
    | Hbox of t list
    | Vbox of t list
    | Button of
        { on_click : button_click
        ; text : string
        }

  and t = kind Env.t
end

module type Translate = sig
  module S1 : S
  module S2 : S
  module Acc : T

  val escape : Acc.t -> S1.Env.escape -> S2.Env.escape
  val button_click : Acc.t -> S1.Env.button_click -> S2.Env.button_click
  val ext : Acc.t -> S1.Env.ext -> S2.Env.ext
end

module Translate (S1 : S) (S2 : S) (T : Translate with module S1 = S1 and module S2 = S2) =
struct
  let rec tr_t : T.Acc.t -> S1.t -> S2.t =
   fun acc { inner; ext } -> { inner = tr_kind acc inner; ext = T.ext acc ext }

  and tr_kind : T.Acc.t -> S1.kind -> S2.kind =
   fun acc -> function
    | Escape t -> Escape (T.escape acc t)
    | Hbox ts -> Hbox (List.map ts ~f:(tr_t acc))
    | Vbox ts -> Vbox (List.map ts ~f:(tr_t acc))
    | Button { on_click; text } -> Button { on_click = T.button_click acc on_click; text }
 ;;
end
