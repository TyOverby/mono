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

  val escape : Acc.t -> S1.Env.escape -> Acc.t * S2.Env.escape
  val button_click : Acc.t -> S1.Env.button_click -> Acc.t * S2.Env.button_click
  val ext : Acc.t -> S1.Env.ext -> Acc.t * S2.Env.ext
end

module Translate (S1 : S) (S2 : S) (T : Translate with module S1 = S1 and module S2 = S2) =
struct
  let rec tr_t : T.Acc.t -> S1.t -> T.Acc.t * S2.t =
   fun acc { inner; ext } ->
    let acc, inner = tr_kind acc inner in
    let acc, ext = T.ext acc ext in
    acc, { inner; ext }

  and tr_kind : T.Acc.t -> S1.kind -> T.Acc.t * S2.kind =
   fun acc -> function
    | Escape t ->
      let acc, esc = T.escape acc t in
      acc, Escape esc
    | Hbox ts ->
      let acc, ts = List.fold_map ts ~init:acc ~f:tr_t in
      acc, Hbox ts
    | Vbox ts ->
      let acc, ts = List.fold_map ts ~init:acc ~f:tr_t in
      acc, Vbox ts
    | Button { on_click; text } ->
      let acc, on_click = T.button_click acc on_click in
      acc, Button { on_click; text }
 ;;

  let translate ~init t = tr_t init t
end
