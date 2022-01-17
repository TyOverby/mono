open! Core

type special =
  [ `Escape
  | `Enter
  | `Tab
  | `Backspace
  | `Insert
  | `Delete
  | `Home
  | `End
  | `Arrow of [ `Up | `Down | `Left | `Right ]
  | `Page of [ `Up | `Down ]
  | `Function of int
  ]
[@@deriving sexp, equal]

type button =
  [ `Left
  | `Middle
  | `Right
  | `Scroll of [ `Up | `Down ]
  ]
[@@deriving sexp, equal]

type mods = [ `Meta | `Ctrl | `Shift ] list [@@deriving sexp, equal]

type key = [ special | `Uchar of Uchar.t | `ASCII of char ] * mods
[@@deriving sexp, equal]

type mouse = [ `Press of button | `Drag | `Release ] * (int * int) * mods
[@@deriving sexp, equal]

type paste =
  [ `Start
  | `End
  ]
[@@deriving sexp, equal]

type event =
  [ `Key of key
  | `Mouse of mouse
  | `Paste of paste
  ]
[@@deriving sexp, equal]

type t =
  [ event
  | `Resize of int * int
  | `End
  ]
[@@deriving sexp, equal]
