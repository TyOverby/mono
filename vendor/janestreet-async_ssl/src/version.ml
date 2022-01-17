open! Core
open! Import

type t =
  | Sslv23
  | Tls
  | Sslv3
  | Tlsv1
  | Tlsv1_1
  | Tlsv1_2
  | Tlsv1_3
[@@deriving sexp, compare]

let default = Tls
