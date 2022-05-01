open! Core

let () = print_s [%sexp (Uopt.is_some (Uopt.some "Uopt.none") : bool)]
