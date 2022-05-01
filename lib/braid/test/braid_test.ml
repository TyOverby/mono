open! Core
open! Braid

let%expect_test _ =
  print_s [%sexp (Uopt.is_some (Uopt.some "Uopt.none") : bool)];
  [%expect {| true |}]
;;
