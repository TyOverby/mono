open! Core
open! Furby
open! Furby.Let_syntax

let eval_and_print sexp_of t = t |> eval |> sexp_of |> print_s

let%expect_test "float constant" =
  eval_and_print [%sexp_of: float] (Furby.float_const 0.0);
  [%expect {| 0 |}]
;;

let%expect_test "f_bool constant" =
  eval_and_print [%sexp_of: F_bool.t] (Furby.bool_const (F_bool.of_bool true));
  [%expect {| true |}];
  eval_and_print [%sexp_of: F_bool.t] (Furby.bool_const (F_bool.of_bool false));
  [%expect {| false |}];
  eval_and_print [%sexp_of: F_bool.t] (Furby.bool_const (F_bool.of_float 0.2));
  [%expect {| 0.2 |}]
;;

let%expect_test "if" =
  eval_and_print
    [%sexp_of: F_bool.t]
    (let%sub a = Furby.bool_const (F_bool.of_bool true) in
     a);
  [%expect {| true |}]
;;

let%expect_test "if" =
  eval_and_print
    [%sexp_of: float]
    (if%sub Furby.bool_const (F_bool.of_float 0.2)
    then Furby.float_const 0.0
    else Furby.float_const 10.0);
  [%expect {| 8 |}]
;;

let%expect_test "lt" =
  let test ~falloff =
    eval_and_print
      [%sexp_of: F_bool.t]
      (Furby.float_const 9.0
      < Furby.float_const 8.0
      |> with_falloff ~falloff:(Furby.float_const falloff))
  in
  test ~falloff:1.;
  [%expect {| false |}];
  test ~falloff:4.;
  [%expect {| 0.75 |}]
;;
