open! Core
open! Braid.Private
open Braid.Private.High.Let_syntax

let print_env env = print_endline (Low.debug env)

let%expect_test "addition" =
  let t =
    let%bind a = High.const_node 2 in
    let%bind b = High.const_node 3 in
    High.arr2 a b ~f:( + )
  in
  let mid, mid_lookup, r = High.Expert.lower t in
  let low, low_lookup = Mid.Expert.lower mid in
  let r_mid =
    match mid_lookup.f r with
    | Node n -> n
    | _ -> assert false
  in
  let r_low = low_lookup.f r_mid in
  Low.Node.incr_refcount low r_low;
  Low.stabilize low;
  print_env low;
  [%expect
    {|
    ┌───┬───┬──────────┬───┬───┐
    │ # │ @ │ V        │ ? │ R │
    ├───┼───┼──────────┼───┼───┤
    │ 0 │   │ <filled> │ - │ 1 │
    │ 1 │   │ <filled> │ - │ 1 │
    │ 2 │   │ <filled> │ - │ 1 │
    └───┴───┴──────────┴───┴───┘ |}];
  Low.Node.read_value low r_low |> [%sexp_of: int] |> print_s;
  [%expect {| 5 |}]
;;

let%expect_test "if" =
  let t =
    let%bind cond = High.const_node true in
    let%bind a = High.const_node 2 in
    let%bind b = High.const_node 3 in
    High.if_ cond ~then_:a ~else_:b
  in
  let mid, mid_lookup, r = High.Expert.lower t in
  let low, low_lookup = Mid.Expert.lower mid in
  let r_mid =
    match mid_lookup.f r with
    | Node n -> n
    | _ -> assert false
  in
  let r_low = low_lookup.f r_mid in
  Low.Node.incr_refcount low r_low;
  Low.stabilize low;
  print_env low;
  [%expect
    {|
    ┌───┬────────┬──────────┬───┬───┐
    │ # │ @      │ V        │ ? │ R │
    ├───┼────────┼──────────┼───┼───┤
    │ 0 │        │ <filled> │ - │ 1 │
    │ 1 │ if-in  │ true     │ - │ 1 │
    │ 2 │        │ <filled> │ - │ 1 │
    │ 3 │        │ <empty>  │ x │ 0 │
    │ 4 │ if-out │ <filled> │ - │ 1 │
    └───┴────────┴──────────┴───┴───┘ |}];
  Low.Node.read_value low r_low |> [%sexp_of: int] |> print_s;
  [%expect {| 2 |}]
;;

let%expect_test "if with a constant" =
  let t =
    let%bind cond = High.const true in
    let%bind a = High.const_node 2 in
    let%bind b = High.const_node 3 in
    High.if_ cond ~then_:a ~else_:b
  in
  let mid, mid_lookup, r = High.Expert.lower t in
  let low, low_lookup = Mid.Expert.lower mid in
  let r_mid =
    match mid_lookup.f r with
    | Node n -> n
    | _ -> assert false
  in
  let r_low = low_lookup.f r_mid in
  Low.Node.incr_refcount low r_low;
  Low.stabilize low;
  print_env low;
  [%expect
    {|
    ┌───┬───┬──────────┬───┬───┐
    │ # │ @ │ V        │ ? │ R │
    ├───┼───┼──────────┼───┼───┤
    │ 0 │   │ <filled> │ - │ 1 │
    │ 1 │   │ <empty>  │ x │ 0 │
    └───┴───┴──────────┴───┴───┘ |}];
  Low.Node.read_value low r_low |> [%sexp_of: int] |> print_s;
  [%expect {| 2 |}]
;;

let%expect_test "constant prop" =
  let t =
    let%bind a = High.const 2 in
    High.arr1 a ~f:(fun a -> a + 1)
  in
  let _mid, mid_lookup, r = High.Expert.lower t in
  (match mid_lookup.f r with
  | Constant i -> print_s [%message (i : int)]
  | _ -> assert false);
  [%expect {| (i 3) |}]
;;
