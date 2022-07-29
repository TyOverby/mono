open! Core
open! Braid
open Braid.High.Let_syntax

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
