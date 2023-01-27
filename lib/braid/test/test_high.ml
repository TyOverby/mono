open! Core
open! Braid.Private
open Braid
open Braid.Private.High.Let_syntax

let print_env env = print_endline (Low.debug env)

let%expect_test "addition" =
  let t =
    let%bind a = High.const_node 2 in
    let%bind b = High.const_node 3 in
    High.arr2 a b ~f:( + )
  in
  let mid, mid_lookup, _eff, r = High.Expert.lower t in
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
    let a = High.const_node 2 in
    let b = High.const_node 3 in
    High.if_ cond ~then_:a ~else_:b
  in
  let mid, mid_lookup, _eff, r = High.Expert.lower t in
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
    let a = High.const_node 2 in
    let b = High.const_node 3 in
    High.if_ cond ~then_:a ~else_:b
  in
  let mid, mid_lookup, _eff, r = High.Expert.lower t in
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
    └───┴───┴──────────┴───┴───┘ |}];
  Low.Node.read_value low r_low |> [%sexp_of: int] |> print_s;
  [%expect {| 2 |}]
;;

let%expect_test "constant prop" =
  let t =
    let%bind a = High.const 2 in
    High.arr1 a ~f:(fun a -> a + 1)
  in
  let _mid, mid_lookup, _eff, r = High.Expert.lower t in
  (match mid_lookup.f r with
   | Constant i -> print_s [%message (i : int)]
   | _ -> assert false);
  [%expect {| (i 3) |}]
;;

let%expect_test "addition" =
  let t =
    let%bind a, set_a = High.state 2 in
    let%bind b, set_b = High.state 3 in
    let%bind res = High.arr2 a b ~f:( + ) in
    return (res, set_a, set_b)
  in
  let mid, mid_lookup, _eff, (r, set_a, set_b) = High.Expert.lower t in
  let low, low_lookup = Mid.Expert.lower mid in
  let r =
    match mid_lookup.f r with
    | Node n -> low_lookup.f n
    | _ -> assert false
  in
  let set_a =
    match mid_lookup.f set_a with
    | Node n -> low_lookup.f n
    | _ -> assert false
  in
  let set_b =
    match mid_lookup.f set_b with
    | Node n -> low_lookup.f n
    | _ -> assert false
  in
  Low.Node.incr_refcount low r;
  Low.Node.incr_refcount low set_a;
  Low.Node.incr_refcount low set_b;
  Low.stabilize low;
  print_env low;
  [%expect
    {|
    ┌───┬───┬──────────┬───┬───┐
    │ # │ @ │ V        │ ? │ R │
    ├───┼───┼──────────┼───┼───┤
    │ 0 │   │ <filled> │ - │ 2 │
    │ 1 │   │ <filled> │ - │ 1 │
    │ 2 │   │ <filled> │ - │ 2 │
    │ 3 │   │ <filled> │ - │ 1 │
    │ 4 │   │ <filled> │ - │ 1 │
    │ 5 │   │ <filled> │ - │ 1 │
    │ 6 │   │ <filled> │ - │ 1 │
    └───┴───┴──────────┴───┴───┘ |}];
  Low.Node.read_value low r |> [%sexp_of: int] |> print_s;
  [%expect {| 5 |}];
  (Low.Node.read_value low set_a) (fun _ -> 10);
  Low.stabilize low;
  Low.Node.read_value low r |> [%sexp_of: int] |> print_s;
  [%expect {| 13 |}];
  (Low.Node.read_value low set_b) (fun _ -> 30);
  Low.stabilize low;
  Low.Node.read_value low r |> [%sexp_of: int] |> print_s;
  [%expect {| 40 |}]
;;

let%test_module "covariance" =
  (module struct
    let f : [< `A | `B | `E ] Value.t -> unit = fun _ -> ()
    let g : [> `A ] Value.t -> unit = fun _ -> ()

    let _ =
      let%bind get, set = High.state `A in
      let _get_cast = (get :> [ `A | `B | `C ] Value.t) in
      let _set_cast = (set :> (([ `A | `B | `D ] -> [ `A | `B ]) -> unit) Value.t) in
      let _auto_casted1 = f get in
      let _auto_casted2 = g get in
      return ()
    ;;

    let _ =
      let%bind get, set = High.state None in
      let _set_cast = (set :> (([ `A | `B ] option -> [ `A ] option) -> unit) Value.t) in
      let _get_cast = (get :> [< `A | `B > `A ] option High.Value.t) in
      return ()
    ;;
  end)
;;

let%expect_test "side-effect inside an if" =
  let t =
    let%bind cond, set_cond = High.state true in
    let%bind res =
      High.if_
        cond
        ~then_:
          (let%bind () = High.on_stabilization0 (fun () -> print_endline "hello") in
           High.const_node 2)
        ~else_:(High.const 3)
    in
    High.arr2 res set_cond ~f:Tuple2.create
  in
  let mid, mid_lookup, _eff, r = High.Expert.lower t in
  let low, low_lookup = Mid.Expert.lower mid in
  let r_mid =
    match mid_lookup.f r with
    | Node n -> n
    | _ -> assert false
  in
  let r_low = low_lookup.f r_mid in
  Low.Node.incr_refcount low r_low;
  Low.stabilize low;
  Low.Node.read_value low r_low |> fst |> [%sexp_of: int] |> print_s;
  [%expect {|
    hello
    2 |}];
  Low.stabilize low;
  [%expect {| hello |}];
  let _, setter = Low.Node.read_value low r_low in
  setter (Fn.const false);
  Low.stabilize low;
  [%expect {| |}]
;;

let%expect_test "top-level side-effect" =
  let t = High.on_stabilization0 (fun () -> print_endline "hello") in
  let mid, _mid_lookup, eff, _r = High.Expert.lower t in
  let low, low_lookup = Mid.Expert.lower mid in
  assert (List.length eff = 1);
  List.iter eff ~f:(fun (T node) ->
    let node = low_lookup.f node in
    Low.Node.incr_refcount low node);
  Low.stabilize low;
  [%expect {| hello |}]
;;

let%expect_test "top-level side-effect inside an ignored if" =
  let t =
    let%bind cond, set_cond = High.state true in
    let%bind _ =
      High.if_
        cond
        ~then_:
          (let%bind () = High.on_stabilization0 (fun () -> print_endline "hello") in
           const ())
        ~else_:(const ())
    in
    return set_cond
  in
  let mid, _mid_lookup, eff, _r = High.Expert.lower t in
  let low, low_lookup = Mid.Expert.lower mid in
  List.iter eff ~f:(fun (T node) ->
    let node = low_lookup.f node in
    Low.Node.incr_refcount low node);
  Low.stabilize low;
  [%expect {| hello |}]
;;
