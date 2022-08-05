open! Core
open! Braid.Private

let print_env env = print_endline (Low.debug env)

let%test_module "info" =
  (module struct
    open Low.Info

    let print t = print_endline (to_string ~verbose:true t)

    let%expect_test "of_int 2" =
      print (of_int 2);
      [%expect
        {|
        .--------------- refcount
        | .------------- has_cutoff
        | | .----------- value is int
        | | | .--------- has value
        | | | | .------- dirty
        v v v v v
        0 0 0 1 0
        0x2 |}]
    ;;

    let%expect_test "of_int -1" =
      print (of_int (-1));
      [%expect
        {|
                          .--------------- refcount
                          | .------------- has_cutoff
                          | | .----------- value is int
                          | | | .--------- has value
                          | | | | .------- dirty
                          v v v v v
        1152921504606846975 1 1 1 1
        0x7fffffffffffffff |}]
    ;;

    let%expect_test "of_int -1" =
      let lnot x = x lxor -1 in
      print (of_int (lnot 2));
      [%expect
        {|
                          .--------------- refcount
                          | .------------- has_cutoff
                          | | .----------- value is int
                          | | | .--------- has value
                          | | | | .------- dirty
                          v v v v v
        1152921504606846975 1 1 0 1
        0x7ffffffffffffffd |}]
    ;;

    let%expect_test "empty" =
      print init;
      [%expect
        {|
        .--------------- refcount
        | .------------- has_cutoff
        | | .----------- value is int
        | | | .--------- has value
        | | | | .------- dirty
        v v v v v
        0 0 1 0 1
        0x5 |}]
    ;;

    let%expect_test "set clean" =
      print (set_clean init);
      [%expect
        {|
        .--------------- refcount
        | .------------- has_cutoff
        | | .----------- value is int
        | | | .--------- has value
        | | | | .------- dirty
        v v v v v
        0 0 1 0 0
        0x4 |}]
    ;;

    let%expect_test "set dirty" =
      print (init |> set_clean |> set_dirty);
      [%expect
        {|
        .--------------- refcount
        | .------------- has_cutoff
        | | .----------- value is int
        | | | .--------- has value
        | | | | .------- dirty
        v v v v v
        0 0 1 0 1
        0x5 |}]
    ;;

    let%expect_test "set value isn't int" =
      print (init |> set_value_isn't_int);
      [%expect
        {|
        .--------------- refcount
        | .------------- has_cutoff
        | | .----------- value is int
        | | | .--------- has value
        | | | | .------- dirty
        v v v v v
        0 0 0 0 1
        0x1 |}]
    ;;

    let%expect_test "set value is int" =
      print (init |> set_value_isn't_int |> set_value_is_int);
      [%expect
        {|
        .--------------- refcount
        | .------------- has_cutoff
        | | .----------- value is int
        | | | .--------- has value
        | | | | .------- dirty
        v v v v v
        0 0 1 0 1
        0x5 |}]
    ;;

    let%expect_test "set has_value" =
      print (set_has_value init);
      [%expect
        {|
        .--------------- refcount
        | .------------- has_cutoff
        | | .----------- value is int
        | | | .--------- has value
        | | | | .------- dirty
        v v v v v
        0 0 1 1 1
        0x7 |}]
    ;;

    let%expect_test "set has_cutoff" =
      print (set_has_cutoff init);
      [%expect
        {|
        .--------------- refcount
        | .------------- has_cutoff
        | | .----------- value is int
        | | | .--------- has value
        | | | | .------- dirty
        v v v v v
        1 1 1 0 1
        0xd |}]
    ;;

    let%expect_test "refcounts " =
      let t = init in
      printf "%b" (is_referenced t);
      [%expect "false"];
      let t = incr_refcount init in
      print t;
      [%expect
        {|
        .--------------- refcount
        | .------------- has_cutoff
        | | .----------- value is int
        | | | .--------- has value
        | | | | .------- dirty
        v v v v v
        2 0 1 0 1
        0x15 |}];
      printf "%b" (is_referenced t);
      [%expect "true"];
      let t = incr_refcount t in
      print t;
      [%expect
        {|
        .--------------- refcount
        | .------------- has_cutoff
        | | .----------- value is int
        | | | .--------- has value
        | | | | .------- dirty
        v v v v v
        4 0 1 0 1
        0x25 |}];
      printf "%b" (is_referenced t);
      [%expect {| true |}];
      let t = decr_refcount t in
      print t;
      [%expect
        {|
        .--------------- refcount
        | .------------- has_cutoff
        | | .----------- value is int
        | | | .--------- has value
        | | | | .------- dirty
        v v v v v
        2 0 1 0 1
        0x15 |}];
      printf "%b" (is_referenced t);
      [%expect "true"];
      let t = decr_refcount t in
      print t;
      [%expect
        {|
        .--------------- refcount
        | .------------- has_cutoff
        | | .----------- value is int
        | | | .--------- has value
        | | | | .------- dirty
        v v v v v
        0 0 1 0 1
        0x5 |}];
      printf "%b" (is_referenced t);
      [%expect "false"]
    ;;
  end)
;;

let%expect_test "addition" =
  let env = Low.create ~length:3 in
  print_env env;
  [%expect
    {|
    ┌───┬───┬─────────┬───┬───┐
    │ # │ @ │ V       │ ? │ R │
    ├───┼───┼─────────┼───┼───┤
    │ 0 │   │ <empty> │ x │ 0 │
    │ 1 │   │ <empty> │ x │ 0 │
    │ 2 │   │ <empty> │ x │ 0 │
    └───┴───┴─────────┴───┴───┘ |}];
  let a = Low.next_id env in
  let b = Low.next_id env in
  let c = Low.next_id env in
  Low.prepare
    env
    ~name:"a"
    ~sexp_of:sexp_of_int
    ~compute:(fun () -> 2)
    ~depends_on:[||]
    ~depended_on_by:[| T c |]
    a;
  Low.prepare
    env
    ~name:"b"
    ~sexp_of:sexp_of_int
    ~compute:(fun () -> 3)
    ~depends_on:[||]
    ~depended_on_by:[| T c |]
    b;
  Low.prepare
    env
    ~name:"c"
    ~sexp_of:sexp_of_int
    ~compute:(fun () -> Low.Node.read_value env a + Low.Node.read_value env b)
    ~depends_on:[| T a; T b |]
    ~depended_on_by:[||]
    c;
  print_env env;
  [%expect
    {|
    ┌───┬───┬─────────┬───┬───┐
    │ # │ @ │ V       │ ? │ R │
    ├───┼───┼─────────┼───┼───┤
    │ 0 │ a │ <empty> │ x │ 0 │
    │ 1 │ b │ <empty> │ x │ 0 │
    │ 2 │ c │ <empty> │ x │ 0 │
    └───┴───┴─────────┴───┴───┘ |}];
  Expect_test_helpers_core.require_no_allocation [%here] (fun () ->
      Low.Node.incr_refcount env c);
  print_env env;
  [%expect
    {|
    ┌───┬───┬─────────┬───┬───┐
    │ # │ @ │ V       │ ? │ R │
    ├───┼───┼─────────┼───┼───┤
    │ 0 │ a │ <empty> │ x │ 1 │
    │ 1 │ b │ <empty> │ x │ 1 │
    │ 2 │ c │ <empty> │ x │ 1 │
    └───┴───┴─────────┴───┴───┘ |}];
  Expect_test_helpers_core.require_no_allocation [%here] (fun () -> Low.stabilize env);
  print_env env;
  [%expect{|
    ┌───┬───┬───┬───┬───┐
    │ # │ @ │ V │ ? │ R │
    ├───┼───┼───┼───┼───┤
    │ 0 │ a │ 2 │ - │ 1 │
    │ 1 │ b │ 3 │ - │ 1 │
    │ 2 │ c │ 5 │ - │ 1 │
    └───┴───┴───┴───┴───┘ |}];
  Expect_test_helpers_core.require_no_allocation [%here] (fun () ->
      Low.Node.write_value env a 10);
  print_env env;
  [%expect{|
    ┌───┬───┬────┬───┬───┐
    │ # │ @ │ V  │ ? │ R │
    ├───┼───┼────┼───┼───┤
    │ 0 │ a │ 10 │ - │ 1 │
    │ 1 │ b │ 3  │ - │ 1 │
    │ 2 │ c │ 5  │ x │ 1 │
    └───┴───┴────┴───┴───┘ |}];
  Expect_test_helpers_core.require_no_allocation [%here] (fun () -> Low.stabilize env);
  print_env env;
  [%expect{|
    ┌───┬───┬────┬───┬───┐
    │ # │ @ │ V  │ ? │ R │
    ├───┼───┼────┼───┼───┤
    │ 0 │ a │ 10 │ - │ 1 │
    │ 1 │ b │ 3  │ - │ 1 │
    │ 2 │ c │ 13 │ - │ 1 │
    └───┴───┴────┴───┴───┘ |}];
  Low.Node.decr_refcount env c;
  print_env env;
  [%expect{|
    ┌───┬───┬────┬───┬───┐
    │ # │ @ │ V  │ ? │ R │
    ├───┼───┼────┼───┼───┤
    │ 0 │ a │ 10 │ - │ 0 │
    │ 1 │ b │ 3  │ - │ 0 │
    │ 2 │ c │ 13 │ - │ 0 │
    └───┴───┴────┴───┴───┘ |}]
;;

let%expect_test "if" =
  let env = Low.create ~length:5 in
  print_env env;
  [%expect
    {|
    ┌───┬───┬─────────┬───┬───┐
    │ # │ @ │ V       │ ? │ R │
    ├───┼───┼─────────┼───┼───┤
    │ 0 │   │ <empty> │ x │ 0 │
    │ 1 │   │ <empty> │ x │ 0 │
    │ 2 │   │ <empty> │ x │ 0 │
    │ 3 │   │ <empty> │ x │ 0 │
    │ 4 │   │ <empty> │ x │ 0 │
    └───┴───┴─────────┴───┴───┘ |}];
  let cond = Low.next_id env in
  let switch_in = Low.next_id env in
  let a = Low.next_id env in
  let b = Low.next_id env in
  let switch_out = Low.next_id env in
  Low.prepare
    env
    ~sexp_of:[%sexp_of: int]
    ~compute:(fun () -> 0)
    ~depends_on:[||]
    ~depended_on_by:[| T switch_in |]
    ~name:"cond"
    cond;
  Low.prepare
    env
    ~sexp_of:[%sexp_of: int]
    ~compute:(fun () ->
      let prev =
        if Low.Node.has_value env switch_in then Low.Node.read_value env switch_in else -1
      in
      let next = Low.Node.read_value env cond in
      (match prev, next with
      | -1, 0 -> Low.Node.incr_refcount env a
      | -1, 1 -> Low.Node.incr_refcount env b
      | 0, 0 -> ()
      | 1, 1 -> ()
      | 0, 1 ->
        Low.Node.decr_refcount env a;
        Low.Node.incr_refcount env b;
        Low.Node.mark_dirty env switch_out
      | 1, 0 ->
        Low.Node.decr_refcount env b;
        Low.Node.incr_refcount env a;
        Low.Node.mark_dirty env switch_out
      | _ -> assert false);
      next)
    ~depends_on:[| T cond |]
    ~depended_on_by:[| T switch_out |]
    ~name:"switch_in"
    switch_in;
  Low.prepare
    env
    ~sexp_of:[%sexp_of: string]
    ~compute:(fun () -> "hello")
    ~depends_on:[||]
    ~depended_on_by:[| T switch_out |]
    ~name:"a"
    a;
  Low.prepare
    env
    ~sexp_of:[%sexp_of: string]
    ~compute:(fun () -> "world")
    ~depends_on:[||]
    ~depended_on_by:[| T switch_out |]
    ~name:"b"
    b;
  Low.prepare
    env
    ~sexp_of:[%sexp_of: string]
    ~compute:(fun () ->
      match Low.Node.read_value env switch_in with
      | 0 -> Low.Node.read_value env a
      | 1 -> Low.Node.read_value env b
      | _ -> assert false)
    ~depends_on:[| T switch_in |]
    ~depended_on_by:[||]
    ~name:"switch_out"
    switch_out;
  print_env env;
  [%expect
    {|
    ┌───┬────────────┬─────────┬───┬───┐
    │ # │ @          │ V       │ ? │ R │
    ├───┼────────────┼─────────┼───┼───┤
    │ 0 │ cond       │ <empty> │ x │ 0 │
    │ 1 │ switch_in  │ <empty> │ x │ 0 │
    │ 2 │ a          │ <empty> │ x │ 0 │
    │ 3 │ b          │ <empty> │ x │ 0 │
    │ 4 │ switch_out │ <empty> │ x │ 0 │
    └───┴────────────┴─────────┴───┴───┘ |}];
  Low.Node.incr_refcount env switch_out;
  print_env env;
  [%expect
    {|
    ┌───┬────────────┬─────────┬───┬───┐
    │ # │ @          │ V       │ ? │ R │
    ├───┼────────────┼─────────┼───┼───┤
    │ 0 │ cond       │ <empty> │ x │ 1 │
    │ 1 │ switch_in  │ <empty> │ x │ 1 │
    │ 2 │ a          │ <empty> │ x │ 0 │
    │ 3 │ b          │ <empty> │ x │ 0 │
    │ 4 │ switch_out │ <empty> │ x │ 1 │
    └───┴────────────┴─────────┴───┴───┘ |}];
  Low.stabilize env;
  print_env env;
  [%expect{|
    ┌───┬────────────┬─────────┬───┬───┐
    │ # │ @          │ V       │ ? │ R │
    ├───┼────────────┼─────────┼───┼───┤
    │ 0 │ cond       │ 0       │ - │ 1 │
    │ 1 │ switch_in  │ 0       │ - │ 1 │
    │ 2 │ a          │ hello   │ - │ 1 │
    │ 3 │ b          │ <empty> │ x │ 0 │
    │ 4 │ switch_out │ hello   │ - │ 1 │
    └───┴────────────┴─────────┴───┴───┘ |}];
  Low.Node.write_value env cond 1;
  print_env env;
  [%expect{|
    ┌───┬────────────┬─────────┬───┬───┐
    │ # │ @          │ V       │ ? │ R │
    ├───┼────────────┼─────────┼───┼───┤
    │ 0 │ cond       │ 1       │ - │ 1 │
    │ 1 │ switch_in  │ 0       │ x │ 1 │
    │ 2 │ a          │ hello   │ - │ 1 │
    │ 3 │ b          │ <empty> │ x │ 0 │
    │ 4 │ switch_out │ hello   │ - │ 1 │
    └───┴────────────┴─────────┴───┴───┘ |}];
  Low.stabilize env;
  print_env env;
  [%expect{|
    ┌───┬────────────┬───────┬───┬───┐
    │ # │ @          │ V     │ ? │ R │
    ├───┼────────────┼───────┼───┼───┤
    │ 0 │ cond       │ 1     │ - │ 1 │
    │ 1 │ switch_in  │ 1     │ - │ 1 │
    │ 2 │ a          │ hello │ - │ 0 │
    │ 3 │ b          │ world │ - │ 1 │
    │ 4 │ switch_out │ world │ - │ 1 │
    └───┴────────────┴───────┴───┴───┘ |}]
;;
