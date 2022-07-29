open! Core
open! Braid

let print_env env = print_endline (Low.debug env)

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
  Low.Node.incr_refcount env c;
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
  Low.stabilize env;
  print_env env;
  [%expect
    {|
    ┌───┬───┬───┬───┬───┐
    │ # │ @ │ V │ ? │ R │
    ├───┼───┼───┼───┼───┤
    │ 0 │ a │ 2 │ - │ 1 │
    │ 1 │ b │ 3 │ - │ 1 │
    │ 2 │ c │ 5 │ - │ 1 │
    └───┴───┴───┴───┴───┘ |}];
  Low.Node.write_value env a 10;
  print_env env;
  [%expect
    {|
    ┌───┬───┬────┬───┬───┐
    │ # │ @ │ V  │ ? │ R │
    ├───┼───┼────┼───┼───┤
    │ 0 │ a │ 10 │ - │ 1 │
    │ 1 │ b │ 3  │ - │ 1 │
    │ 2 │ c │ 5  │ x │ 1 │
    └───┴───┴────┴───┴───┘ |}];
  Low.stabilize env;
  print_env env;
  [%expect
    {|
    ┌───┬───┬────┬───┬───┐
    │ # │ @ │ V  │ ? │ R │
    ├───┼───┼────┼───┼───┤
    │ 0 │ a │ 10 │ - │ 1 │
    │ 1 │ b │ 3  │ - │ 1 │
    │ 2 │ c │ 13 │ - │ 1 │
    └───┴───┴────┴───┴───┘ |}];
  Low.Node.decr_refcount env c;
  print_env env;
  [%expect
    {|
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
  [%expect
    {|
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
  [%expect
    {|
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
  [%expect
    {|
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
