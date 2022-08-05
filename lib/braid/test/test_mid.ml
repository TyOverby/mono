open! Core
open! Braid.Private

let print_env env = print_endline (Low.debug env)

let compile_and_compute mid node =
  let low, lookup = Mid.Expert.lower mid in
  Low.Node.incr_refcount low (lookup.f node);
  Low.stabilize low;
  let v = Low.Node.read_value low (lookup.f node) in
  print_s (Mid.Node.sexp_of node v)
;;

let%expect_test "addition" =
  let mid = Mid.empty in
  let mid, a =
    Mid.Expert.add
      mid
      ~name:"a"
      ~sexp_of:[%sexp_of: int]
      ~depends_on:[]
      ~compute:(fun _ ~me:_ () -> 2)
  in
  let mid, b =
    Mid.Expert.add
      mid
      ~name:"b"
      ~sexp_of:[%sexp_of: int]
      ~depends_on:[]
      ~compute:(fun _ ~me:_ () -> 3)
  in
  let mid, c =
    Mid.Expert.add
      mid
      ~name:"c"
      ~sexp_of:[%sexp_of: int]
      ~depends_on:[ T a; T b ]
      ~compute:(fun ops ->
        let a = ops.value a in
        let b = ops.value b in
        fun ~me:_ () -> a () + b ())
  in
  let low, lookup = Mid.Expert.lower mid in
  print_env low;
  [%expect
    {|
    ┌───┬───┬─────────┬───┬───┐
    │ # │ @ │ V       │ ? │ R │
    ├───┼───┼─────────┼───┼───┤
    │ 0 │ a │ <empty> │ x │ 0 │
    │ 1 │ b │ <empty> │ x │ 0 │
    │ 2 │ c │ <empty> │ x │ 0 │
    └───┴───┴─────────┴───┴───┘ |}];
  Low.Node.incr_refcount low (lookup.f c);
  Low.stabilize low;
  print_env low;
  [%expect
    {|
    ┌───┬───┬───┬───┬───┐
    │ # │ @ │ V │ ? │ R │
    ├───┼───┼───┼───┼───┤
    │ 0 │ a │ 2 │ - │ 1 │
    │ 1 │ b │ 3 │ - │ 1 │
    │ 2 │ c │ 5 │ - │ 1 │
    └───┴───┴───┴───┴───┘ |}]
;;

let%expect_test "if" =
  let mid = Mid.empty in
  let mid, cond =
    Mid.Expert.add
      mid
      ~name:"cond"
      ~sexp_of:[%sexp_of: int]
      ~depends_on:[]
      ~compute:(fun _ops ~me:_ () -> 0)
  in
  let mid, a =
    Mid.Expert.add
      mid
      ~name:"a"
      ~sexp_of:[%sexp_of: string]
      ~depends_on:[]
      ~compute:(fun _ops ~me:_ () -> "hello")
  in
  let mid, b =
    Mid.Expert.add
      mid
      ~name:"b"
      ~sexp_of:[%sexp_of: string]
      ~depends_on:[]
      ~compute:(fun _ops ~me:_ () -> "world")
  in
  let rec mid__switch_in__switch_out =
    lazy
      (let mid, switch_in =
         Mid.Expert.add
           mid
           ~name:"switch in"
           ~sexp_of:[%sexp_of: int]
           ~depends_on:[ T cond ]
           ~priority:(Mid.Priority.reward Switch)
           ~compute:(fun ops ~me ->
             let incr_a = ops.incr_refcount a in
             let incr_b = ops.incr_refcount b in
             let decr_a = ops.decr_refcount a in
             let decr_b = ops.decr_refcount b in
             let (lazy (_, _, switch_out)) = mid__switch_in__switch_out in
             let mark_switch_out_dirty = ops.mark_dirty switch_out in
             let i_have_value = ops.has_value me in
             let my_previous_value = ops.value me in
             let cond_value = ops.value cond in
             fun () ->
               let prev = if i_have_value () then my_previous_value () else -1 in
               let next = cond_value () in
               if prev = next
               then ()
               else (
                 (match prev with
                 | -1 -> ()
                 | 0 -> decr_a ()
                 | 1 -> decr_b ()
                 | _ -> assert false);
                 (match next with
                 | -1 -> ()
                 | 0 -> incr_a ()
                 | 1 -> incr_b ()
                 | _ -> assert false);
                 mark_switch_out_dirty ());
               next)
       in
       let mid, switch_out =
         Mid.Expert.add
           mid
           ~name:"switch out"
           ~sexp_of:[%sexp_of: string]
           ~depends_on:[ T switch_in ]
           ~priority:(Mid.Priority.punish Switch)
           ~compute:(fun ops ~me:_ ->
             let a_value = ops.value a in
             let b_value = ops.value b in
             let in_value = ops.value switch_in in
             fun () ->
               match in_value () with
               | 0 -> a_value ()
               | 1 -> b_value ()
               | _ -> assert false)
       in
       mid, switch_in, switch_out)
  in
  let (lazy (mid, switch_in, switch_out)) = mid__switch_in__switch_out in
  let low, lookup = Mid.Expert.lower mid in
  print_env low;
  [%expect
    {|
    ┌───┬────────────┬─────────┬───┬───┐
    │ # │ @          │ V       │ ? │ R │
    ├───┼────────────┼─────────┼───┼───┤
    │ 0 │ cond       │ <empty> │ x │ 0 │
    │ 1 │ switch in  │ <empty> │ x │ 0 │
    │ 2 │ a          │ <empty> │ x │ 0 │
    │ 3 │ b          │ <empty> │ x │ 0 │
    │ 4 │ switch out │ <empty> │ x │ 0 │
    └───┴────────────┴─────────┴───┴───┘ |}];
  Low.Node.incr_refcount low (lookup.f switch_out);
  Low.stabilize low;
  print_env low;
  [%expect
    {|
    ┌───┬────────────┬─────────┬───┬───┐
    │ # │ @          │ V       │ ? │ R │
    ├───┼────────────┼─────────┼───┼───┤
    │ 0 │ cond       │ 0       │ - │ 1 │
    │ 1 │ switch in  │ 0       │ - │ 1 │
    │ 2 │ a          │ hello   │ - │ 1 │
    │ 3 │ b          │ <empty> │ x │ 0 │
    │ 4 │ switch out │ hello   │ - │ 1 │
    └───┴────────────┴─────────┴───┴───┘ |}];
  Low.Node.write_value low (lookup.f cond) 1;
  Low.stabilize low;
  print_env low;
  [%expect
    {|
    ┌───┬────────────┬───────┬───┬───┐
    │ # │ @          │ V     │ ? │ R │
    ├───┼────────────┼───────┼───┼───┤
    │ 0 │ cond       │ 1     │ - │ 1 │
    │ 1 │ switch in  │ 1     │ - │ 1 │
    │ 2 │ a          │ hello │ - │ 0 │
    │ 3 │ b          │ world │ - │ 1 │
    │ 4 │ switch out │ world │ - │ 1 │
    └───┴────────────┴───────┴───┴───┘ |}]
;;

let%expect_test "pretty addition" =
  let mid = Mid.empty in
  let mid, a = Mid.const mid ~name:"a" ~sexp_of:[%sexp_of: int] 2 in
  let mid, b = Mid.const mid ~name:"b" ~sexp_of:[%sexp_of: int] 3 in
  let mid, c = Mid.map2 mid ~name:"c" ~sexp_of:[%sexp_of: int] a b ~f:( + ) in
  compile_and_compute mid c;
  [%expect {| 5 |}]
;;

let%expect_test "pretty if" =
  let mid = Mid.empty in
  let mid, cond = Mid.const mid ~name:"cond" ~sexp_of:[%sexp_of: bool] true in
  let mid, a = Mid.const mid ~name:"a" ~sexp_of:[%sexp_of: int] 3 in
  let mid, a' = Mid.map mid ~name:"a'" ~sexp_of:[%sexp_of: int] a ~f:(fun a -> a + 1) in
  let mid, b = Mid.const mid ~name:"b" ~sexp_of:[%sexp_of: int] 5 in
  let mid, b' = Mid.map mid ~name:"b'" ~sexp_of:[%sexp_of: int] b ~f:(fun a -> a + 1) in
  let mid, out = Mid.if_ mid cond ~then_:a ~else_:b in
  (* *)
  let low, lookup = Mid.Expert.lower mid in
  print_env low;
  [%expect
    {|
    ┌───┬────────┬─────────┬───┬───┐
    │ # │ @      │ V       │ ? │ R │
    ├───┼────────┼─────────┼───┼───┤
    │ 0 │ cond   │ <empty> │ x │ 0 │
    │ 1 │ if-in  │ <empty> │ x │ 0 │
    │ 2 │ a      │ <empty> │ x │ 0 │
    │ 3 │ a'     │ <empty> │ x │ 0 │
    │ 4 │ b      │ <empty> │ x │ 0 │
    │ 5 │ b'     │ <empty> │ x │ 0 │
    │ 6 │ if-out │ <empty> │ x │ 0 │
    └───┴────────┴─────────┴───┴───┘ |}];
  Low.Node.incr_refcount low (lookup.f out);
  print_env low;
  [%expect
    {|
    ┌───┬────────┬─────────┬───┬───┐
    │ # │ @      │ V       │ ? │ R │
    ├───┼────────┼─────────┼───┼───┤
    │ 0 │ cond   │ <empty> │ x │ 1 │
    │ 1 │ if-in  │ <empty> │ x │ 1 │
    │ 2 │ a      │ <empty> │ x │ 0 │
    │ 3 │ a'     │ <empty> │ x │ 0 │
    │ 4 │ b      │ <empty> │ x │ 0 │
    │ 5 │ b'     │ <empty> │ x │ 0 │
    │ 6 │ if-out │ <empty> │ x │ 1 │
    └───┴────────┴─────────┴───┴───┘ |}];
  Low.stabilize low;
  print_env low;
  [%expect
    {|
    ┌───┬────────┬─────────┬───┬───┐
    │ # │ @      │ V       │ ? │ R │
    ├───┼────────┼─────────┼───┼───┤
    │ 0 │ cond   │ true    │ - │ 1 │
    │ 1 │ if-in  │ true    │ - │ 1 │
    │ 2 │ a      │ 3       │ - │ 1 │
    │ 3 │ a'     │ <empty> │ x │ 0 │
    │ 4 │ b      │ <empty> │ x │ 0 │
    │ 5 │ b'     │ <empty> │ x │ 0 │
    │ 6 │ if-out │ 3       │ - │ 1 │
    └───┴────────┴─────────┴───┴───┘ |}]
;;
