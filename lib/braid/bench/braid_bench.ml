open! Core
open! Braid
open! Braid.Let_syntax
open Common

let bench_braid t =
  let (input, output), (module M) = Braid.compile t in
  unstage (M.watch output) ();
  let set_input = unstage (M.set input) in
  let get_output = unstage (M.unsafe_value output) in
  { set_input; get_output; stabilize = M.stabilize }
;;

let single_addition () =
  bench_braid
    (let%bind a = var 0 in
     let%bind b = arr1 (Var.read a) ~f:(fun a -> a + 1) in
     return (a, b))
;;

let n_additions n () =
  let rec make i prev =
    if i = 0
    then return prev
    else (
      let%bind me = arr1 prev ~f:(fun a -> a + 1) in
      make (i - 1) me)
  in
  bench_braid
    (let%bind a = var 0 in
     let%bind b = make n (Var.read a) in
     return (a, b))
;;

let tree n () =
  let rec make i prev =
    if i = 0
    then return prev
    else (
      let%bind left =
        let%bind left = arr1 prev ~f:(fun a -> a + 1) in
        make (i - 1) left
      in
      let%bind right =
        let%bind right = arr1 prev ~f:(fun a -> a * 2) in
        make (i - 1) right
      in
      arr2 left right ~f:(fun a b -> a + b))
  in
  let t =
    let%bind a = var 0 in
    let%bind b = make n (Var.read a) in
    return (a, b)
  in
  bench_braid t
;;
