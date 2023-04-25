open! Core
open! Incremental
open! Incremental.Let_syntax
open Common

type ('i, 'r) f =
  { f :
      'w.
      (module Incremental.S with type state_witness = 'w)
      -> ('i, 'w) Incremental.Var.t * ('r, 'w) Incremental.Observer.t
  }

let bench_incr (type i r) (f : (i, r) f) =
  let module Incr = Incremental.Make () in
  Incr.State.set_max_height_allowed Incr.State.t 10_000;
  let input, output = f.f (module Incr) in
  let set_input v = Incr.Var.set input v in
  let get_output () = Incr.Observer.value_exn output in
  { set_input; get_output; stabilize = Incr.stabilize }
;;

let single_addition () =
  bench_incr
    { f =
        (fun (type a) (module Incr : Incremental.S with type state_witness = a) ->
          let var = Incr.Var.create 0 in
          let node = Incr.map (Incr.Var.watch var) ~f:(fun a -> a + 1) in
          let observer = Incr.observe node in
          var, observer)
    }
;;

let n_additions n () =
  let rec make i prev =
    if i = 0 then prev else make (i - 1) (Incremental.map prev ~f:(fun a -> a + 1))
  in
  bench_incr
    { f =
        (fun (type a) (module Incr : Incremental.S with type state_witness = a) ->
          let var = Incr.Var.create 0 in
          let node = make n (Incr.Var.watch var) in
          let observer = Incr.observe node in
          var, observer)
    }
;;

let tree n () =
  let rec make i prev =
    if i = 0
    then prev
    else (
      let left =
        let left = Incremental.map prev ~f:(fun a -> a + 1) in
        make (i - 1) left
      in
      let right =
        let right = Incremental.map prev ~f:(fun a -> a * 2) in
        make (i - 1) right
      in
      Incremental.map2 left right ~f:(fun a b -> a + b))
  in
  bench_incr
    { f =
        (fun (type a) (module Incr : Incremental.S with type state_witness = a) ->
          let var = Incr.Var.create 0 in
          let node = make n (Incr.Var.watch var) in
          let observer = Incr.observe node in
          var, observer)
    }
;;

let tree_with_tuples n () =
  let rec make i prev =
    if i = 0
    then prev
    else (
      let left =
        let left = Incremental.map prev ~f:(fun a -> a + 1) in
        make (i - 1) left
      in
      let right =
        let right = Incremental.map prev ~f:(fun a -> a * 2) in
        make (i - 1) right
      in
      Incremental.both left right |> Incremental.map ~f:(fun (a, b) -> a + b))
  in
  bench_incr
    { f =
        (fun (type a) (module Incr : Incremental.S with type state_witness = a) ->
          let var = Incr.Var.create 0 in
          let node = make n (Incr.Var.watch var) in
          let observer = Incr.observe node in
          var, observer)
    }
;;
