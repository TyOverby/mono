open! Core
open! Braid
open Braid.Let_syntax
open Common

let single_addition =
  [ "braid", Braid_bench.single_addition
  ; "incremental", Incr_bench.single_addition
  ; "function", Ocaml_bench.single_addition
  ]
;;

let n_additions =
  [ "braid (1000)", Braid_bench.n_additions 1000
  ; "incremental (100)", Incr_bench.n_additions 1000
  ; "ocaml (1000)", Ocaml_bench.n_additions 1000
  ]
;;

let tree =
  [ "braid", Braid_bench.tree 15
  ; "incremental", Incr_bench.tree 15
  ; "ocaml", Ocaml_bench.tree 15
    (*; "incremental (with tuples)", Incr_bench.tree_with_tuples 15 *)
  ]
;;

let%bench_fun ("single addition" [@params t = single_addition]) =
  let t = t () in
  let i = ref 0 in
  fun () ->
    t.set_input !i;
    incr i;
    t.stabilize ();
    t.get_output ()
;;

let%bench_fun ("n_additions" [@params t = n_additions]) =
  let t = t () in
  let i = ref 0 in
  fun () ->
    t.set_input !i;
    incr i;
    t.stabilize ();
    t.get_output ()
;;

let%bench_fun ("(all cutoff) additions" [@params t = n_additions]) =
  let t = t () in
  fun () ->
    t.stabilize ();
    t.get_output ()
;;

let%bench_fun ("tree" [@params t = tree]) =
  let t = t () in
  let i = ref 0 in
  fun () ->
    t.set_input !i;
    incr i;
    t.stabilize ();
    t.get_output ()
;;
