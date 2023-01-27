open! Core
open! Braid.Private
open! Braid.Private.High.Let_syntax

let run_tests tests inputs =
  let tests = List.map tests ~f:(fun (name, f) -> name, f ()) in
  List.iter inputs ~f:(fun input ->
    List.iter tests ~f:(fun ((name, test) : _ * _ Braid_benchmarks.Common.t) ->
      test.set_input input;
      test.stabilize ();
      let output = test.get_output () in
      print_s [%message "" ~test:(name : string) (output : int)];
      ()))
;;

let%expect_test "single_addition" =
  run_tests Braid_benchmarks.Benchmarks.single_addition [ 0; 1; 5; 10 ];
  [%expect
    {|
    ((test braid) (output 1))
    ((test incremental) (output 1))
    ((test function) (output 1))
    ((test braid) (output 2))
    ((test incremental) (output 2))
    ((test function) (output 2))
    ((test braid) (output 6))
    ((test incremental) (output 6))
    ((test function) (output 6))
    ((test braid) (output 11))
    ((test incremental) (output 11))
    ((test function) (output 11)) |}]
;;
