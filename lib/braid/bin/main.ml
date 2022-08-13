open! Core

let test = Braid_benchmarks.Braid_bench.tree 15 ()

let () =
  for i = 0 to 10_000 do
    test.set_input i;
    test.stabilize ()
  done
;;

let () = print_s [%message (test.get_output () : int)]
