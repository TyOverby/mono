open! Core
open Common

let bench_fun start ~f =
  let input = ref start in
  let output = ref (f start) in
  let set_input v = input := v in
  let get_output () = !output in
  let stabilize () = output := f !input in
  { set_input; get_output; stabilize }
;;

let single_addition () = bench_fun 0 ~f:(fun i -> i + 1)

let n_additions n () =
  bench_fun 0 ~f:(fun i ->
      let acc = ref i in
      for _ = 0 to n - 1 do
        acc := !acc + 1
      done;
      !acc)
;;

let tree n () =
  let rec f i prev =
    if i = 0
    then prev
    else (
      let left = f (i - 1) (prev + 1) in
      let right = f (i - 1) (prev * 2) in
      left + right)
  in
  bench_fun 0 ~f:(f n)
;;
