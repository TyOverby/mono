open! Core
open Safety

type int16_array =
  ( int
  , Stdlib.Bigarray.int16_unsigned_elt
  , Stdlib.Bigarray.c_layout )
  Stdlib.Bigarray.Array1.t

let _new_int16_array length : int16_array =
  Stdlib.Bigarray.(Array1.create int16_unsigned c_layout length)
;;

type info = int Array.t
type depends_on = int Array.t
type depends_on_idx = int
type depended_on_by = int Array.t
type depended_on_by_idx = int

let uint_32_max = Int.pow 2 32 - 1
let[@inline always] info info i = Array.get info ((i * 2) + 0)

let[@inline always] set_info (info : int array) (i : int) (v : int) =
  Stdlib.Array.unsafe_set info (i * 2) v
;;

(* Array.set info ((i * 2) + 0) v *)

let[@inline always] depends_on_idx info i = Array.get info ((i * 2) + 1) lsr 32

let[@inline always] depended_on_by_idx info i =
  Array.get info ((i * 2) + 1) land uint_32_max
;;

let[@inline always] depends_on_length depends_on idx = Array.get depends_on idx

let[@inline always] get_depends_on depends_on idx offset =
  Array.get depends_on (idx + offset + 1)
;;

let[@inline always] depended_on_by_length depended_on_by idx =
  Array.get depended_on_by idx
;;

let[@inline always] get_depended_on_by depended_on_by idx offset =
  Array.get depended_on_by (idx + offset + 1)
;;

module Builder = struct
  type t =
    { mutable nodes : (int Array.t * int Array.t) Int.Map.t
    ; mutable count : int
    ; info_init : int
    }

  let create info_init = { nodes = Int.Map.empty; count = 0; info_init }

  let add t id ~depends_on ~depended_on_by =
    let depends_on = Array.of_array depends_on in
    let depended_on_by = Array.of_array depended_on_by in
    t.nodes <- Map.add_exn t.nodes ~key:id ~data:(depends_on, depended_on_by);
    t.count <- t.count + 1
  ;;

  let _uint_16_max = Int.pow 2 16 - 1

  let finalize t =
    (* todo: assert that all nodes have been prepared *)
    let nodes = t.nodes in
    let a_size, b_size =
      Map.fold nodes ~init:(0, 0) ~f:(fun ~key:_ ~data:(a, b) (a_size, b_size) ->
          let a = a_size + 1 + Array.length a in
          let b = b_size + 1 + Array.length b in
          a, b)
    in
    let info = Array.init (Map.length nodes * 2) ~f:(fun _ -> 0) in
    let a_arr = Array.create ~len:a_size 0 in
    let b_arr = Array.create ~len:b_size 0 in
    let a_idx = ref 0 in
    let b_idx = ref 0 in
    Map.iteri nodes ~f:(fun ~key:i ~data:(a, b) ->
        Array.set info ((i * 2) + 0) t.info_init;
        Array.set info ((i * 2) + 1) ((!a_idx lsl 32) + !b_idx);
        Array.set a_arr !a_idx (Array.length a);
        Array.set b_arr !b_idx (Array.length b);
      Array.blit ~src:a ~src_pos:0 ~dst:a_arr ~dst_pos:(!a_idx + 1) ~len:(Array.length a);
      Array.blit ~src:b ~src_pos:0 ~dst:b_arr ~dst_pos:(!b_idx + 1) ~len:(Array.length b);
        a_idx := !a_idx + 1 + Array.length a;
        b_idx := !b_idx + 1 + Array.length b;
        ());
    assert (!a_idx = a_size);
    assert (!b_idx = b_size);
    info, a_arr, b_arr
  ;;
end
