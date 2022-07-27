open! Core
open Durse

module rec Sexp : sig
  type t = Buffer.t

  include Serializer.S with type t := t

  val create : int -> t
end = struct
  type t = Buffer.t

  let[@inline] write_int buf int = Buffer.add_string buf (Int.to_string int)

  let[@inline] write_bool buf = function
    | true -> Buffer.add_string buf "true"
    | false -> Buffer.add_string buf "false"
  ;;

  module Record = struct
    type nonrec t = t

    module Field = struct
      type nonrec t =
        { buf : t
        ; mutable first_written : bool
        }

      let[@inline] value (type a) t (module T : Serializable.S with type t = a) a =
        if t.first_written then Buffer.add_char t.buf ' ';
        t.first_written <- true;
        T.serialize (module Sexp) t.buf a
      ;;

      let[@inline] finish { buf; _ } = Buffer.add_char buf ')'
    end

    let[@inline] field buf name =
      Buffer.add_char buf '(';
      Buffer.add_string buf name;
      Buffer.add_char buf ' ';
      { Field.buf; first_written = false }
    ;;

    let[@inline] finish buf = Buffer.add_char buf ')'
  end

  let[@inline] record buf =
    Buffer.add_char buf '(';
    buf
  ;;

  let create = Buffer.create
end

module My_record = struct
  type t =
    { a : int
    ; b : bool
    ; c : int list [@sexp.list]
    }
  [@@deriving sexp]

  let[@inline] serialize (type ser) (module T : Serializer.S with type t = ser) ser t =
    let { a; b; c } = t in
    let record = T.record ser in
    let () =
      let field_a = T.Record.field record "a" in
      T.Record.Field.value field_a (module Int) a;
      T.Record.Field.finish field_a
    in
    let () =
      let field_b = T.Record.field record "b" in
      T.Record.Field.value field_b (module Bool) b;
      T.Record.Field.finish field_b
    in
    let () =
      let field_c = T.Record.field record "c" in
      List.iter c ~f:(fun i -> T.Record.Field.value field_c (module Int) i);
      T.Record.Field.finish field_c
    in
    T.Record.finish record
  ;;
end

let%expect_test "" =
  let t = { My_record.a = 5; b = true; c = [ 1; 2; 3 ] } in
  let buf = Sexp.create 20 in
  My_record.serialize (module Sexp) buf t;
  let buf = (buf :> Buffer.t) in
  print_endline (Buffer.contents buf);
  [%expect {| ((a 5)(b true)(c 1 2 3)) |}]
;;

let t = { My_record.a = 5; b = true; c = [ 1; 2; 3 ] }

let%bench "derse sexp" =
  let buf = Sexp.create 10 in
  My_record.serialize (module Sexp) buf t;
  let buf = (buf :> Buffer.t) in
  Buffer.contents buf
;;

let%bench "sexplib sexp" = t |> My_record.sexp_of_t |> Core.Sexp.to_string_mach
