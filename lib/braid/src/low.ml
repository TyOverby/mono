open! Core
include Safety

module Node0 : sig
  type 'a t = private int
  type packed = T : 'a t -> packed [@@unboxed]

  val of_int : int -> 'a t
end = struct
  type 'a t = int
  type packed = T : 'a t -> packed [@@unboxed]

  let of_int = Fn.id
end

type t =
  { values : Obj.t Option_array.t
  ; cutoffs : Obj.t Option_array.t
  ; sexp_ofs : Obj.t Option_array.t
  ; names : string Option_array.t
  ; computes : Obj.t Option_array.t
  ; refcount : int Array.t
  ; dirty : bool Array.t
  ; depends_on : Node0.packed Array.t Array.t
  ; depended_on_by : Node0.packed Array.t Array.t
  ; length : int
  ; mutable next_id : int
  }

let create ~length =
  { values = Option_array.init length (fun _ -> None)
  ; cutoffs = Option_array.init length (fun _ -> None)
  ; sexp_ofs = Option_array.init length (fun _ -> None)
  ; names = Option_array.init length (fun _ -> None)
  ; computes = Option_array.init length (fun _ -> None)
  ; refcount = Array.create ~len:length 0
  ; dirty = Array.create ~len:length true
  ; depends_on = Array.create ~len:length Array.empty
  ; depended_on_by = Array.create ~len:length Array.empty
  ; length
  ; next_id = 0
  }
;;

let next_id t =
  let to_return = t.next_id in
  assert (to_return < t.length);
  t.next_id <- to_return + 1;
  Node0.of_int to_return
;;

let prepare
    (type a)
    env
    ?(sexp_of : (a -> Sexp.t) option)
    ?name
    ~(compute : unit -> a)
    ~(depends_on : Node0.packed array)
    ~(depended_on_by : Node0.packed array)
    (id : a Node0.t)
  =
  let id = (id :> int) in
  Option_array.set_some env.computes id (Obj.repr compute);
  Array.set env.depends_on id (Array.of_array depends_on);
  Array.set env.depended_on_by id (Array.of_array depended_on_by);
  Option_array.set env.sexp_ofs id (Option.map sexp_of ~f:Obj.magic);
  Option_array.set env.names id name
;;

let sexp_of_t t =
  let sexps = Array.init t.length (fun _ -> Sexp.Atom "") in
  for i = 0 to t.length - 1 do
    let name =
      match Option_array.get t.names i with
      | None -> sprintf "#%d:" i
      | Some name -> sprintf "%s#%d:" name i
    in
    let value =
      match Option_array.get t.values i, Option_array.get t.sexp_ofs i with
      | Some v, Some sexp_of -> Obj.magic sexp_of v
      | None, _ -> Sexp.Atom "<empty>"
      | Some _, None -> Sexp.Atom "<filled>"
    in
    let dirty =
      match Array.get t.dirty i with
      | true -> Sexp.Atom "dirty"
      | false -> Sexp.Atom "clean"
    in
    let refcount = Array.get t.refcount i in
    let sexp = [%message name ~_:(value : Sexp.t) ~_:(dirty : Sexp.t) (refcount : int)] in
    Array.set sexps i sexp
  done;
  [%sexp_of: Sexp.t Array.t] sexps
;;

let debug t =
  let module T = struct
    type t =
      { i : int
      ; name : string
      ; value : string
      ; dirty : bool
      ; refcount : int
      }
  end
  in
  let open T in
  let i_column =
    Ascii_table_kernel.Column.create ~align:Right "#" (fun { i; _ } -> Int.to_string i)
  in
  let name_column =
    Ascii_table_kernel.Column.create ~align:Left "@" (fun { name; _ } -> name)
  in
  let value_column =
    Ascii_table_kernel.Column.create ~align:Left "V" (fun { value; _ } -> value)
  in
  let clean_column =
    Ascii_table_kernel.Column.create "?" (fun { dirty; _ } -> if dirty then "x" else "-")
  in
  let refcount_column =
    Ascii_table_kernel.Column.create ~align:Right "R" (fun { refcount; _ } ->
        Int.to_string refcount)
  in
  let ts =
    List.init t.length (fun i ->
        let name =
          match Option_array.get t.names i with
          | None -> ""
          | Some name -> name
        in
        let value =
          match Option_array.get t.values i, Option_array.get t.sexp_ofs i with
          | Some v, Some sexp_of -> Obj.magic sexp_of v |> Sexp.to_string_hum
          | None, _ -> "<empty>"
          | Some _, None -> "<filled>"
        in
        let dirty = Array.get t.dirty i in
        let refcount = Array.get t.refcount i in
        { i; name; value; dirty; refcount })
  in
  match
    Ascii_table_kernel.draw
      ~prefer_split_on_spaces:false
      [ i_column; name_column; value_column; clean_column; refcount_column ]
      ts
  with
  | None -> "couldn't render table"
  | Some screen ->
    Ascii_table_kernel.Screen.to_string
      screen
      ~bars:`Unicode
      ~string_with_attr:(fun _ s -> s)
;;

module Node = struct
  include Node0

  let rec incr_refcount : type a. _ -> a t -> unit =
   fun env (id : a t) : unit ->
    let id = (id :> int) in
    let prev_refcount = Array.get env.refcount id in
    Array.set env.refcount id (prev_refcount + 1);
    if prev_refcount = 0
    then (
      let depends_on = Array.get env.depends_on id in
      for i = 0 to Array.length depends_on - 1 do
        let (T id) = Array.get depends_on i in
        incr_refcount env id
      done)
 ;;

  let rec decr_refcount : type a. _ -> a t -> unit =
   fun env (id : a t) : unit ->
    let id = (id :> int) in
    let prev_refcount = Array.get env.refcount id in
    Array.set env.refcount id (prev_refcount - 1);
    if prev_refcount = 1
    then (
      let depends_on = Array.get env.depends_on id in
      for i = 0 to Array.length depends_on - 1 do
        let (T id) = Array.get depends_on i in
        decr_refcount env id
      done)
 ;;

  let is_dirty (type a) env (id : a t) : bool =
    let id = (id :> int) in
    Array.get env.dirty id
  ;;

  let mark_dirty (type a) env (id : a t) : unit =
    let id = (id :> int) in
    Array.set env.dirty id true
  ;;

  let has_value (type a) env (id : a t) : bool =
    let id = (id :> int) in
    Option_array.is_some env.values id
  ;;

  let read_value (type a) env (id : a t) : a =
    let id = (id :> int) in
    Option_array.get_some env.values id |> (Obj.magic : Obj.t -> a)
  ;;

  let write_value (type a) env (id : a t) (new_ : a) : unit =
    let id = (id :> int) in
    let prop =
      if Option_array.is_some env.values id
      then (
        let cutoff =
          if Option_array.is_some env.cutoffs id
          then
            Option_array.get_some env.cutoffs id |> (Obj.magic : Obj.t -> a -> a -> bool)
          else phys_equal
        in
        let old = Option_array.get_some env.values id |> (Obj.magic : Obj.t -> a) in
        if cutoff old new_
        then false
        else (
          Option_array.set_some env.values id (Obj.repr new_);
          true))
      else (
        Option_array.set_some env.values id (Obj.repr new_);
        false)
    in
    if prop
    then (
      let depends_on_me = Array.get env.depended_on_by id in
      for i = 0 to Array.length depends_on_me - 1 do
        let (T id) = Array.get depends_on_me i in
        Array.set env.dirty (id :> int) true
      done)
    [@@inline always]
  ;;

  let recompute (type a) env (id : a t) : unit =
    let id_i = (id :> int) in
    let compute =
      (Obj.magic : Obj.t -> unit -> a) (Option_array.get_some env.computes id_i)
    in
    write_value env id (compute ())
    [@@inline always]
  ;;
end

(* bits for info int 
     .--------------- refcount
     |   .----------- has_cutoff
     |   | .--------- ever_computed
     |   | | .------- dirty
     v   v v v
  |.....| | | | 
 *)

let stabilize env =
  for i = 0 to env.length - 1 do
    if Array.get env.dirty i && Array.get env.refcount i <> 0
    then (
      Array.set env.dirty i false;
      Node.recompute env (Node.of_int i))
  done
;;
