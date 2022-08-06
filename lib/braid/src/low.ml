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

module Info = struct
  type t = int

  let of_int = Fn.id

  (* proposed (i32): 
     deps_array: 15 bits 
     refcount:   15 bits 
     has_value:   1 bit
     dirty:       1 bit
   *)

  (* proposed (b54): 
     deps_array: 20 bits 
     refcount:   20 bits 

     cutoff:      1 bit
     has_value:   1 bit
     dirty:       1 bit
   *)

  (* bits for info int 
    .---------------- refcount
    |  .------------- has_cutoff
    |  | .----------- value is int
    |  | | .--------- has value
    |  | | | .------- dirty
    v  v v v v
  |...| | | | | *)

  let int_to_bool : int -> bool = Obj.magic
  let bool_to_int : bool -> int = Obj.magic
  let lnot x = x lxor -1

  let to_string ?(verbose = false) t =
    let refcount = sprintf "%d" (t lsr 3) in
    let verboseness =
      match verbose with
      | false -> ""
      | true ->
        let padding = String.length refcount in
        let padding = String.init padding (Fn.const ' ') in
        sprintf
          {|
%s.--------------- refcount
%s| .------------- has_cutoff
%s| | .----------- [ free ]
%s| | | .--------- has value
%s| | | | .------- dirty
%sv v v v v
|}
          padding
          padding
          padding
          padding
          padding
          padding
    in
    sprintf
      "%s %s %d %d %d %d\n 0x%x"
      verboseness
      refcount
      ((t lsr 3) land 1)
      ((t lsr 2) land 1)
      ((t lsr 1) land 1)
      ((t lsr 0) land 1)
      t
  ;;

  (* getters *)
  let is_dirty t = int_to_bool ((t lsr 0) land 1) [@@inline always]
  let has_value t = int_to_bool ((t lsr 1) land 1) [@@inline always]
  let has_cutoff t = int_to_bool ((t lsr 3) land 1) [@@inline always]
  let refcount t = t lsr 4 [@@inline always]
  let is_referenced t = refcount t <> 0 [@@inline always]
  let isn't_referenced t = refcount t = 0 [@@inline always]
  let refcount_is_one t = refcount t = 1 [@@inline always]
  let has_value_and_cutoff t = t land 0b1010 = 0b1010

  (* setters *)
  let set_dirty t = t lor 1 [@@inline always]
  let set_clean t = t land lnot (1 lsl 0) [@@inline always]
  let set_has_value t = t lor (1 lsl 1) [@@inline always]
  let set_has_cutoff t = t lor (1 lsl 3) [@@inline always]
  let init = set_dirty 0
  let incr_refcount t = t + (1 lsl 4) [@@inline always]

  let decr_refcount t =
    (* debug assert refcount t <> 0 *)
    t - (1 lsl 4)
    [@@inline always]
  ;;
end

type int16_bigarray =
  ( int
  , Stdlib.Bigarray.int16_unsigned_elt
  , Stdlib.Bigarray.c_layout )
  Stdlib.Bigarray.Array0.t

type t =
  { values : Obj_array.t
  ; info : Info.t Array.t
  ; cutoffs : Obj_array.t
  ; sexp_ofs : Obj.t option Array.t
  ; names : string option Array.t
  ; computes : Obj_array.t
  ; depends_on : Node0.packed Array.t Array.t
  ; depended_on_by : Node0.packed Array.t Array.t
  ; length : int
  ; mutable next_id : int
  }

let create ~length =
  { values = Obj_array.init_empty length
  ; info = Array.create ~len:length Info.init
  ; cutoffs = Obj_array.init_empty length
  ; sexp_ofs = Array.init length (fun _ -> None)
  ; names = Array.init length ~f:(fun _ -> None)
  ; computes = Obj_array.init_empty length
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
  Obj_array.set_some env.computes id (Obj.repr compute);
  Array.set env.depends_on id (Array.of_array depends_on);
  Array.set env.depended_on_by id (Array.of_array depended_on_by);
  Array.set env.sexp_ofs id (Option.map sexp_of ~f:Obj.magic);
  Array.set env.names id name
;;

let sexp_of_t t =
  let sexps = Array.init t.length (fun _ -> Sexp.Atom "") in
  for i = 0 to t.length - 1 do
    let name =
      match Array.get t.names i with
      | None -> sprintf "#%d:" i
      | Some name -> sprintf "%s#%d:" name i
    in
    let info = Array.get t.info i in
    let value =
      match Info.has_value info with
      | false -> Sexp.Atom "<empty>"
      | true ->
        let value = Obj_array.get_some t.values i in
        (match Array.get t.sexp_ofs i with
        | Some sexp_of -> Obj.magic sexp_of value
        | None -> Sexp.Atom "<filled>")
    in
    let dirty =
      match Info.is_dirty info with
      | true -> Sexp.Atom "dirty"
      | false -> Sexp.Atom "clean"
    in
    let refcount = Info.refcount info in
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
          match Array.get t.names i with
          | None -> ""
          | Some name -> name
        in
        let info = Array.get t.info i in
        let value =
          match Info.has_value info with
          | false -> Sexp.Atom "<empty>"
          | true ->
            let value = Obj_array.get_some t.values i in
            (match Array.get t.sexp_ofs i with
            | Some sexp_of -> Obj.magic sexp_of value
            | None -> Sexp.Atom "<filled>")
        in
        let value = Sexp.to_string_hum value in
        let dirty, refcount = Info.(is_dirty info, refcount info) in
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
    let prev_info = Array.get env.info id in
    let new_info = Info.incr_refcount prev_info in
    Array.set env.info id new_info;
    if Info.isn't_referenced prev_info
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
    let prev_info = Array.get env.info id in
    let new_info = Info.decr_refcount prev_info in
    Array.set env.info id new_info;
    if Info.refcount_is_one prev_info
    then (
      let depends_on = Array.get env.depends_on id in
      for i = 0 to Array.length depends_on - 1 do
        let (T id) = Array.get depends_on i in
        decr_refcount env id
      done)
 ;;

  let is_dirty (type a) env (id : a t) : bool =
    let id = (id :> int) in
    let info = Array.get env.info id in
    Info.is_dirty info
  ;;

  let mark_dirty (type a) env (id : a t) : unit =
    let id = (id :> int) in
    let info = Array.get env.info id in
    let new_info = Info.set_dirty info in
    Array.set env.info id new_info
  ;;

  let has_value (type a) env (id : a t) : bool =
    let id = (id :> int) in
    let info = Array.get env.info id in
    Info.has_value info
  ;;

  let read_value (type a) env (id : a t) : a =
    let id = (id :> int) in
    Obj_array.get_some env.values id |> (Obj.magic : Obj.t -> a)
  ;;

  let propagate_dirty (type a) env (id : int) : unit =
    let depends_on_me = Array.get env.depended_on_by id in
    for i = 0 to Array.length depends_on_me - 1 do
      let (T id) = Array.get depends_on_me i in
      mark_dirty env id
    done;
    ()
  ;;

  let write_value_without_cutoff_or_propagating_dirtyness
      (type a)
      env
      values
      (id : a t)
      (new_ : a)
      : unit
    =
    let id = (id :> int) in
    Obj_array.set_some_assuming_currently_int values id (Obj.repr new_)
  ;;

  let write_value_with_cutoff (type a) env values (id : a t) (new_ : a) : unit =
    let id = (id :> int) in
    let cutoff =
      Obj_array.get_some env.cutoffs id |> (Obj.magic : Obj.t -> a -> a -> bool)
    in
    let old = Obj_array.get_some values id |> (Obj.magic : Obj.t -> a) in
    if cutoff old new_
    then ()
    else (
      Obj_array.set_some values id (Obj.repr new_);
      propagate_dirty env id)
  ;;

  let write_value_with_phys_equal (type a) env values (id : a t) (new_ : a) =
    let id = (id :> int) in
    let old = Obj_array.get_some values id |> (Obj.magic : Obj.t -> a) in
    if phys_equal old new_
    then ()
    else (
      Obj_array.set_some values id (Obj.repr new_);
      propagate_dirty env id)
    [@@inline always]
  ;;

  let write_value (type a) env values ~info (id : a t) (new_ : a) : unit =
    if Info.has_value_and_cutoff info
    then write_value_with_cutoff env values id new_
    else write_value_with_phys_equal env values id new_
    [@@inline always]
  ;;

  let recompute (type a) env values ~info (id : a t) =
    let id_i = (id :> int) in
    let compute =
      (Obj.magic : Obj.t -> unit -> a) (Obj_array.get_some env.computes id_i)
    in
    write_value env values ~info id (compute ())
    [@@inline always]
  ;;

  let write_value (type a) env (id : a t) (new_ : a) : unit =
    let id_i = (id :> int) in
    let info = Array.get env.info id_i in
    let new_info = info |> Info.set_clean |> Info.set_has_value in
    write_value env env.values ~info:new_info id new_;
    Array.set env.info id_i new_info
  ;;
end

let stabilize env =
  let values = env.values in
  for i = 0 to env.length - 1 do
    let info = Array.get env.info i in
    if Bool.Non_short_circuiting.(Info.is_dirty info && Info.is_referenced info)
    then (
      let new_info = info |> Info.set_clean |> Info.set_has_value in
      Node.recompute env values ~info:new_info (Node.of_int i);
      Array.set env.info i new_info)
  done
;;
