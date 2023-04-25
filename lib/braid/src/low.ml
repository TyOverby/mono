open! Core
include Safety

module Node0 : sig
  type 'a t = private int
  type packed = T : 'a t -> packed [@@unboxed]

  val of_int : int -> 'a t
  val to_int : _ t -> int
end = struct
  type 'a t = int
  type packed = T : 'a t -> packed [@@unboxed]

  let of_int = Fn.id
  let to_int = Fn.id
end

module Info = struct
  type t = int

  let of_int = Fn.id

  (* proposed (i32): 
     deps_array: 15 bits 
     refcount:   15 bits 
     has_value:   1 bit
     dirty:       1 bit
     :

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
  let lnot x = x lxor -1

  let to_string ?(verbose = false) t =
    let refcount = sprintf "%d" (t lsr 3) in
    let verboseness =
      match verbose with
      | false -> ""
      | true ->
        let padding = String.length refcount in
        let padding = String.init padding ~f:(Fn.const ' ') in
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
  let _has_value_and_cutoff t = t land 0b1010 = 0b1010

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

type t =
  { values : Obj_array.t
  ; mutable info_arr : Info_arr.info
  ; cutoffs : Obj_array.t
  ; sexp_ofs : Obj.t option Array.t
  ; names : string option Array.t
  ; computes : Obj_array.t
  ; mutable depends_on : Info_arr.depends_on
  ; mutable depended_on : Info_arr.depended_on_by
  ; length : int
  ; mutable next_id : int
  ; mutable builder : Info_arr.Builder.t option
  }

let create ~length =
  let info_arr, depends_on, depended_on =
    Info_arr.Builder.(finalize (create Info.init))
  in
  let builder = Some (Info_arr.Builder.create Info.init) in
  { values = Obj_array.init_empty length
  ; info_arr
  ; cutoffs = Obj_array.init_empty length
  ; sexp_ofs = Array.init length ~f:(fun _ -> None)
  ; names = Array.init length ~f:(fun _ -> None)
  ; computes = Obj_array.init_empty length
  ; depends_on
  ; depended_on
  ; length
  ; builder
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
  (match env.builder with
  | None -> failwith "Low.prepare called after finalization"
  | Some b ->
    let depends_on = Core.Array.map depends_on ~f:(fun (Node0.T t) -> Node0.to_int t) in
    let depended_on_by =
      Core.Array.map depended_on_by ~f:(fun (Node0.T t) -> Node0.to_int t)
    in
    Info_arr.Builder.add b (id :> int) ~depends_on ~depended_on_by);
  Obj_array.set_some env.computes id (Obj.repr compute);
  Array.set env.sexp_ofs id (Option.map sexp_of ~f:Obj.magic);
  Array.set env.names id name
;;

let finalize t =
  let builder = Option.value_exn t.builder in
  let info_arr, depends_on, depended_on = Info_arr.Builder.finalize builder in
  t.builder <- None;
  t.info_arr <- info_arr;
  t.depends_on <- depends_on;
  t.depended_on <- depended_on
;;

let sexp_of_t t =
  let sexps = Array.init t.length ~f:(fun _ -> Sexp.Atom "") in
  for i = 0 to t.length - 1 do
    let name =
      match Array.get t.names i with
      | None -> sprintf "#%d:" i
      | Some name -> sprintf "%s#%d:" name i
    in
    let info = Info_arr.info t.info_arr i in
    let value =
      match Info.has_value info with
      | false -> Sexp.Atom "<empty>"
      | true ->
        let value = Obj_array.get_some t.values i in
        (match Array.get t.sexp_ofs i with
         | Some sexp_of ->
           let sexp_of : Obj.t -> Sexp.t = Obj.magic sexp_of in
           sexp_of value
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
    List.init t.length ~f:(fun i ->
        let name =
          match Array.get t.names i with
          | None -> ""
          | Some name -> name
        in
        let info = Info_arr.info t.info_arr i in
        let value =
          match Info.has_value info with
          | false -> Sexp.Atom "<empty>"
          | true ->
            let value = Obj_array.get_some t.values i in
            (match Array.get t.sexp_ofs i with
           | Some sexp_of ->
             let sexp_of : Obj.t -> Sexp.t = Obj.magic sexp_of in
             sexp_of value
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

  let[@inline always] rec incr_refcount : type a. _ -> a t -> unit =
   fun env (id : a t) : unit ->
    let id = (id :> int) in
    let prev_info = Info_arr.info env.info_arr id in
    let new_info = Info.incr_refcount prev_info in
    Info_arr.set_info env.info_arr id new_info;
    if Info.isn't_referenced prev_info
    then (
      let idx = Info_arr.depends_on_idx env.info_arr id in
      let len = Info_arr.depends_on_length env.depends_on idx in
      for i = 0 to len - 1 do
        let id = Info_arr.get_depends_on env.depends_on idx i in
        incr_refcount env (Node0.of_int id)
      done)
 ;;

  let[@inline always] rec decr_refcount : type a. _ -> a t -> unit =
   fun env (id : a t) : unit ->
    let id = (id :> int) in
    let prev_info = Info_arr.info env.info_arr id in
    let new_info = Info.decr_refcount prev_info in
    Info_arr.set_info env.info_arr id new_info;
    if Info.refcount_is_one prev_info
    then (
      let idx = Info_arr.depends_on_idx env.info_arr id in
      let len = Info_arr.depends_on_length env.depends_on idx in
      for i = 0 to len - 1 do
        let id = Info_arr.get_depends_on env.depends_on idx i in
        decr_refcount env (Node0.of_int id)
      done)
 ;;

  let[@inline always] is_dirty (type a) env (id : a t) : bool =
    let id = (id :> int) in
    let info = Info_arr.info env.info_arr id in
    Info.is_dirty info
  ;;

  let _mark_dirty (type a) env (id : a t) : unit =
    let id = (id :> int) in
    let info = Info_arr.info env.info_arr id in
    let new_info = Info.set_dirty info in
    Info_arr.set_info env.info_arr id new_info
  ;;

  let[@inline always] mark_dirty (type a) info_arr (id : a t) : unit =
    let id = (id :> int) in
    let info = Info_arr.info info_arr id in
    let new_info = Info.set_dirty info in
    Info_arr.set_info info_arr id new_info
  ;;

  let[@inline always] has_value (type a) env (id : a t) : bool =
    let id = (id :> int) in
    let info = Info_arr.info env.info_arr id in
    Info.has_value info
  ;;

  let[@inline always] read_value (type a) env (id : a t) : a =
    let id = (id :> int) in
    Obj_array.get_some env.values id |> (Obj.magic : Obj.t -> a)
  ;;

  let propagate_dirty' info_arr depended_on (id : int) : unit =
    let idx = Info_arr.depended_on_by_idx info_arr id in
    let len = Info_arr.depended_on_by_length depended_on idx in
    for i = 0 to len - 1 do
      let id = Info_arr.get_depended_on_by depended_on idx i in
      mark_dirty info_arr (Node0.of_int id)
    done;
    ()
  ;;

  let write_value_without_cutoff_or_propagating_dirtyness
      (type a)
      values
      (id : a t)
      (new_ : a)
    : unit
    =
    let id = (id :> int) in
    Obj_array.set_some_assuming_currently_int values id (Obj.repr new_)
  ;;

  let write_value_with_cutoff
      (type a)
      env
      values
      info_arr
      depended_on
      (id : a t)
      (new_ : a)
    : unit
    =
    let id = (id :> int) in
    let cutoff =
      Obj_array.get_some env.cutoffs id |> (Obj.magic : Obj.t -> a -> a -> bool)
    in
    let old = Obj_array.get_some values id |> (Obj.magic : Obj.t -> a) in
    if cutoff old new_
    then ()
    else (
      Obj_array.set_some values id (Obj.repr new_);
      propagate_dirty' info_arr depended_on id)
  ;;

  let write_value_with_phys_equal
      (type a)
      values
      info_arr
      depended_on
      (id : a t)
      (new_ : a)
    =
    let id = (id :> int) in
    let old = Obj_array.get_some values id |> (Obj.magic : Obj.t -> a) in
    if phys_equal old new_
    then ()
    else (
      Obj_array.set_some values id (Obj.repr new_);
      propagate_dirty' info_arr depended_on id)
  [@@inline always]
  ;;

  let write_value (type a) env values info_arr depended_on ~info (id : a t) (new_ : a)
    : unit
    =
    let has_value = Info.has_value info in
    let has_cutoff = Info.has_cutoff info in
    match has_value with
    | false -> write_value_without_cutoff_or_propagating_dirtyness values id new_
    | true ->
      (match has_cutoff with
      | true -> write_value_with_cutoff env values info_arr depended_on id new_
      | false -> write_value_with_phys_equal values info_arr depended_on id new_)
  [@@inline always]
  ;;

  let mark_dirty (type a) env (id : a t) : unit =
    let id = (id :> int) in
    let info = Info_arr.info env.info_arr id in
    let new_info = Info.set_dirty info in
    Info_arr.set_info env.info_arr id new_info
  ;;

  let recompute (type a) env values info_arr depended_on ~info (id : a t) =
    let id_i = (id :> int) in
    let compute =
      (Obj.magic : Obj.t -> unit -> a) (Obj_array.get_some env.computes id_i)
    in
    write_value env values info_arr depended_on ~info id (compute ())
  [@@inline always]
  ;;

  let write_value (type a) env (id : a t) (new_ : a) : unit =
    let id_i = (id :> int) in
    let info = Info_arr.info env.info_arr id_i in
    write_value env env.values env.info_arr env.depended_on ~info id new_;
    let new_info = info |> Info.set_clean |> Info.set_has_value in
    Info_arr.set_info env.info_arr id_i new_info
  ;;
end

let stabilize env =
  let values = env.values in
  let info_arr = env.info_arr in
  let depended_on = env.depended_on in
  for i = 0 to env.length - 1 do
    let info = Info_arr.info info_arr i in
    if Bool.Non_short_circuiting.(Info.is_dirty info && Info.is_referenced info)
    then (
      Node.recompute env values info_arr depended_on ~info (Node.of_int i);
      let new_info = info |> Info.set_clean |> Info.set_has_value in
      Info_arr.set_info info_arr i new_info)
  done
;;

let stabilize' env =
  let values = env.values in
  let info_arr = env.info_arr in
  let depended_on = env.depended_on in
  for i = 0 to env.length - 1 do
    let info = Info_arr.info info_arr i in
    let compute = (Obj.magic : Obj.t -> unit -> _) (Obj_array.get_some env.computes i) in
    let new_ = compute () in
    if Bool.Non_short_circuiting.(Info.is_dirty info && Info.is_referenced info)
    then (
      Node.write_value_with_phys_equal values info_arr depended_on (Node0.of_int i) new_;
      let new_info = info |> Info.set_clean |> Info.set_has_value in
      Info_arr.set_info info_arr i new_info)
  done
;;
