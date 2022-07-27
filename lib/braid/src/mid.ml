open! Core

module Node = struct
  module Id = struct
    type t = int [@@deriving sexp_of]

    let init = 0
    let next a = a + 1
  end

  type 'a t =
    { name : string option
    ; sexp_of : ('a -> Sexp.t) option
    ; type_id : 'a Type_equal.Id.t
    ; compute : ops -> unit -> 'a
    ; id : Id.t
    }

  and ops =
    | T :
        { eval : 'a. 'a t -> 'env -> 'a
        ; incr_refcount : 'a. 'a t -> 'env -> unit
        ; decr_refcount : 'a. 'a t -> 'env -> unit
        ; mark_dirty : 'a. 'a t -> 'env -> unit
        ; env : 'env
        }
        -> ops
  [@@deriving sexp_of]

  module Packed = struct
    type 'a node = 'a t

    module T = struct
      type t = T : 'a node -> t

      let sexp_of_t (T t) = sexp_of_t sexp_of_opaque t

      let compare (T a) (T b) =
        let r = compare_int a.id b.id in
        if r = 0 then assert (Type_equal.Id.same a.type_id b.type_id);
        r
      ;;
    end

    include Comparable.Make_plain (T)
    include T
  end

  let create ?name ?sexp_of id ~compute =
    let type_id =
      let name = Option.value name ~default:"" in
      let sexp_of = Option.value sexp_of ~default:sexp_of_opaque in
      Type_equal.Id.create ~name sexp_of
    in
    { name; sexp_of; type_id; id; compute }
  ;;
end

type ops = Node.ops =
  | T :
      { eval : 'a. 'a Node.t -> 'env -> 'a
      ; incr_refcount : 'a. 'a Node.t -> 'env -> unit
      ; decr_refcount : 'a. 'a Node.t -> 'env -> unit
      ; mark_dirty : 'a. 'a Node.t -> 'env -> unit
      ; env : 'env
      }
      -> ops

type t =
  { cur_id : int
  ; nodes : Node.Packed.Set.t
  ; depends_on : Node.Packed.Set.t Node.Packed.Map.t
  ; depended_on_by : Node.Packed.Set.t Node.Packed.Map.t
  ; priority : Node.Packed.Set.t
  }

let empty =
  { cur_id = 0
  ; nodes = Node.Packed.Set.empty
  ; depends_on = Node.Packed.Map.empty
  ; depended_on_by = Node.Packed.Map.empty
  ; priority = Node.Packed.Set.empty
  }
;;

let add ?name ?sexp_of ?(priority = false) t ~depends_on ~compute =
  let id = t.cur_id in
  let node = Node.create ?name ?sexp_of id ~compute in
  let nodes = Set.add t.nodes (T node) in
  let depended_on_by =
    Set.fold depends_on ~init:t.depended_on_by ~f:(fun acc dep ->
        Map.update acc dep ~f:(function
            | None -> Node.Packed.Set.singleton (T node)
            | Some set -> Set.add set (T node)))
  in
  let depends_on = Map.add_exn t.depends_on (T node) depends_on in
  let p = if priority then Set.add t.priority (T node) else t.priority in
  { cur_id = id + 1; nodes; depends_on; depended_on_by; priority = p }, node
;;

let compute_distance_from_priority_nodes { nodes; depends_on; priority; _ } =
  let rec traverse ~map ~node ~distance =
    match Map.find map node with
    | Some d' when d' <= distance -> map
    | Some _ | None ->
      let map = Map.set map node distance in
      Map.find depends_on node
      |> Option.value ~default:Node.Packed.Set.empty
      |> Set.fold ~init:map ~f:(fun map node ->
             traverse ~map ~node ~distance:(distance + 1))
  in
  let distance_map = Map.of_key_set nodes ~f:(fun _ -> Int.max_value) in
  Set.fold priority ~init:distance_map ~f:(fun map node ->
      traverse ~map ~node ~distance:0)
;;
