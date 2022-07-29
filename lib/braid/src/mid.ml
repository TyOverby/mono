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
    ; compute : ops -> me:'a t -> unit -> 'a
    ; id : Id.t
    }

  and ops =
    { value : 'a. 'a t -> unit -> 'a
    ; has_value : 'a. 'a t -> unit -> bool
    ; incr_refcount : 'a. 'a t -> unit -> unit
    ; decr_refcount : 'a. 'a t -> unit -> unit
    ; mark_dirty : 'a. 'a t -> unit -> unit
    }
  [@@deriving sexp_of]

  let sexp_of = function
    | { sexp_of = None; _ } -> fun _ -> Sexp.Atom "<empty>"
    | { sexp_of = Some sexp_of; _ } -> sexp_of
  ;;

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

type t =
  { cur_id : int
  ; nodes : Node.Packed.Set.t
  ; depends_on : Node.Packed.Set.t Node.Packed.Map.t
  ; depended_on_by : Node.Packed.Set.t Node.Packed.Map.t
  ; computes_after : Node.Packed.Set.t Node.Packed.Map.t
  ; computed_before : Node.Packed.Set.t Node.Packed.Map.t
  ; priority : Node.Packed.Set.t
  }

let empty =
  { cur_id = 0
  ; nodes = Node.Packed.Set.empty
  ; depends_on = Node.Packed.Map.empty
  ; depended_on_by = Node.Packed.Map.empty
  ; computes_after = Node.Packed.Map.empty
  ; computed_before = Node.Packed.Map.empty
  ; priority = Node.Packed.Set.empty
  }
;;

let add ?name ?sexp_of ?(priority = false) ?(computes_after = []) t ~depends_on ~compute =
  let id = t.cur_id in
  let node = Node.create ?name ?sexp_of id ~compute in
  let nodes = Set.add t.nodes (T node) in
  let depended_on_by =
    List.fold depends_on ~init:t.depended_on_by ~f:(fun acc dep ->
        Map.update acc dep ~f:(function
            | None -> Node.Packed.Set.singleton (T node)
            | Some set -> Set.add set (T node)))
  in
  let computed_before =
    List.fold computes_after ~init:t.computed_before ~f:(fun acc dep ->
        Map.update acc dep ~f:(function
            | None -> Node.Packed.Set.singleton (T node)
            | Some set -> Set.add set (T node)))
  in
  let depends_on =
    Map.add_exn t.depends_on (T node) (Node.Packed.Set.of_list depends_on)
  in
  let computes_after =
    Map.add_exn t.computes_after (T node) (Node.Packed.Set.of_list computes_after)
  in
  let p = if priority then Set.add t.priority (T node) else t.priority in
  let t =
    { cur_id = id + 1
    ; nodes
    ; depends_on
    ; depended_on_by
    ; computes_after
    ; computed_before
    ; priority = p
    }
  in
  t, node
;;

let compute_distance_from_priority_nodes { nodes; depends_on; priority; _ } =
  (* TODO: should computes_after be factored into the distance calculation?  *)
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

let toposort t ~nodes ~priorities =
  let q =
    let cmp =
      Comparable.lexicographic
        [ Comparable.lift [%compare: int] ~f:(Map.find_exn priorities)
        ; Comparable.lift [%compare: int] ~f:(fun (Node.Packed.T node) -> node.id)
        ]
    in
    Pairing_heap.create ~cmp ()
  in
  let seen = ref Node.Packed.Set.empty in
  let enqueue_all nodes =
    Set.iter nodes ~f:(fun node ->
        let depends_on =
          Set.union (Map.find_exn t.depends_on node) (Map.find_exn t.computes_after node)
        in
        if Set.is_empty (Set.diff depends_on !seen) then Pairing_heap.add q node)
  in
  enqueue_all nodes;
  let out = ref [] in
  let rec loop () =
    match Pairing_heap.pop q with
    | None -> ()
    | Some node ->
      out := node :: !out;
      seen := Set.add !seen node;
      [ Map.find t.depended_on_by node; Map.find t.computed_before node ]
      |> List.filter_opt
      |> Node.Packed.Set.union_list
      |> enqueue_all;
      loop ()
  in
  loop ();
  List.rev !out
;;

module Expert = struct
  type with_id =
    | T :
        { type_id : 'a Type_equal.Id.t
        ; low_id : 'a Low.Node.t
        }
        -> with_id

  type lookup = { f : 'a. 'a Node.t -> 'a Low.Node.t }

  type ops = Node.ops =
    { value : 'a. 'a Node.t -> unit -> 'a
    ; has_value : 'a. 'a Node.t -> unit -> bool
    ; incr_refcount : 'a. 'a Node.t -> unit -> unit
    ; decr_refcount : 'a. 'a Node.t -> unit -> unit
    ; mark_dirty : 'a. 'a Node.t -> unit -> unit
    }

  let add = add

  let lower t =
    let priorities = compute_distance_from_priority_nodes t in
    let in_order = toposort t ~nodes:t.nodes ~priorities in
    let low = Low.create ~length:t.cur_id in
    let id_mappings = ref Node.Packed.Map.empty in
    List.iter in_order ~f:(fun (Node.Packed.T node) ->
        let low_id = Low.next_id low in
        let data : with_id = T { type_id = node.type_id; low_id } in
        id_mappings := Map.add_exn !id_mappings ~key:(T node) ~data);
    let id_mappings = !id_mappings in
    let get : type a. a Node.t -> a Low.Node.t =
     fun node ->
      let (T { type_id; low_id }) = Map.find_exn id_mappings (T node) in
      let T = Type_equal.Id.same_witness_exn type_id node.type_id in
      low_id
    in
    let ops : ops =
      let value node =
        let low_id = get node in
        fun () -> Low.Node.read_value low low_id
      in
      let has_value node =
        let low_id = get node in
        fun () -> Low.Node.has_value low low_id
      in
      let incr_refcount node =
        let low_id = get node in
        fun () -> Low.Node.incr_refcount low low_id
      in
      let decr_refcount node =
        let low_id = get node in
        fun () -> Low.Node.decr_refcount low low_id
      in
      let mark_dirty node =
        let low_id = get node in
        fun () -> Low.Node.mark_dirty low low_id
      in
      { value; has_value; incr_refcount; decr_refcount; mark_dirty }
    in
    List.iter in_order ~f:(fun (Node.Packed.T node) ->
        let deps_to_array deps_map =
          Map.find deps_map (Node.Packed.T node)
          |> Option.value ~default:Node.Packed.Set.empty
          |> Set.to_list
          |> List.map ~f:(fun (Node.Packed.T node) -> Low.Node.T (get node))
          |> List.to_array
        in
        let depends_on = deps_to_array t.depends_on in
        let depended_on_by = deps_to_array t.depended_on_by in
        Low.prepare
          low
          ?name:node.name
          ?sexp_of:node.sexp_of
          ~compute:(node.compute ops ~me:node)
          ~depends_on
          ~depended_on_by
          (get node));
    low, { f = get }
  ;;
end

open Expert

let const ?name ?sexp_of t a =
  add t ?name ?sexp_of ~depends_on:[] ~compute:(fun _ ~me:_ () -> a)
;;

let map ?name ?sexp_of t a ~f =
  add t ?name ?sexp_of ~depends_on:[ T a ] ~compute:(fun { value; _ } ~me:_ ->
      let a_value = value a in
      fun () -> f (a_value ()))
;;

let map2 ?name ?sexp_of t a b ~f =
  add t ?name ?sexp_of ~depends_on:[ T a; T b ] ~compute:(fun { value; _ } ~me:_ ->
      let a_value = value a in
      let b_value = value b in
      fun () -> f (a_value ()) (b_value ()))
;;

let map3 ?name ?sexp_of t a b c ~f =
  add t ?name ?sexp_of ~depends_on:[ T a; T b; T c ] ~compute:(fun { value; _ } ~me:_ ->
      let a_value = value a in
      let b_value = value b in
      let c_value = value c in
      fun () -> f (a_value ()) (b_value ()) (c_value ()))
;;

let map4 ?name ?sexp_of t a b c d ~f =
  add
    t
    ?name
    ?sexp_of
    ~depends_on:[ T a; T b; T c; T d ]
    ~compute:(fun { value; _ } ~me:_ ->
      let a_value = value a in
      let b_value = value b in
      let c_value = value c in
      let d_value = value d in
      fun () -> f (a_value ()) (b_value ()) (c_value ()) (d_value ()))
;;
