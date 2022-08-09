open! Core
(**)
module Priority = struct
  type kind = Switch [@@deriving compare, sexp_of]

  type t =
    | Reward of kind
    | Neutral of int
    | Punish of kind
  [@@deriving sexp_of]

  let multi_compare first ~fallback = if first = 0 then fallback else first

  let compare (a, a_iter, a_id) (b, b_iter, b_id) =
    let by_id = compare_int a_id b_id in
    let by_iter = compare_int a_iter b_iter in
    match a, b with
    | Reward ak, Reward bk ->
      let by_kind = compare_kind ak bk in
      by_kind |> multi_compare ~fallback:by_iter |> multi_compare ~fallback:by_id
    | Neutral ad, Neutral bd ->
      let by_dist = compare_int ad bd in
      -by_dist |> multi_compare ~fallback:by_iter |> multi_compare ~fallback:by_id
    | Punish ak, Punish bk ->
      let by_kind = compare_kind ak bk in
      -by_kind |> multi_compare ~fallback:by_iter |> multi_compare ~fallback:(-by_id)
    | Reward _, Neutral _ | Reward _, Punish _ -> -1
    | Punish _, Neutral _ | Punish _, Reward _ -> 1
    | Neutral _, Reward _ -> 1
    | Neutral _, Punish _ -> -1
  ;;

  let neutral = Neutral 0
  let reward kind = Reward kind
  let punish kind = Punish kind
end

module Node = struct
  module Id = struct
    type t = int [@@deriving sexp_of]
  end

  type 'a t =
    { name : string option
    ; sexp_of : ('a -> Sexp.t) option
    ; type_id : 'a Type_equal.Id.t
    ; compute : Low.t -> lookup -> me:'a t -> unit -> 'a
    ; id : Id.t
    }

  and lookup = { f : 'a. 'a t -> 'a Low.Node.t } [@@deriving sexp_of]

  let sexp_of = function
    | { sexp_of = None; _ } -> fun _ -> Sexp.Atom "<empty>"
    | { sexp_of = Some sexp_of; _ } -> sexp_of
  ;;

  module Packed = struct
    type 'a node = 'a t

    module T = struct
      type t = T : 'a node -> t [@@unpacked]

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
  ; priority : Node.Packed.Set.t
  ; priorities : Priority.t Node.Packed.Map.t
  }

let empty =
  { cur_id = 0
  ; nodes = Node.Packed.Set.empty
  ; depends_on = Node.Packed.Map.empty
  ; depended_on_by = Node.Packed.Map.empty
  ; priority = Node.Packed.Set.empty
  ; priorities = Node.Packed.Map.empty
  }
;;

let rec propagate_priorities (t : t) (T node : Node.Packed.t) ~priority =
  let continue, priorities =
    match Map.find t.priorities (T node) with
    | None -> true, Map.set t.priorities ~key:(T node) ~data:priority
    | Some p' when Priority.compare (priority, 0, 0) (p', 0, 0) < 1 ->
      true, Map.set t.priorities ~key:(T node) ~data:priority
    | Some _ -> false, t.priorities
  in
  let t = { t with priorities } in
  let continue, subsequent_priorities =
    match priority with
    | Priority.Reward _ -> continue, Priority.Neutral 0
    | Neutral i -> continue, Neutral (i + 1)
    | Punish _ -> false, Neutral 0
  in
  if continue
  then
    Map.find t.depends_on (T node)
    |> Option.value ~default:Node.Packed.Set.empty
    |> Set.fold ~init:t ~f:(fun t node ->
           propagate_priorities t node ~priority:subsequent_priorities)
  else t
;;

let add ?name ?sexp_of ?(priority = Priority.Neutral Int.max_value) t ~depends_on ~compute
  =
  let id = t.cur_id in
  let node = Node.create ?name ?sexp_of id ~compute in
  let nodes = Set.add t.nodes (T node) in
  let depended_on_by =
    List.fold depends_on ~init:t.depended_on_by ~f:(fun acc dep ->
        Map.update acc dep ~f:(function
            | None -> Node.Packed.Set.singleton (T node)
            | Some set -> Set.add set (T node)))
  in
  let depends_on =
    Map.add_exn t.depends_on ~key:(T node) ~data:(Node.Packed.Set.of_list depends_on)
  in
  let p =
    match priority with
    | Reward _ -> Set.add t.priority (T node)
    | _ -> t.priority
  in
  let t =
    { cur_id = id + 1
    ; nodes
    ; depends_on
    ; depended_on_by
    ; priorities = t.priorities
    ; priority = p
    }
  in
  let t = propagate_priorities t (T node) ~priority in
  t, node
;;

let toposort t ~nodes =
  let q =
    let cmp (Node.Packed.T n1, iter1) (Node.Packed.T n2, iter2) =
      let p1 = Map.find_exn t.priorities (T n1) in
      let p2 = Map.find_exn t.priorities (T n2) in
      let id1 = n1.id in
      let id2 = n2.id in
      Priority.compare (p1, iter1, id1) (p2, iter2, id2)
    in
    Pairing_heap.create ~cmp ()
  in
  let seen = ref Node.Packed.Set.empty in
  let iter = ref 0 in
  let enqueue_all nodes =
    Set.iter nodes ~f:(fun node ->
        let depends_on = Map.find_exn t.depends_on node in
        if Set.is_empty (Set.diff depends_on !seen) then Pairing_heap.add q (node, !iter))
  in
  enqueue_all nodes;
  let out = ref [] in
  let rec loop () =
    incr iter;
    match Pairing_heap.pop q with
    | None -> ()
    | Some (node, _iter) ->
      out := node :: !out;
      if Set.mem !seen node then raise_s [%message "loop detected!" [%here]];
      seen := Set.add !seen node;
      Map.find t.depended_on_by node
      |> Option.value ~default:Node.Packed.Set.empty
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

  type lookup = Node.lookup = { f : 'a. 'a Node.t -> 'a Low.Node.t }

  let add = add

  let lower t =
    (*let priorities = compute_distance_from_priority_nodes t in *)
    let in_order = toposort t ~nodes:t.nodes in
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
    let lookup = { f = get } in
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
          ~compute:(node.compute low lookup ~me:node)
          ~depends_on
          ~depended_on_by
          (get node));
    Low.finalize low;
    low, lookup
  ;;
end

open Expert

let const ?name ?sexp_of t a =
  add t ?name ?sexp_of ~depends_on:[] ~compute:(fun _ _ ~me:_ () -> a)
;;

let map ?name ?sexp_of t a ~f =
  add t ?name ?sexp_of ~depends_on:[ T a ] ~compute:(fun low lookup ~me:_ ->
      let a_id = lookup.f a in
      fun () ->
        let a_value = Low.Node.read_value low a_id in
        f a_value)
;;

let map2 ?name ?sexp_of t a b ~f =
  add t ?name ?sexp_of ~depends_on:[ T a; T b ] ~compute:(fun low lookup ~me:_ ->
      let a_id = lookup.f a in
      let b_id = lookup.f b in
      fun () ->
        let a_value = Low.Node.read_value low a_id in
        let b_value = Low.Node.read_value low b_id in
        f a_value b_value)
;;

let map3 ?name ?sexp_of t a b c ~f =
  add t ?name ?sexp_of ~depends_on:[ T a; T b; T c ] ~compute:(fun low lookup ~me:_ ->
      let a_id = lookup.f a in
      let b_id = lookup.f b in
      let c_id = lookup.f c in
      fun () ->
        let a_value = Low.Node.read_value low a_id in
        let b_value = Low.Node.read_value low b_id in
        let c_value = Low.Node.read_value low c_id in
        f a_value b_value c_value)
;;

let map4 ?name ?sexp_of t a b c d ~f =
  add
    t
    ?name
    ?sexp_of
    ~depends_on:[ T a; T b; T c; T d ]
    ~compute:(fun low lookup ~me:_ ->
      let a_id = lookup.f a in
      let b_id = lookup.f b in
      let c_id = lookup.f c in
      let d_id = lookup.f d in
      fun () ->
        let a_value = Low.Node.read_value low a_id in
        let b_value = Low.Node.read_value low b_id in
        let c_value = Low.Node.read_value low c_id in
        let d_value = Low.Node.read_value low d_id in
        f a_value b_value c_value d_value)
;;

let if_ ?sexp_of t cond ~then_:a ~else_:b =
  let sexp_of_out =
    Option.first_some sexp_of (Option.first_some a.Node.sexp_of b.Node.sexp_of)
  in
  let rec t__in__out =
    lazy
      (let mid, if_in =
         add
           t
           ~name:"if-in"
           ~depends_on:[ T cond ]
           ~sexp_of:[%sexp_of: bool]
           ~priority:(Priority.reward Switch)
           ~compute:(fun low lookup ~me ->
             let a_id = lookup.f a in
             let b_id = lookup.f b in
             let my_id = lookup.f me in
             let cond_id = lookup.f cond in
             let (lazy (_, _, switch_out)) = t__in__out in
             let out_id = lookup.f switch_out in
             fun () ->
               let prev_valid, prev =
                 if Low.Node.has_value low my_id
                 then true, Low.Node.read_value low my_id
                 else false, true
               in
               let next = Low.Node.read_value low cond_id in
               let should_dirty =
                 match prev_valid, prev, next with
                 | false, _, true ->
                   Low.Node.incr_refcount low a_id;
                   false
                 | false, _, false ->
                   Low.Node.incr_refcount low b_id;
                   false
                 | true, true, true -> false
                 | true, false, false -> false
                 | true, true, false ->
                   Low.Node.decr_refcount low a_id;
                   Low.Node.incr_refcount low b_id;
                   true
                 | true, false, true ->
                   Low.Node.decr_refcount low b_id;
                   Low.Node.incr_refcount low a_id;
                   true
               in
               if should_dirty then Low.Node.mark_dirty low out_id;
               next)
       in
       let mid, if_out =
         add
           mid
           ~name:"if-out"
           ~depends_on:[ T if_in ]
           ?sexp_of:sexp_of_out
           ~priority:(Priority.punish Switch)
           ~compute:(fun low lookup ~me:_ ->
             let a_id = lookup.f a in
             let b_id = lookup.f b in
             let in_id = lookup.f if_in in
             fun () ->
               match Low.Node.read_value low in_id with
               | true -> Low.Node.read_value low a_id
               | false -> Low.Node.read_value low b_id)
       in
       mid, if_in, if_out)
  in
  let (lazy (t, _if_in, if_out)) = t__in__out in
  t, if_out
;;
