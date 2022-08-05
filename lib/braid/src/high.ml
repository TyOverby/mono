open! Core

module Name = struct
  type 'a t = 'a Type_equal.Id.t

  let create () = Type_equal.Id.create "" sexp_of_opaque
end

module Env =
  Univ_map.Make
    (Univ_map.Type_id_key)
    (struct
      type 'a t = 'a Mid.Node.t

      let sexp_of_t _ = sexp_of_opaque
    end)

module Value = struct
  type 'a t =
    | Constant of 'a
    | Exception of exn
    | Name of 'a Name.t
end

module Value_or_node = struct
  type 'a t =
    | Constant of 'a
    | Exception of exn
    | Node of 'a Mid.Node.t

  let to_value env = function
    | Constant c -> Value.Constant c, env
    | Exception exn -> Value.Exception exn, env
    | Node node ->
      let name = Name.create () in
      let env = Env.set env name node in
      Name name, env
  ;;
end

module Arr = struct
  type ('a, 'r) arr1 =
    { a : 'a Value.t
    ; f : 'a -> 'r
    }

  type ('a, 'b, 'r) arr2 =
    { a : 'a Value.t
    ; b : 'b Value.t
    ; f : 'a -> 'b -> 'r
    }

  type ('a, 'b, 'c, 'r) arr3 =
    { a : 'a Value.t
    ; b : 'b Value.t
    ; c : 'c Value.t
    ; f : 'a -> 'b -> 'c -> 'r
    }

  type ('a, 'b, 'c, 'd, 'r) arr4 =
    { a : 'a Value.t
    ; b : 'b Value.t
    ; c : 'c Value.t
    ; d : 'd Value.t
    ; f : 'a -> 'b -> 'c -> 'd -> 'r
    }

  type 'r t =
    | Arr1 : ('a, 'r) arr1 -> 'r t
    | Arr2 : ('a, 'b, 'r) arr2 -> 'r t
    | Arr3 : ('a, 'b, 'c, 'r) arr3 -> 'r t
    | Arr4 : ('a, 'b, 'c, 'd, 'r) arr4 -> 'r t

  let attempt f =
    try Value_or_node.Constant (f ()) with
    | exn -> Value_or_node.Exception exn
  ;;

  let rewrap (mid, node) = mid, Value_or_node.Node node

  let to_node ~env mid = function
    | Value.Name id -> mid, Env.find_exn env id
    | Value.Constant c -> Mid.const mid c
    | Value.Exception exn ->
      let mid, const = Mid.const mid () in
      Mid.map mid const ~f:(fun () -> raise exn)
  ;;

  let eval
      : type a.
        ?sexp_of:(a -> Sexp.t) -> Mid.t -> Env.t -> a t -> Mid.t * a Value_or_node.t
    =
   fun ?sexp_of mid env t ->
    match t with
    | Arr1 t ->
      (match t with
      | { a = Constant a; f } -> mid, attempt (fun () -> f a)
      | { a = Exception exn; _ } -> mid, Exception exn
      | { a = Name a; f } ->
        let n = Env.find_exn env a in
        rewrap (Mid.map ?sexp_of mid n ~f))
    | Arr2 t ->
      (match t with
      | { a = Constant a; b = Constant b; f } -> mid, attempt (fun () -> f a b)
      | { a = Exception exn; _ } -> mid, Exception exn
      | { a = _; b = Exception exn; _ } -> mid, Exception exn
      | { a = Constant a; b = Name b; f } ->
        let b = Env.find_exn env b in
        rewrap (Mid.map ?sexp_of mid b ~f:(fun b -> f a b))
      | { a = Name a; b = Constant b; f } ->
        let a = Env.find_exn env a in
        rewrap (Mid.map ?sexp_of mid a ~f:(fun a -> f a b))
      | { a = Name a; b = Name b; f } ->
        let a = Env.find_exn env a in
        let b = Env.find_exn env b in
        rewrap (Mid.map2 ?sexp_of mid a b ~f))
    | Arr3 t ->
      (match t with
      | { a = Constant a; b = Constant b; c = Constant c; f } ->
        mid, attempt (fun () -> f a b c)
      | { a = Exception exn; _ } -> mid, Exception exn
      | { a = _; b = Exception exn; _ } -> mid, Exception exn
      | { a = _; b = _; c = Exception exn; _ } -> mid, Exception exn
      | { a = Name a; b = Name b; c = Name c; f } ->
        let a = Env.find_exn env a in
        let b = Env.find_exn env b in
        let c = Env.find_exn env c in
        rewrap (Mid.map3 ?sexp_of mid a b c ~f)
      | { a = Constant a; b = Name b; c = Name c; f } ->
        let b = Env.find_exn env b in
        let c = Env.find_exn env c in
        rewrap (Mid.map2 ?sexp_of mid b c ~f:(fun b c -> f a b c))
      | { a = Name a; b = Constant b; c = Name c; f } ->
        let a = Env.find_exn env a in
        let c = Env.find_exn env c in
        rewrap (Mid.map2 ?sexp_of mid a c ~f:(fun a c -> f a b c))
      | { a = Name a; b = Name b; c = Constant c; f } ->
        let a = Env.find_exn env a in
        let b = Env.find_exn env b in
        rewrap (Mid.map2 ?sexp_of mid a b ~f:(fun a b -> f a b c))
      | { a = Name a; b = Constant b; c = Constant c; f } ->
        let a = Env.find_exn env a in
        rewrap (Mid.map ?sexp_of mid a ~f:(fun a -> f a b c))
      | { a = Constant a; b = Name b; c = Constant c; f } ->
        let b = Env.find_exn env b in
        rewrap (Mid.map ?sexp_of mid b ~f:(fun b -> f a b c))
      | { a = Constant a; b = Constant b; c = Name c; f } ->
        let c = Env.find_exn env c in
        rewrap (Mid.map ?sexp_of mid c ~f:(fun c -> f a b c)))
    | Arr4 t ->
      (match t with
      | { a = Constant a; b = Constant b; c = Constant c; d = Constant d; f } ->
        mid, attempt (fun () -> f a b c d)
      | { a = Exception exn; _ } -> mid, Exception exn
      | { a = _; b = Exception exn; _ } -> mid, Exception exn
      | { a = _; b = _; c = Exception exn; _ } -> mid, Exception exn
      | { a = _; b = _; c = _; d = Exception exn; _ } -> mid, Exception exn
      | { a; b; c; d; f } ->
        (* This is the point where ty gave up *)
        let mid, a = to_node ~env mid a in
        let mid, b = to_node ~env mid b in
        let mid, c = to_node ~env mid c in
        let mid, d = to_node ~env mid d in
        rewrap (Mid.map4 ?sexp_of mid a b c d ~f))
 ;;
end

type 'a t =
  | Return : 'a -> 'a t
  | Const : 'a -> 'a Value.t t
  | Actually_const : 'a -> 'a Value.t t
  | Arr : 'r Arr.t -> 'r Value.t t
  | Bind :
      { a : 'a t
      ; f : 'a -> 'b t
      }
      -> 'b t
  | If :
      { cond : bool Value.t
      ; then_ : 'a Value.t
      ; else_ : 'a Value.t
      }
      -> 'a Value.t t

let return a = Return a
let const a = Const a
let const_node a = Actually_const a
let arr1 a ~f = Arr (Arr1 { a; f })
let arr2 a b ~f = Arr (Arr2 { a; b; f })
let arr3 a b c ~f = Arr (Arr3 { a; b; c; f })
let arr4 a b c d ~f = Arr (Arr4 { a; b; c; d; f })
let bind a ~f = Bind { a; f }
let if_ cond ~then_ ~else_ = If { cond; then_; else_ }

module Let_syntax = struct
  module Let_syntax = struct
    let bind = bind
    let return = return
  end
end

let rec lower : type a. Mid.t -> Env.t -> a t -> Mid.t * Env.t * a =
 fun mid env t ->
  match t with
  | Return a -> mid, env, a
  | Const a -> mid, env, Constant a
  | Actually_const a ->
    let mid, node = Arr.to_node mid ~env (Constant a) in
    let name = Name.create () in
    let env = Env.set env name node in
    mid, env, Name name
  | Arr arr ->
    let mid, value_or_node = Arr.eval mid env arr in
    let value, env = Value_or_node.to_value env value_or_node in
    mid, env, value
  | If { cond; then_; else_ } ->
    (match cond with
    | Value.Exception _ as exn -> mid, env, exn
    | Value.Constant true -> mid, env, then_
    | Constant false -> mid, env, else_
    | cond ->
      let mid, cond = Arr.to_node ~env mid cond in
      let mid, then_ = Arr.to_node ~env mid then_ in
      let mid, else_ = Arr.to_node ~env mid else_ in
      let mid, res = Mid.if_ mid cond ~then_ ~else_ in
      let name = Name.create () in
      let env = Env.set env name res in
      mid, env, Name name)
  | Bind { a; f } ->
    let mid, env, r = lower mid env a in
    lower mid env (f r)
;;

let lower t = lower Mid.empty Env.empty t

module Expert = struct
  module Value_or_node = Value_or_node

  type lookup = { f : 'a. 'a Value.t -> 'a Value_or_node.t }

  let lower t =
    let mid, env, r = lower t in
    let lookup (type a) (v : a Value.t) : a Value_or_node.t =
      match v with
      | Constant c -> Constant c
      | Exception exn -> Exception exn
      | Name name -> Node (Env.find_exn env name)
    in
    mid, { f = lookup }, r
  ;;
end
