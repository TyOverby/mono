open! Core

module Value = struct
  type 'a t =
    | Constant of 'a
    | Exception of exn
    | Node of 'a Mid.Node.t
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
    try Value.Constant (f ()) with
    | exn -> Value.Exception exn
  ;;

  let rewrap (mid, node) = mid, Value.Node node

  let to_node mid = function
    | Value.Node node -> mid, node
    | Value.Constant c -> Mid.const mid c
    | Value.Exception exn ->
      let mid, const = Mid.const mid () in
      Mid.map mid const ~f:(fun () -> raise exn)
  ;;

  let eval : type a. Mid.t -> a t -> Mid.t * a Value.t =
   fun mid t ->
    match t with
    | Arr1 t ->
      (match t with
      | { a = Constant a; f } -> mid, attempt (fun () -> f a)
      | { a = Exception _ as exn; _ } -> mid, exn
      | { a = Node n; f } -> rewrap (Mid.map mid n ~f))
    | Arr2 t ->
      (match t with
      | { a = Constant a; b = Constant b; f } -> mid, attempt (fun () -> f a b)
      | { a = Exception _ as exn; _ } -> mid, exn
      | { a = _; b = Exception _ as exn; _ } -> mid, exn
      | { a = Constant a; b = Node b; f } -> rewrap (Mid.map mid b ~f:(fun b -> f a b))
      | { a = Node a; b = Constant b; f } -> rewrap (Mid.map mid a ~f:(fun a -> f a b))
      | { a = Node a; b = Node b; f } -> rewrap (Mid.map2 mid a b ~f))
    | Arr3 t ->
      (match t with
      | { a = Constant a; b = Constant b; c = Constant c; f } ->
        mid, attempt (fun () -> f a b c)
      | { a = Exception _ as exn; _ } -> mid, exn
      | { a = _; b = Exception _ as exn; _ } -> mid, exn
      | { a = _; b = _; c = Exception _ as exn; _ } -> mid, exn
      | { a = Node a; b = Node b; c = Node c; f } -> rewrap (Mid.map3 mid a b c ~f)
      | { a = Constant a; b = Node b; c = Node c; f } ->
        rewrap (Mid.map2 mid b c ~f:(fun b c -> f a b c))
      | { a = Node a; b = Constant b; c = Node c; f } ->
        rewrap (Mid.map2 mid a c ~f:(fun a c -> f a b c))
      | { a = Node a; b = Node b; c = Constant c; f } ->
        rewrap (Mid.map2 mid a b ~f:(fun a b -> f a b c))
      | { a = Node a; b = Constant b; c = Constant c; f } ->
        rewrap (Mid.map mid a ~f:(fun a -> f a b c))
      | { a = Constant a; b = Node b; c = Constant c; f } ->
        rewrap (Mid.map mid b ~f:(fun b -> f a b c))
      | { a = Constant a; b = Constant b; c = Node c; f } ->
        rewrap (Mid.map mid c ~f:(fun c -> f a b c)))
    | Arr4 t ->
      (match t with
      | { a = Constant a; b = Constant b; c = Constant c; d = Constant d; f } ->
        mid, attempt (fun () -> f a b c d)
      | { a = Exception _ as exn; _ } -> mid, exn
      | { a = _; b = Exception _ as exn; _ } -> mid, exn
      | { a = _; b = _; c = Exception _ as exn; _ } -> mid, exn
      | { a = _; b = _; c = _; d = Exception _ as exn; _ } -> mid, exn
      | { a; b; c; d; f } ->
        (* This is the point where ty gave up *)
        let mid, a = to_node mid a in
        let mid, b = to_node mid b in
        let mid, c = to_node mid c in
        let mid, d = to_node mid d in
        rewrap (Mid.map4 mid a b c d ~f))
 ;;
end

type 'a t =
  | Return : 'a -> 'a t
  | Const : 'a -> 'a Value.t t
  | Arr : 'r Arr.t -> 'r Value.t t
  | Bind :
      { a : 'a t
      ; f : 'a -> 'b t
      }
      -> 'b t

let return a = Return a
let const a = Const a
let arr1 a ~f = Arr (Arr1 { a; f })
let arr2 a b ~f = Arr (Arr2 { a; b; f })
let arr3 a b c ~f = Arr (Arr3 { a; b; c; f })
let arr4 a b c d ~f = Arr (Arr4 { a; b; c; d; f })
let bind a ~f = Bind { a; f }

let rec lower : type a. Mid.t -> a t -> Mid.t * a =
 fun mid t ->
  match t with
  | Return a -> mid, a
  | Const a ->
    let mid, a = Mid.const mid a in
    mid, Value.Node a
  | Arr arr -> Arr.eval mid arr
  | Bind { a; f } ->
    let mid, r = lower mid a in
    lower mid (f r)
;;

let lower t = lower Mid.empty t
