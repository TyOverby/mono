open! Core

module F_bool : sig
  type t = T of float [@@deriving sexp_of]

  val of_float : float -> t
  val of_bool : bool -> t
  val to_float : t -> float
end = struct
  type t = T of float [@@deriving sexp_of]

  let sexp_of_t (T t) =
    match t with
    | 0.0 -> Sexp.Atom "false"
    | 1.0 -> Sexp.Atom "true"
    | other -> Float.sexp_of_t other
  ;;

  let of_float f = T (Float.clamp_exn f ~min:0.0 ~max:1.0)
  let to_float (T a) = a
  let of_bool b = T (if b then 1.0 else 0.0)
end

module Value = struct
  type 'a t =
    | Float : float -> float t
    | Bool : F_bool.t -> F_bool.t t
  [@@deriving sexp_of]
end

open Value

type 'a t =
  | Float_constant : float -> float t
  | Bool_constant : F_bool.t -> F_bool.t t
  | Add : float t * float t -> float t
  | If : F_bool.t t * 'a t * 'a t -> 'a t
  | Lt :
      { left : float t
      ; right : float t
      ; falloff : float t
      }
      -> F_bool.t t
  | Var : 'a Type_equal.Id.t -> 'a t
  | Sub :
      { bound : 'a t
      ; id : 'a Type_equal.Id.t
      ; body : 'b t
      }
      -> 'b t

type 'a missing_falloff = float t -> 'a t

let with_falloff t ~falloff = t falloff

module Env = Univ_map.Make (Univ_map.Type_id_key) (Value)

let float_const f = Float_constant f
let bool_const f = Bool_constant f
let ( + ) a b = Add (a, b)
let if_ c a b = If (c, a, b)
let ( < ) left right falloff = Lt { left; right; falloff }
let type_id () = Type_equal.Id.create ~name:"" sexp_of_opaque

let sub bound ~f =
  let id = type_id () in
  let body = f (Var id) in
  Sub { bound; id; body }
;;

let rec eval : type a. a t -> Env.t -> a Value.t =
 fun t env ->
  match t with
  | Float_constant f -> Float f
  | Bool_constant b -> Bool b
  | Add (a, b) ->
    let (Float a) = eval a env
    and (Float b) = eval b env in
    Float (a +. b)
  | If (cond, a, b) ->
    let (Bool cond) = eval cond env in
    let cond = F_bool.to_float cond in
    (match eval a env, eval b env with
    | Float a, Float b -> Float ((a *. cond) +. (b *. (1.0 -. cond)))
    | Bool a, Bool b ->
      let a = F_bool.to_float a
      and b = F_bool.to_float b in
      (a *. cond) +. (b *. (1.0 -. cond)) |> F_bool.of_float |> Bool)
  | Lt { left = a; right = b; falloff } ->
    let (Float a) = eval a env
    and (Float b) = eval b env in
    if Float.( < ) a b
    then Bool (F_bool.of_float 1.0)
    else (
      let (Float falloff) = eval falloff env in
      let how_off = a -. b in
      match Float.clamp ((-.how_off /. falloff) +. 1.0) ~min:0.0 ~max:1.0 with
      | Error _ -> Bool (F_bool.of_float 0.0)
      | Ok b -> Bool (F_bool.of_float b))
  | Var a -> Env.find_exn env a
  | Sub { bound; id; body } ->
    let bound = eval bound env in
    let env = Env.set env ~key:id ~data:bound in
    eval body env
;;

let eval : type a. a t -> a =
 fun t ->
  match eval t Env.empty with
  | Bool b -> b
  | Float f -> f
;;

module Let_syntax = struct
  module Let_syntax = struct
    let return = Fn.id
    let sub ?here:_ = sub
    let map t ~f:_ = t

    let switch ~match_ ~branches ~with_ =
      assert (branches = 2);
      if_ match_ (with_ 0) (with_ 1)
    ;;
  end
end
