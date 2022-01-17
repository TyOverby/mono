module Ucharr = Uchar
open Core
open! Bonsai_term
open! Notty
open! Notty.Infix
open Bonsai.Let_syntax
module N = Bignum
module Id = N

module Model = struct
  type 'a t =
    { children : 'a N.Map.t
    ; focus : N.t option
    }
  [@@deriving sexp, equal]
end

open Model

module Action = struct
  type 'a t =
    | Set_focus of N.t option
    | Set_children of 'a list
    | Insert_underneath of N.t * 'a
  [@@deriving sexp, equal]
end

open Action

type maybe_do_thing =
  | Continue of unit Ui_effect.t
  | Consumed_by of unit Ui_effect.t

let meld a b =
  match a, b with
  | Continue a, Continue b -> Continue (Ui_effect.Many [ a; b ])
  | Continue a, Consumed_by b -> Consumed_by (Ui_effect.Many [ a; b ])
  | Consumed_by a, Continue b -> Continue (Ui_effect.Many [ a; b ])
  | Consumed_by _, Consumed_by _ -> assert false
;;

module Actions = struct
  type 'a t =
    { focus_up : maybe_do_thing
    ; focus_down : maybe_do_thing
    ; clear_focus : unit Ui_effect.t
    ; insert_under_cursor : 'a -> maybe_do_thing
    ; set : Id.t list -> 'a list -> maybe_do_thing
    }

  let always_continue =
    { focus_up = Continue Ui_effect.Ignore
    ; focus_down = Continue Ui_effect.Ignore
    ; clear_focus = Ui_effect.Ignore
    ; insert_under_cursor = (fun _ -> Continue Ui_effect.Ignore)
    ; set = (fun _ _ -> Continue Ui_effect.Ignore)
    }
  ;;

  let from_map f map = f (fun id -> Map.find_exn map id)
end

let apply_action ~inject:_ ~schedule_event:_ model action =
  match action with
  | Set_focus focus -> { model with focus }
  | Set_children children ->
    let children =
      children |> List.mapi ~f:(fun i c -> N.of_int i, c) |> N.Map.of_alist_exn
    in
    { children; focus = None }
  | Insert_underneath (target, to_insert) ->
    let new_id =
      if Map.is_empty model.children
      then N.zero
      else (
        match Map.closest_key model.children `Greater_than target with
        | None -> N.(target + one)
        | Some (neighbor, _) -> N.((target + neighbor) / (one + one)))
    in
    let children = Map.set model.children ~key:new_id ~data:to_insert in
    { children; focus = Some new_id }
;;

let inject_focus_dir ~extrema ~direction ~field { children; focus } inject for_child =
  let clear_focus = inject (Set_focus None) in
  let set_focus a = inject (Set_focus (Some a)) in
  match focus with
  | None ->
    (match extrema children with
    | Some (focus, _) ->
      (match field (for_child focus) with
      | Consumed_by a -> Consumed_by (Ui_effect.Many [ a; set_focus focus ])
      | Continue _ as a -> meld a (Consumed_by (set_focus focus)))
    | None -> Continue clear_focus)
  | Some focus ->
    meld
      (Continue (for_child focus).Actions.clear_focus)
      (match field (for_child focus) with
      | Consumed_by a -> Consumed_by a
      | Continue _ as a ->
        (match Map.closest_key children direction focus with
        | None -> meld a (Continue clear_focus)
        | Some (focus, _) -> Consumed_by (inject (Set_focus (Some focus)))))
;;

let inject_focus_down model inject =
  inject_focus_dir
    ~extrema:Map.min_elt
    ~direction:`Greater_than
    model
    inject
    ~field:(fun { Actions.focus_down; _ } -> focus_down)
;;

let inject_focus_up model inject =
  inject_focus_dir
    ~extrema:Map.max_elt
    ~direction:`Less_than
    model
    inject
    ~field:(fun { Actions.focus_up; _ } -> focus_up)
;;

let component (type a) (module C : Bonsai.Model with type t = a)
    : (a Model.t * ((Id.t -> a Actions.t) -> a Actions.t)) Bonsai.Computation.t
  =
  let module M = struct
    type t = C.t Model.t [@@deriving sexp, equal]
  end
  in
  let module A = struct
    type t = C.t Action.t [@@deriving sexp, equal]
  end
  in
  let%sub sm =
    Bonsai.state_machine0
      [%here]
      (module M)
      (module A)
      ~default_model:{ children = N.Map.empty; focus = None }
      ~apply_action
  in
  let%arr model, inject = sm in
  let actions for_child =
    { Actions.focus_up = inject_focus_up model inject for_child
    ; focus_down = inject_focus_down model inject for_child
    ; clear_focus =
        (match model.focus with
        | None -> Ui_effect.Ignore
        | Some target ->
          Ui_effect.Many
            [ inject (Set_focus None); (for_child target).Actions.clear_focus ])
    ; insert_under_cursor =
        (fun content ->
          match model.focus with
          | None -> Continue Ui_effect.Ignore
          | Some target ->
            (match (for_child target).insert_under_cursor content with
            | Consumed_by _ as e -> e
            | Continue _ as e ->
              meld e (Consumed_by (inject (Insert_underneath (target, content))))))
    ; set =
        (fun path targets ->
          match path with
          | [] -> Consumed_by (inject (Set_children targets))
          | x :: xs -> (for_child x).Actions.set xs targets)
    }
  in
  model, actions
;;

let effect_of_maybe_do_thing = function
  | Consumed_by e -> e
  | Continue e -> e
;;
