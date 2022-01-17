open! Core
open Bonsai.Let_syntax

let with_inject_fixed_point f =
  let%sub r, _ =
    Bonsai.wrap
      (module Unit)
      ~default_model:()
      ~apply_action:(fun ~inject:_ ~schedule_event (_result, inject) () action ->
        (* speedy thing go in, speedy thing come out *)
        schedule_event (inject action))
      ~f:(fun _model inject -> f inject)
  in
  return r
;;

let yoink a =
  let%sub _, result =
    Bonsai.actor1
      [%here]
      (module Unit)
      (module Unit)
      ~recv:(fun ~schedule_event:_ a () () -> (), a)
      ~default_model:()
      a
  in
  return
  @@ let%map result = result in
  result ()
;;

let scope_model
      (type a cmp)
      (module M : Bonsai.Comparator with type t = a and type comparator_witness = cmp)
      ~on:v
      computation
  =
  let v = Bonsai.Value.map v ~f:(fun k -> Map.singleton (module M) k ()) in
  let%sub map = Bonsai.assoc (module M) v ~f:(fun _ _ -> computation) in
  let%arr map = map in
  (* This _exn is ok because we know that the map is a singleton *)
  let _k, r = Map.max_elt_exn map in
  r
;;

let state_machine1_dynamic_model
      (type m a)
      here
      (module M : Bonsai.Model with type t = m)
      (module A : Bonsai.Action with type t = a)
      ~model
      ~apply_action
      input
  =
  let model_creator =
    match model with
    | `Given m ->
      Bonsai.Value.map m ~f:(fun m -> function
        | None -> m
        | Some a -> a)
    | `Computed f -> f
  in
  let module M_actual = struct
    type t = M.t option [@@deriving sexp, equal]
  end
  in
  let apply_action ~inject ~schedule_event (input, model_creator) model action =
    let model = model_creator model in
    Some (apply_action ~inject ~schedule_event input model action)
  in
  let%sub model_and_inject =
    Bonsai.state_machine1
      here
      (module M_actual)
      (module A)
      ~default_model:None
      ~apply_action
      (Bonsai.Value.both input model_creator)
  in
  return
  @@ let%map model, inject = model_and_inject
  and model_creator = model_creator in
  model_creator model, inject
;;

let state_machine0_dynamic_model here model_mod action_mod ~model ~apply_action =
  let apply_action ~inject ~schedule_event () model action =
    apply_action ~inject ~schedule_event model action
  in
  state_machine1_dynamic_model
    here
    model_mod
    action_mod
    ~model
    ~apply_action
    (Bonsai.Value.return ())
;;

let state_dynamic_model (type m) here (module M : Bonsai.Model with type t = m) ~model =
  let apply_action ~inject:_ ~schedule_event:_ _old_model new_model = new_model in
  state_machine0_dynamic_model here (module M) (module M) ~model ~apply_action
;;

let exactly_once here effect =
  let%sub has_run, set_has_run = Bonsai.state here (module Bool) ~default_model:false in
  if%sub has_run
  then Bonsai.const ()
  else
    Bonsai.Edge.lifecycle
      ~on_activate:
        (let%map set_has_run = set_has_run
         and event = effect in
         Ui_effect.Many [ set_has_run true; event ])
      ()
;;

let exactly_once_with_value here modul effect =
  let%sub value, set_value = Bonsai.state_opt here modul in
  let%sub () =
    match%sub value with
    | None ->
      Bonsai.Edge.lifecycle
        ~on_activate:
          (let%map set_value = set_value
           and effect = effect in
           let%bind.Bonsai.Effect r = effect in
           set_value (Some r))
        ()
    | Some _ -> Bonsai.const ()
  in
  return value
;;

let freeze here model value =
  let%sub state, set_state = Bonsai.state_opt here model in
  match%sub state with
  | Some state -> return state
  | None ->
    let%sub () =
      Bonsai.Edge.lifecycle
        ~on_activate:
          (let%map set_state = set_state
           and value = value in
           set_state (Some value))
        ()
    in
    return value
;;

let thunk (type a) (f : unit -> a) =
  let%sub out = return Bonsai.Value.(map (return ()) ~f) in
  freeze
    [%here]
    (module struct
      type t = (a[@sexp.opaque]) [@@deriving sexp]

      let equal = phys_equal
    end)
    out
;;

let toggle here ~default_model =
  let%sub state =
    Bonsai.state_machine0
      here
      (module Bool)
      (module Unit)
      ~apply_action:(fun ~inject:_ ~schedule_event:_ b () -> not b)
      ~default_model
  in
  let%arr state, inject = state in
  state, inject ()
;;

let pipe (type a) here (module A : Bonsai.Model with type t = a) =
  let module Model = struct
    type t =
      { queued_actions : A.t Fdeque.t
      ; queued_receivers : (unit, a) Bonsai.Effect.Private.Callback.t Fdeque.t
      }

    let equal = phys_equal
    let default = { queued_actions = Fdeque.empty; queued_receivers = Fdeque.empty }
    let sexp_of_t { queued_actions; _ } = [%sexp_of: A.t Fdeque.t] queued_actions

    let t_of_sexp sexp =
      let queued_actions = [%of_sexp: A.t Fdeque.t] sexp in
      { default with queued_actions }
    ;;
  end
  in
  let module Action = struct
    type t =
      | Add_action of a
      | Add_receiver of (unit, a) Bonsai.Effect.Private.Callback.t

    let sexp_of_t = function
      | Add_action a -> A.sexp_of_t a
      | Add_receiver r -> sexp_of_opaque r
    ;;
  end
  in
  let%sub _, inject =
    Bonsai.state_machine0
      here
      (module Model)
      (module Action)
      ~default_model:Model.default
      ~apply_action:
        (fun ~inject:_ ~schedule_event model -> function
           | Add_action a ->
             (match Fdeque.dequeue_front model.queued_receivers with
              | None ->
                let queued_actions = Fdeque.enqueue_back model.queued_actions a in
                { model with queued_actions }
              | Some (hd, queued_receivers) ->
                schedule_event (Bonsai.Effect.Private.Callback.respond_to hd a);
                { model with queued_receivers })
           | Add_receiver r ->
             (match Fdeque.dequeue_front model.queued_actions with
              | None ->
                let queued_receivers = Fdeque.enqueue_back model.queued_receivers r in
                { model with queued_receivers }
              | Some (hd, queued_actions) ->
                schedule_event (Bonsai.Effect.Private.Callback.respond_to r hd);
                { model with queued_actions }))
  in
  return
    (let%map inject = inject in
     let request =
       Bonsai.Effect.Private.make ~request:() ~evaluator:(fun r ->
         inject (Add_receiver r))
     in
     (fun a -> inject (Add_action a)), request)
;;

let map_of_set = Bonsai.Incr.compute ~f:Ui_incr.Map.of_set
let map_keys = Bonsai.Incr.compute ~f:Ui_incr.Map.keys

let map_merge a b ~f =
  Bonsai.Incr.compute (Bonsai.Value.both a b) ~f:(fun a_and_b ->
    let%pattern_bind.Ui_incr a, b = a_and_b in
    Incr_map.merge a b ~f)
;;

module Id_gen (T : Int_intf.S) () = struct
  include T

  let component here =
    let%map.Bonsai.Computation _, fetch =
      Bonsai.actor0
        here
        (module T)
        (module Unit)
        ~default_model:T.zero
        ~recv:(fun ~schedule_event:_ i () -> T.( + ) i T.one, i)
    in
    fetch ()
  ;;
end

let mirror
      (type m)
      here
      (module M : Bonsai.Model with type t = m)
      ~store_set
      ~store_value
      ~interactive_set
      ~interactive_value
  =
  let module M2 = struct
    type t =
      { store : M.t
      ; interactive : M.t
      }
    [@@deriving sexp, equal]
  end
  in
  let callback =
    let%map store_set = store_set
    and interactive_set = interactive_set in
    fun old_pair { M2.store = store_value; interactive = interactive_value } ->
      let stability =
        if [%equal: M.t] store_value interactive_value then `Stable else `Unstable
      in
      match stability with
      | `Stable ->
        (* if both of the new values are the same, then we're done! Stability
           has already been reached. *)
        Ui_effect.Ignore
      | `Unstable ->
        (match old_pair with
         | None ->
           (* on_change' is triggered when the values flow through this node
              for the first time.  In this scenario, we prioritize the
              value in the store. *)
           interactive_set store_value
         | Some { M2.store = old_store_value; interactive = old_interactive_value } ->
           let store_changed = not ([%equal: M.t] old_store_value store_value) in
           let interactive_changed =
             not ([%equal: M.t] old_interactive_value interactive_value)
           in
           (match interactive_changed, store_changed with
            (* if the interactive-value has changed, forward that on to the store.
               we intentionally prioritize the interactive value here, so changes to
               the store that happened at the same instant are dropped. *)
            | true, _ -> store_set interactive_value
            (* finally, if the store changed but interactive did not, update the
               interactive value. *)
            | false, true -> interactive_set store_value
            (* this final case should never happen.  Error message explains why.*)
            | false, false ->
              eprint_s
                [%message
                  "BUG" [%here] "on_change triggered when nothing actually changed?"];
              Ui_effect.Ignore))
  in
  Bonsai.Edge.on_change'
    here
    (module M2)
    (let%map store = store_value
     and interactive = interactive_value in
     { M2.store; interactive })
    ~callback
;;
