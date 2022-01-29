module Uu = Unix
open! Core
open! Notty
open! Notty.Infix
module Incr = Ui_incr
module Term = Notty_unix.Term
module Bonsai = Bonsai
module Event = Event

module Action = struct
  type ('dynamic_action, 'static_action) t =
    | Dynamic of 'dynamic_action
    | Static of 'static_action
end

type view =
  { width : int
  ; height : int
  ; image : I.t Lazy.t
  ; cursor : (int * int) option
  }

type event_handler = Event.t -> unit Ui_effect.t
type ret = view * event_handler

type ('m, 'dynamic_action, 'static_action) unpacked =
  { term : Term.t
  ; clock : Incr.Clock.t
  ; queue : ('dynamic_action, 'static_action) Action.t Queue.t
  ; model_var : 'm Incr.Var.t
  ; size_var : (int * int) Incr.Var.t
  ; dynamic_apply_action_incr :
      (schedule_event:(unit Ui_effect.t -> unit) -> 'm -> 'dynamic_action -> 'm) Incr.t
  ; dynamic_apply_action :
      (schedule_event:(unit Ui_effect.t -> unit) -> 'm -> 'dynamic_action -> 'm)
        Incr.Observer.t
  ; static_apply_action :
      schedule_event:(unit Ui_effect.t -> unit) -> 'm -> 'static_action -> 'm
  ; result : ret Incr.Observer.t
  ; result_incr : ret Incr.t
  ; lifecycle : Bonsai.Private.Lifecycle.Collection.t Incr.Observer.t
  ; lifecycle_incr : Bonsai.Private.Lifecycle.Collection.t Incr.t
  ; inject : ('dynamic_action, 'static_action) Action.t -> unit Ui_effect.t
  ; mutable last_lifecycle : Bonsai.Private.Lifecycle.Collection.t
  }

type t = T : (_, _, _) unpacked -> t

let create ~clock (computation : (int * int) Bonsai.Value.t -> ret Bonsai.Computation.t)
    : t
  =
  let term = Term.create () in
  let size_var = Incr.Var.create (Term.size term) in
  let size_val =
    Bonsai.Private.Value.of_incr (Incr.Var.watch size_var) |> Bonsai.Private.conceal_value
  in
  let (Bonsai.Private.Computation.T
        { t = component_unpacked
         ; dynamic_action = _
         ; static_action = _
         ; apply_static
         ; model =
           { default = default_model ; _ 
             }
         })
    =
    size_val |> computation |> Bonsai.Private.reveal_computation
  in
  let environment = Bonsai.Private.Environment.empty in
  let starting_model = default_model in
  let model_var = Incr.Var.create starting_model in
  (* Sadly the only way to give a name to the existential type that we just introduced
     into the environment is by defining a function like this. See
     https://github.com/ocaml/ocaml/issues/7074. *)
  let create_polymorphic
      (type dynamic_action static_action)
      (computation : (_, dynamic_action, static_action, ret) Bonsai.Private.Computation.t)
      apply_static
      : t
    =
    let queue = Queue.create () in
    let module A =
      Ui_effect.Define (struct
        module Action = struct
          type t = (dynamic_action, static_action) Action.t
        end

        let handle = Queue.enqueue queue
      end)
    in
    let inject = A.inject in
    let inject_dynamic a = A.inject (Dynamic a) in
    let inject_static a = A.inject (Static a) in
    let snapshot =
      Bonsai.Private.eval
        ~environment
        ~path:Bonsai.Private.Path.empty
        ~clock
        ~model:(Incr.Var.watch model_var)
        ~inject_dynamic
        ~inject_static
        computation
    in
    let result_incr = Bonsai.Private.Snapshot.result snapshot in
    let dynamic_apply_action_incr =
      Bonsai.Private.Apply_action.to_incremental
        (Bonsai.Private.Snapshot.apply_action snapshot)
    in
    let dynamic_apply_action = Incr.observe dynamic_apply_action_incr in
    let result = result_incr |> Incr.observe in
    let lifecycle_incr = Bonsai.Private.Snapshot.lifecycle_or_empty snapshot in
    let lifecycle = Incr.observe lifecycle_incr in
    Incr.stabilize ();
    T
      { term
      ; clock
      ; size_var
      ; inject
      ; model_var
      ; dynamic_apply_action
      ; dynamic_apply_action_incr
      ; static_apply_action = apply_static ~inject:inject_static
      ; result
      ; result_incr
      ; lifecycle
      ; lifecycle_incr
      ; queue
      ; last_lifecycle = Bonsai.Private.Lifecycle.Collection.empty
      }
  in
  create_polymorphic component_unpacked apply_static
;;

let process_queue ((T { model_var; static_apply_action; dynamic_apply_action; queue; _ })) =
  let update_model ~action ~apply_action =
    (* The only difference between [Var.latest_value] and [Var.value] is that
       if [Var.set] is called _while stabilizing_, then calling [Var.value]
       will return the value that was set when stabilization started, whereas
       [latest_value] will give you the value that was just [set].  Now,
       setting a model in the middle of a stabilizaiton should never happen,
       but I think it's important to be explicit about which behavior we use,
       so I chose the one that would be least surprising if a stabilization
       does happen to occur. *)
    Incr.Var.set
      model_var
      (apply_action
         ~schedule_event:Ui_effect.Expert.handle
         (Incr.Var.latest_value model_var)
         action)
  in
  let process_event (action : _ Action.t) =
    match action with
    | Static action -> update_model ~apply_action:static_apply_action ~action
    | Dynamic action ->
      (* We need to stabilize before every action so that the [input] for the
         apply-actions are up to date. *)
      Incr.stabilize ();
      let apply_action = Incr.Observer.value_exn dynamic_apply_action in
      update_model ~apply_action ~action
  in
  while not (Queue.is_empty queue) do
    process_event (Queue.dequeue_exn queue)
  done;
  Incr.stabilize ()
;;

let trigger_lifecycles (T t) =
  let old = t.last_lifecycle in
  let new_ = t.lifecycle |> Incr.Observer.value_exn in
  t.last_lifecycle <- new_;
  Ui_effect.Expert.handle (Bonsai.Private.Lifecycle.Collection.diff old new_)
;;

let show (T { term; result; size_var; _ } as t) =
  Incr.Var.set size_var (Term.size term);
  Incr.stabilize ();
  process_queue t;
  let _, inject = Incr.Observer.value_exn result in
  while Term.pending term do
    match Term.event term with
    | `End | `Key (`ASCII 'C', [ `Ctrl ]) ->
      Term.release term;
      exit 0
    | event -> Ui_effect.Expert.handle (inject event)
  done;
  process_queue t;
  let view, _ = Incr.Observer.value_exn result in
  Term.image term (Lazy.force view.image);
  Term.cursor term view.cursor;
  trigger_lifecycles t
;;

let create ?(clock = Ui_incr.clock) computation =
  let t = create ~clock computation in
  show t;
  t
;;

let rec block_on_input (T { term; _ } as t) =
  let input, _ = Term.fds term in
  try
    let read, _, _ = Uu.select [ input ] [] [] (1.0 /. 60.0) in
    if List.is_empty read then () else Term.feed term
  with
  | Uu.Unix_error (EINTR, _, _) -> block_on_input t
;;

let loop t =
  while true do
    block_on_input t;
    show t
  done;
  ()
;;
