module Uu = Unix
open! Core
open! Notty
open! Notty.Infix
module Incr = Ui_incr
module Term = Notty_unix.Term
module Bonsai = Bonsai
module Event = Event

type view =
  { width : int
  ; height : int
  ; image : I.t Lazy.t
  ; cursor : (int * int) option
  }

type event_handler = Event.t -> unit Ui_effect.t
type ret = view * event_handler

type ('m, 'a) unpacked =
  { term : Term.t
  ; clock : Incr.Clock.t
  ; queue : 'a Queue.t
  ; model_var : 'm Incr.Var.t
  ; size_var : (int * int) Incr.Var.t
  ; apply_action :
      (schedule_event:(unit Ui_effect.t -> unit) -> 'm -> 'a -> 'm) Incr.Observer.t
  ; apply_action_incr :
      (schedule_event:(unit Ui_effect.t -> unit) -> 'm -> 'a -> 'm) Incr.t
  ; result : ret Incr.Observer.t
  ; result_incr : ret Incr.t
  ; lifecycle : Bonsai.Private.Lifecycle.Collection.t Incr.Observer.t
  ; lifecycle_incr : Bonsai.Private.Lifecycle.Collection.t Incr.t
  ; inject : 'a -> unit Ui_effect.t
  ; mutable last_lifecycle : Bonsai.Private.Lifecycle.Collection.t
  }

type t = T : (_, _) unpacked -> t

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
        ; action
        ; model =
            { default = default_model; sexp_of = _; equal = _; type_id = _; of_sexp = _ }
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
      (type a)
      (computation : (_, a, ret) Bonsai.Private.Computation.t)
      (_action : a Type_equal.Id.t)
      : t
    =
    let queue = Queue.create () in
    let module A =
      Ui_effect.Define (struct
        module Action = struct
          type t = a
        end

        let handle = Queue.enqueue queue
      end)
    in
    let inject = A.inject in
    let snapshot =
      Bonsai.Private.eval
        ~environment
        ~path:Bonsai.Private.Path.empty
        ~clock
        ~model:(Incr.Var.watch model_var)
        ~inject
        computation
    in
    let result_incr = Bonsai.Private.Snapshot.result snapshot in
    let apply_action_incr =
      Bonsai.Private.Snapshot.(Apply_action.to_incremental (apply_action snapshot))
    in
    let apply_action = Incr.observe apply_action_incr in
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
      ; apply_action
      ; apply_action_incr
      ; result
      ; result_incr
      ; lifecycle
      ; lifecycle_incr
      ; queue
      ; last_lifecycle = Bonsai.Private.Lifecycle.Collection.empty
      }
  in
  create_polymorphic component_unpacked action
;;

let process_queue (T { model_var; apply_action; queue; _ }) =
  let process_event action =
    let apply_action = Incr.Observer.value_exn apply_action in
    let new_model =
      apply_action
        (Incr.Var.value model_var)
        action
        ~schedule_event:Ui_effect.Expert.handle
    in
    Incr.Var.set model_var new_model;
    (* We need to stabilize after every action so that [Snapshot.apply_action] is closed
       over the latest model. *)
    Incr.stabilize ()
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
