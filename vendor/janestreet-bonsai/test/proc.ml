open! Core
open! Bonsai
open! Import

module Result_spec = struct
  module type S = sig
    type t
    type incoming

    val view : t -> string
    val incoming : t -> incoming -> unit Effect.t
  end

  type ('result, 'incoming) t =
    (module S with type t = 'result and type incoming = 'incoming)

  module No_incoming = struct
    type incoming = Nothing.t

    let incoming _t incoming = Nothing.unreachable_code incoming
  end

  module type Sexpable = sig
    type t [@@deriving sexp_of]
  end

  module type Stringable = sig
    type t

    val to_string : t -> string
  end

  let invisible (type a) : (a, Nothing.t) t =
    (module struct
      type t = a

      include No_incoming

      let view _ = ""
    end)
  ;;

  let sexp (type a) (module S : Sexpable with type t = a) =
    (module struct
      type t = a

      include No_incoming

      let view s = s |> S.sexp_of_t |> Sexp.to_string_hum
    end : S
      with type t = a
       and type incoming = Nothing.t)
  ;;

  let string (type a) (module S : Stringable with type t = a) =
    (module struct
      type t = a

      include No_incoming

      let view s = s |> S.to_string
    end : S
      with type t = a
       and type incoming = Nothing.t)
  ;;
end

module Handle = struct
  type ('result, 'incoming) t =
    (unit, 'result * string * ('incoming -> unit Effect.t)) Driver.t

  let create
        (type result incoming)
        (result_spec : (result, incoming) Result_spec.t)
        ?(clock = Incr.Clock.create ~start:Time_ns.epoch ())
        computation
    =
    let (module R) = result_spec in
    let component (_ : unit Value.t) =
      let open Bonsai.Let_syntax in
      let%sub result = computation in
      return
        (let%map result = result in
         result, R.view result, R.incoming result)
    in
    Driver.create ~initial_input:() ~clock component
  ;;

  let result handle =
    Driver.flush handle;
    let result, _, _ = Driver.result handle in
    result
  ;;

  let clock = Driver.clock
  let advance_clock_by t = Incr.Clock.advance_clock_by (Driver.clock t)

  let do_actions handle actions =
    let _, _, inject_action = Driver.result handle in
    let event = actions |> List.map ~f:inject_action |> Effect.sequence in
    Driver.schedule_event handle event
  ;;

  let disable_bonsai_path_censoring = Driver.disable_bonsai_path_censoring
  let path_regex = Re.Str.regexp "bonsai_path\\(_[a-z]*\\)*"

  let maybe_censor_bonsai_path handle string =
    if Driver.should_censor_bonsai_path handle
    then Re.Str.global_replace path_regex "bonsai_path_replaced_in_test" string
    else string
  ;;

  let disable_bonsai_hash_censoring = Driver.disable_bonsai_hash_censoring
  let hash_regex = Re.Str.regexp "_hash_[a-f0-9]+"

  let maybe_censor_bonsai_hash handle string =
    if Driver.should_censor_bonsai_hash handle
    then Re.Str.global_replace hash_regex "_hash_replaced_in_test" string
    else string
  ;;

  let recompute_view handle =
    Driver.flush handle;
    let _, view, _ = Driver.result handle in
    let (_ : string) = maybe_censor_bonsai_path handle view in
    Driver.trigger_lifecycles handle
  ;;

  let recompute_view_until_stable ?(max_computes = 100) handle =
    with_return (fun { return } ->
      for _ = 1 to max_computes do
        recompute_view handle;
        if not (Driver.has_after_display_events handle) then return ()
      done;
      failwithf "view not stable after %d recomputations" max_computes ())
  ;;

  let generic_show handle ~before ~f =
    let before = before handle in
    Driver.flush handle;
    let _, view, _ = Driver.result handle in
    let view =
      view |> maybe_censor_bonsai_path handle |> maybe_censor_bonsai_hash handle
    in
    Driver.store_view handle view;
    f before view;
    Driver.trigger_lifecycles handle
  ;;

  let show handle =
    generic_show handle ~before:(Fn.const ()) ~f:(fun () view -> print_endline view)
  ;;

  let show_diff ?(location_style = Patdiff_kernel.Format.Location_style.None) handle =
    generic_show
      handle
      ~before:Driver.last_view
      ~f:(Expect_test_patdiff.print_patdiff ~location_style)
  ;;

  let store_view handle = generic_show handle ~before:(Fn.const ()) ~f:(fun () _ -> ())

  let show_model handle =
    Driver.flush handle;
    Driver.sexp_of_model handle |> print_s
  ;;

  let flush handle = Driver.flush handle

  let result_incr handle =
    let%pattern_bind.Incr result, _view, _inject = Driver.result_incr handle in
    result
  ;;

  let apply_action_incr = Driver.apply_action_incr
  let lifecycle_incr = Driver.lifecycle_incr
end
