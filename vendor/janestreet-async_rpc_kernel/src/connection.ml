open Core
open Async_kernel
module Time_ns = Core_private.Time_ns_alternate_sexp
module P = Protocol
module Reader = Transport.Reader
module Writer = Transport.Writer

module Header : sig
  type t [@@deriving bin_type_class]

  val v1 : t
  val negotiate : us:t -> peer:t -> int Or_error.t
end = struct
  include P.Header

  let negotiate = negotiate ~allow_legacy_peer:true
  let v1 = Protocol_version_header.create_exn ~protocol:Rpc ~supported_versions:[ 1 ]
end

module Handshake_error = struct
  module T = struct
    type t =
      | Eof
      | Transport_closed
      | Timeout
      | Reading_header_failed of Error.t
      | Negotiation_failed of Error.t
      | Negotiated_unexpected_version of int
    [@@deriving sexp]
  end

  include T
  include Sexpable.To_stringable (T)

  exception Handshake_error of (t * Info.t) [@@deriving sexp]

  let to_exn ~connection_description t = Handshake_error (t, connection_description)
end

module Heartbeat_config = struct
  type t =
    { timeout : Time_ns.Span.t
    ; send_every : Time_ns.Span.t
    }
  [@@deriving sexp, bin_io, fields]

  let create
        ?(timeout = Time_ns.Span.of_sec 30.)
        ?(send_every = Time_ns.Span.of_sec 10.)
        ()
    =
    { timeout; send_every }
  ;;

  module Runtime = struct
    type t =
      { mutable timeout : Time_ns.Span.t
      ; send_every : Time_ns.Span.t
      }
    [@@deriving sexp_of]
  end

  let to_runtime { timeout; send_every } = { Runtime.timeout; send_every }
end

type response_handler =
  Nat0.t P.Response.t
  -> read_buffer:Bigstring.t
  -> read_buffer_pos_ref:int ref
  -> [ `keep
     | `wait of unit Deferred.t
     | `remove of unit Rpc_result.t
     | `remove_and_wait of unit Deferred.t
     ]

type t =
  { description : Info.t
  ; heartbeat_config : Heartbeat_config.Runtime.t
  ; mutable heartbeat_callbacks : (unit -> unit) array
  ; mutable last_seen_alive : Time_ns.t
  ; reader : Reader.t
  ; writer : Writer.t
  ; open_queries : (P.Query_id.t, (response_handler[@sexp.opaque])) Hashtbl.t
  ; close_started : Info.t Ivar.t
  ; close_finished : unit Ivar.t
  (* There's a circular dependency between connections and their implementation instances
     (the latter depends on the connection state, which is given access to the connection
     when it is created). *)
  ; implementations_instance : Implementations.Instance.t Set_once.t
  ; time_source : Synchronous_time_source.t
  ; heartbeat_event : Synchronous_time_source.Event.t Set_once.t
  }
[@@deriving sexp_of]

let sexp_of_t_hum_writer t =
  [%sexp { description : Info.t = t.description; writer : Writer.t = t.writer }]
;;

let description t = t.description
let is_closed t = Ivar.is_full t.close_started

let writer t =
  if is_closed t || not (Writer.can_send t.writer) then Error `Closed else Ok t.writer
;;

let bytes_to_write t = Writer.bytes_to_write t.writer
let flushed t = Writer.flushed t.writer

let handle_send_result : t -> 'a Transport.Send_result.t -> 'a =
  fun t r ->
  match r with
  | Sent x -> x
  | Closed ->
    (* All of the places we call [handle_send_result] check whether [t] is closed
       (usually via the [writer] function above). This checks whether [t.writer] is
       closed, which should not happen unless [t] is closed. *)
    failwiths ~here:[%here] "RPC connection got closed writer" t sexp_of_t_hum_writer
  | Message_too_big _ ->
    raise_s
      [%sexp
        "Message cannot be sent"
      , { reason = (r : _ Transport.Send_result.t); connection = (t : t_hum_writer) }]
;;

let dispatch t ~response_handler ~bin_writer_query ~query =
  match writer t with
  | Error `Closed as r -> r
  | Ok writer ->
    Option.iter response_handler ~f:(fun response_handler ->
      Hashtbl.set t.open_queries ~key:query.P.Query.id ~data:response_handler);
    Writer.send_bin_prot
      writer
      (P.Message.bin_writer_needs_length (Writer_with_length.of_writer bin_writer_query))
      (Query query)
    |> handle_send_result t;
    Ok ()
;;

let make_dispatch_bigstring do_send t ~tag ~version buf ~pos ~len ~response_handler =
  match writer t with
  | Error `Closed -> Error `Closed
  | Ok writer ->
    let id = P.Query_id.create () in
    let header : Nat0.t P.Message.t =
      Query { tag; version; id; data = Nat0.of_int_exn len }
    in
    Option.iter response_handler ~f:(fun response_handler ->
      Hashtbl.set t.open_queries ~key:id ~data:response_handler);
    let result =
      do_send writer P.Message.bin_writer_nat0_t header ~buf ~pos ~len
      |> handle_send_result t
    in
    Ok result
;;

let dispatch_bigstring = make_dispatch_bigstring Writer.send_bin_prot_and_bigstring

let schedule_dispatch_bigstring =
  make_dispatch_bigstring Writer.send_bin_prot_and_bigstring_non_copying
;;

let handle_response t (response : _ P.Response.t) ~read_buffer ~read_buffer_pos_ref
  : _ Transport.Handler_result.t
  =
  match Hashtbl.find t.open_queries response.id with
  | None -> Stop (Error (Rpc_error.Unknown_query_id response.id))
  | Some response_handler ->
    (match response_handler response ~read_buffer ~read_buffer_pos_ref with
     | `keep -> Continue
     | `wait wait -> Wait wait
     | `remove_and_wait wait ->
       Hashtbl.remove t.open_queries response.id;
       Wait wait
     | `remove removal_circumstances ->
       Hashtbl.remove t.open_queries response.id;
       (match removal_circumstances with
        | Ok () -> Continue
        | Error e ->
          (match e with
           | Unimplemented_rpc _ -> Continue
           | Bin_io_exn _
           | Connection_closed
           | Write_error _
           | Uncaught_exn _
           | Unknown_query_id _ -> Stop (Error e))))
;;

let handle_msg
      t
      (msg : _ P.Message.t)
      ~read_buffer
      ~read_buffer_pos_ref
      ~close_connection_monitor
  : _ Transport.Handler_result.t
  =
  match msg with
  | Heartbeat ->
    Array.iter t.heartbeat_callbacks ~f:(fun f -> f ());
    Continue
  | Response response -> handle_response t response ~read_buffer ~read_buffer_pos_ref
  | Query query ->
    let instance = Set_once.get_exn t.implementations_instance [%here] in
    Implementations.Instance.handle_query
      instance
      ~close_connection_monitor
      ~query
      ~read_buffer
      ~read_buffer_pos_ref
;;

let close_reason t ~on_close =
  let reason = Ivar.read t.close_started in
  match on_close with
  | `started -> reason
  | `finished ->
    let%bind () = Ivar.read t.close_finished in
    reason
;;

let close_finished t = Ivar.read t.close_finished

let add_heartbeat_callback t f =
  (* Adding heartbeat callbacks is relatively rare, but the callbacks are triggered a lot.
     The array representation makes the addition quadratic for the sake of keeping the
     triggering cheap. *)
  t.heartbeat_callbacks <- Array.append [| f |] t.heartbeat_callbacks
;;

let reset_heartbeat_timeout t timeout =
  t.heartbeat_config.timeout <- timeout;
  t.last_seen_alive <- Synchronous_time_source.now t.time_source
;;

let last_seen_alive t = t.last_seen_alive

let abort_heartbeating t =
  Option.iter (Set_once.get t.heartbeat_event) ~f:(fun event ->
    match Synchronous_time_source.Event.abort t.time_source event with
    | Ok | Previously_unscheduled -> ()
    | Currently_happening ->
      Synchronous_time_source.run_after t.time_source Time_ns.Span.zero (fun () ->
        Synchronous_time_source.Event.abort_exn t.time_source event))
;;

let close ?(streaming_responses_flush_timeout = Time_ns.Span.of_int_sec 5) ~reason t =
  if not (is_closed t)
  then (
    abort_heartbeating t;
    Ivar.fill t.close_started reason;
    (match Set_once.get t.implementations_instance with
     | None -> Deferred.unit
     | Some instance ->
       let flushed = Implementations.Instance.flush instance in
       if Deferred.is_determined flushed
       then (
         Implementations.Instance.stop instance;
         flushed)
       else (
         let%map () =
           Deferred.any_unit
             [ flushed
             ; Writer.stopped t.writer
             ; Time_source.after
                 (Time_source.of_synchronous t.time_source)
                 streaming_responses_flush_timeout
             ]
         in
         Implementations.Instance.stop instance))
    >>> fun () ->
    Writer.close t.writer
    >>> fun () -> Reader.close t.reader >>> fun () -> Ivar.fill t.close_finished ());
  close_finished t
;;

let on_message t ~close_connection_monitor =
  let f buf ~pos ~len:_ : _ Transport.Handler_result.t =
    let pos_ref = ref pos in
    let nat0_msg = P.Message.bin_read_nat0_t buf ~pos_ref in
    match
      handle_msg
        t
        nat0_msg
        ~read_buffer:buf
        ~read_buffer_pos_ref:pos_ref
        ~close_connection_monitor
    with
    | Continue -> Continue
    | Wait _ as res -> res
    | Stop result ->
      let reason =
        let msg = "Rpc message handling loop stopped" in
        match result with
        | Ok () -> Info.of_string msg
        | Error e ->
          Info.create
            msg
            e
            (Rpc_error.sexp_of_t ~get_connection_close_reason:(fun () ->
               [%sexp
                 "Connection.on_message resulted in Connection_closed error. This is \
                  weird."]))
      in
      don't_wait_for (close t ~reason);
      Stop reason
  in
  Staged.stage f
;;

let heartbeat_now t =
  let since_last_heartbeat =
    Time_ns.diff (Synchronous_time_source.now t.time_source) t.last_seen_alive
  in
  if Time_ns.Span.( > ) since_last_heartbeat t.heartbeat_config.timeout
  then (
    let reason () =
      sprintf
        !"No heartbeats received for %{sexp:Time_ns.Span.t}."
        t.heartbeat_config.timeout
    in
    don't_wait_for (close t ~reason:(Info.of_thunk reason)))
  else (
    match writer t with
    | Error `Closed -> ()
    | Ok writer ->
      Writer.send_bin_prot writer P.Message.bin_writer_nat0_t Heartbeat
      |> handle_send_result t)
;;

let default_handshake_timeout = Time_ns.Span.of_sec 30.

let cleanup t ~reason exn =
  don't_wait_for (close ~reason t);
  if not (Hashtbl.is_empty t.open_queries)
  then (
    let error =
      match exn with
      | Rpc_error.Rpc (error, (_ : Info.t)) -> error
      | exn -> Uncaught_exn (Exn.sexp_of_t exn)
    in
    (* clean up open streaming responses *)
    (* an unfortunate hack; ok because the response handler will have nothing
       to read following a response where [data] is an error *)
    let dummy_buffer = Bigstring.create 1 in
    let dummy_ref = ref 0 in
    Hashtbl.iteri t.open_queries ~f:(fun ~key:query_id ~data:response_handler ->
      ignore
        (response_handler
           ~read_buffer:dummy_buffer
           ~read_buffer_pos_ref:dummy_ref
           { id = query_id; data = Error error }));
    Hashtbl.clear t.open_queries;
    Bigstring.unsafe_destroy dummy_buffer)
;;

let schedule_heartbeats t =
  t.last_seen_alive <- Synchronous_time_source.now t.time_source;
  let heartbeat_from_now_on =
    (* [at_intervals] will schedule the first heartbeat the first time the time_source is
       advanced *)
    Synchronous_time_source.Event.at_intervals
      t.time_source
      t.heartbeat_config.send_every
      (fun () -> heartbeat_now t)
  in
  Set_once.set_exn t.heartbeat_event [%here] heartbeat_from_now_on
;;

let run_after_handshake t ~implementations ~connection_state ~writer_monitor_exns =
  let instance =
    Implementations.instantiate
      implementations
      ~writer:t.writer
      ~connection_description:t.description
      ~connection_close_started:(Ivar.read t.close_started)
      ~connection_state:(connection_state t)
  in
  Set_once.set_exn t.implementations_instance [%here] instance;
  let close_connection_monitor = Monitor.create ~name:"RPC close connection monitor" () in
  Monitor.detach_and_iter_errors close_connection_monitor ~f:(fun exn ->
    let reason =
      Info.create_s [%message "Uncaught exception in implementation" (exn : Exn.t)]
    in
    don't_wait_for (close ~reason t));
  let monitor = Monitor.create ~name:"RPC connection loop" () in
  let reason name exn =
    exn, Info.tag (Info.of_exn exn) ~tag:("exn raised in RPC connection " ^ name)
  in
  Stream.iter
    (Stream.interleave
       (Stream.of_list
          [ Stream.map ~f:(reason "loop") (Monitor.detach_and_get_error_stream monitor)
          ; Stream.map ~f:(reason "Writer.t") writer_monitor_exns
          ]))
    ~f:(fun (exn, reason) -> cleanup t exn ~reason);
  within ~monitor (fun () ->
    schedule_heartbeats t;
    Reader.read_forever
      t.reader
      ~on_message:(Staged.unstage (on_message t ~close_connection_monitor))
      ~on_end_of_batch:(fun () ->
        t.last_seen_alive <- Synchronous_time_source.now t.time_source)
    >>> function
    | Ok reason -> cleanup t ~reason (Rpc_error.Rpc (Connection_closed, t.description))
    (* The protocol is such that right now, the only outcome of the other side closing the
       connection normally is that we get an eof. *)
    | Error (`Eof | `Closed) ->
      cleanup
        t
        ~reason:(Info.of_string "EOF or connection closed")
        (Rpc_error.Rpc (Connection_closed, t.description)))
;;

let do_handshake t ~handshake_timeout =
  match writer t with
  | Error `Closed -> return (Error Handshake_error.Transport_closed)
  | Ok writer ->
    Writer.send_bin_prot writer Header.bin_t.writer Header.v1 |> handle_send_result t;
    (* If we use [max_connections] in the server, then this read may just hang until the
       server starts accepting new connections (which could be never).  That is why a
       timeout is used *)
    let result =
      Monitor.try_with
        ~rest:`Log
        ~run:`Now
        (fun () -> Reader.read_one_message_bin_prot t.reader Header.bin_t.reader)
    in
    (match%map
       Time_source.with_timeout
         (Time_source.of_synchronous t.time_source)
         handshake_timeout
         result
     with
     | `Timeout ->
       (* There's a pending read, the reader is basically useless now, so we clean it
          up. *)
       don't_wait_for (close t ~reason:(Info.of_string "Handshake timeout"));
       Error Handshake_error.Timeout
     | `Result (Error exn) ->
       let reason = Info.of_string "[Reader.read_one_message_bin_prot] raised" in
       don't_wait_for (close t ~reason);
       Error (Reading_header_failed (Error.of_exn exn))
     | `Result (Ok (Error `Eof)) -> Error Eof
     | `Result (Ok (Error `Closed)) -> Error Transport_closed
     | `Result (Ok (Ok peer)) ->
       (match Header.negotiate ~us:Header.v1 ~peer with
        | Error e -> Error (Negotiation_failed e)
        | Ok 1 -> Ok ()
        | Ok i -> Error (Negotiated_unexpected_version i)))
;;

let contains_magic_prefix = Protocol_version_header.contains_magic_prefix ~protocol:Rpc

let create
      ?implementations
      ~connection_state
      ?(handshake_timeout = default_handshake_timeout)
      ?(heartbeat_config = Heartbeat_config.create ())
      ?(description = Info.of_string "<created-directly>")
      ?(time_source = Synchronous_time_source.wall_clock ())
      ({ reader; writer } : Transport.t)
  =
  let implementations =
    match implementations with
    | None -> Implementations.null ()
    | Some s -> s
  in
  let t =
    { description
    ; heartbeat_config = Heartbeat_config.to_runtime heartbeat_config
    ; heartbeat_callbacks = [||]
    ; last_seen_alive = Synchronous_time_source.now time_source
    ; reader
    ; writer
    ; open_queries = Hashtbl.Poly.create ~size:10 ()
    ; close_started = Ivar.create ()
    ; close_finished = Ivar.create ()
    ; implementations_instance = Set_once.create ()
    ; time_source
    ; heartbeat_event = Set_once.create ()
    }
  in
  let writer_monitor_exns = Monitor.detach_and_get_error_stream (Writer.monitor writer) in
  upon (Writer.stopped writer) (fun () ->
    don't_wait_for (close t ~reason:(Info.of_string "RPC transport stopped")));
  match%map do_handshake t ~handshake_timeout with
  | Ok () ->
    run_after_handshake t ~implementations ~connection_state ~writer_monitor_exns;
    Ok t
  | Error error ->
    Error (Handshake_error.to_exn ~connection_description:description error)
;;

let with_close
      ?implementations
      ?handshake_timeout
      ?heartbeat_config
      ?description
      ?time_source
      ~connection_state
      transport
      ~dispatch_queries
      ~on_handshake_error
  =
  let handle_handshake_error =
    match on_handshake_error with
    | `Call f -> f
    | `Raise -> raise
  in
  let%bind t =
    create
      ?implementations
      ?handshake_timeout
      ?heartbeat_config
      ?description
      ?time_source
      ~connection_state
      transport
  in
  match t with
  | Error e ->
    let%bind () = Transport.close transport in
    handle_handshake_error e
  | Ok t ->
    Monitor.protect
      ~run:
        `Schedule
      ~rest:`Log
      ~finally:(fun () ->
        close t ~reason:(Info.of_string "Rpc.Connection.with_close finished"))
      (fun () ->
         let%bind result = dispatch_queries t in
         let%map () =
           match implementations with
           | None -> Deferred.unit
           | Some _ -> close_finished t
         in
         result)
;;

let server_with_close
      ?handshake_timeout
      ?heartbeat_config
      ?description
      ?time_source
      transport
      ~implementations
      ~connection_state
      ~on_handshake_error
  =
  let on_handshake_error =
    match on_handshake_error with
    | `Call f -> `Call f
    | `Raise -> `Raise
    | `Ignore -> `Call (fun _ -> Deferred.unit)
  in
  with_close
    ?handshake_timeout
    ?heartbeat_config
    ?description
    ?time_source
    transport
    ~implementations
    ~connection_state
    ~on_handshake_error
    ~dispatch_queries:(fun _ -> Deferred.unit)
;;

let close
      ?streaming_responses_flush_timeout
      ?(reason = Info.of_string "Rpc.Connection.close")
      t
  =
  close ?streaming_responses_flush_timeout ~reason t
;;

module Client_implementations = struct
  type nonrec 's t =
    { connection_state : t -> 's
    ; implementations : 's Implementations.t
    }

  let null () =
    { connection_state = (fun _ -> ()); implementations = Implementations.null () }
  ;;
end
