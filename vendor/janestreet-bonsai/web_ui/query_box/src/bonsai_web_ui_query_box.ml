open! Core
open Bonsai_web
open Bonsai.Let_syntax
open Vdom

module Model = struct
  type 'k suggestion_list_state =
    | Selected of 'k
    | First_item
    | Closed
  [@@deriving equal, sexp]

  type 'k t =
    { query : string
    ; suggestion_list_state : 'k suggestion_list_state
    }
  [@@deriving equal, sexp]
end

module Action = struct
  type t =
    | Set_query of string
    | Move_next
    | Move_prev
    | Close_suggestions
    | Open_suggestions
  [@@deriving sexp]
end

module Suggestion_list_kind = struct
  type t =
    | Transient_overlay
    | Permanent_fixture
  [@@deriving sexp, compare, enumerate, equal]
end

module Expand_direction = struct
  type t =
    | Down
    | Up
  [@@deriving sexp, compare, enumerate, equal]
end

let select_key ~first_try ~then_try ~else_use =
  match first_try with
  | Some (key, _) -> Model.Selected key
  | None ->
    (match then_try with
     | (lazy (Some (key, _))) -> Model.Selected key
     | (lazy None) -> else_use)
;;

let create
      (type k cmp)
      (module Key : Bonsai.Comparator with type t = k and type comparator_witness = cmp)
      ?(initial_query = "")
      ?(max_visible_items = Value.return 10)
      ?(suggestion_list_kind = Value.return Suggestion_list_kind.Transient_overlay)
      ?(expand_direction = Value.return Expand_direction.Down)
      ?(selected_item_attr = Value.return Attr.empty)
      ?(extra_list_container_attr = Value.return Attr.empty)
      ?(extra_input_attr = Value.return Attr.empty)
      ?(extra_attr = Value.return Attr.empty)
      ~f
      ~on_select
      ()
  =
  let%sub { Model.query; suggestion_list_state }, inject, items =
    Bonsai.wrap
      (module struct
        type t = Key.t Model.t [@@deriving sexp]

        let equal a b = Model.equal (fun a b -> Key.comparator.compare a b = 0) a b
      end)
      ~default_model:{ Model.query = initial_query; suggestion_list_state = Closed }
      ~apply_action:(fun ~inject:_ ~schedule_event:_ (_, _, items) model action ->
        let suggestion_list_state =
          (* We normalize which item is selected in case the list has changed
             since the last action. Normalization just means setting the
             selected key to the closest thing that actually exists. *)
          match model.suggestion_list_state with
          | Selected key ->
            select_key
              ~first_try:(Map.closest_key items `Less_or_equal_to key)
              ~then_try:(lazy (Map.closest_key items `Greater_or_equal_to key))
              ~else_use:First_item
          | First_item -> First_item
          | Closed -> Closed
        in
        match action with
        | Action.Set_query query ->
          let suggestion_list_state =
            match suggestion_list_state with
            | Selected key -> Model.Selected key
            | First_item | Closed -> First_item
          in
          { Model.query; suggestion_list_state }
        | Open_suggestions -> { model with suggestion_list_state = First_item }
        | Close_suggestions -> { model with suggestion_list_state = Closed }
        | Move_next ->
          let suggestion_list_state =
            match suggestion_list_state with
            | Selected key ->
              select_key
                ~first_try:(Map.closest_key items `Greater_than key)
                ~then_try:(lazy (Map.min_elt items))
                ~else_use:(Selected key)
            | First_item ->
              (match Map.min_elt items with
               | None -> First_item
               | Some (first_key, _) ->
                 (match Map.closest_key items `Greater_than first_key with
                  | None -> Selected first_key
                  | Some (second_key, _) -> Selected second_key))
            | Closed -> First_item
          in
          { model with suggestion_list_state }
        | Move_prev ->
          let suggestion_list_state =
            match model.suggestion_list_state with
            | Selected key ->
              select_key
                ~first_try:(Map.closest_key items `Less_than key)
                ~then_try:(lazy (Map.max_elt items))
                ~else_use:(Selected key)
            | First_item | Closed ->
              (match Map.max_elt items with
               | None -> First_item
               | Some (last_key, _) -> Selected last_key)
          in
          { model with suggestion_list_state })
      ~f:(fun model inject ->
        let%sub { Model.query; _ } = return model in
        let%sub items = f query in
        let%arr model = model
        and inject = inject
        and items = items in
        model, inject, items)
  in
  let%sub selected_key =
    match%sub suggestion_list_state with
    | Selected key ->
      let%arr key = key
      and items = items in
      (match Map.closest_key items `Less_or_equal_to key with
       | Some (key, _) -> Some key
       | None ->
         (match Map.closest_key items `Greater_or_equal_to key with
          | Some (key, _) -> Some key
          | None -> None))
    | First_item ->
      let%arr items = items in
      (match Map.min_elt items with
       | Some (key, _) -> Some key
       | None -> None)
    | Closed -> Bonsai.const None
  in
  let%sub items =
    Bonsai.assoc
      (module Key)
      items
      ~f:(fun key item ->
        let%arr key = key
        and item = item
        and selected_key = selected_key
        and selected_item_attr = selected_item_attr in
        let attr =
          match selected_key with
          | Some selected_key when Key.comparator.compare key selected_key = 0 ->
            selected_item_attr
          | _ -> Attr.empty
        in
        Node.div ~attr [ item ])
  in
  let%sub restricted_items =
    let%arr items = items
    and max_visible_items = max_visible_items
    and selected_key = selected_key in
    match selected_key with
    | Some selected_key ->
      let last_length = ref (-1) in
      let length = ref 0 in
      let items = ref items in
      let result = ref (Map.empty (module Key)) in
      (* We alternate between taking something larger and smaller than the
         selected key until we have taken [max_visible_items] or have exhausted
         the source list. This is probably not done in the most efficient
         manner, but it's O(max_visible_items * log(number_of_items)), which is
         probably acceptable if [max_visible_items] is small. *)
      while !last_length < !length do
        last_length := !length;
        if !length < max_visible_items
        then (
          match Map.closest_key !items `Less_or_equal_to selected_key with
          | Some (key, data) ->
            result := Map.set !result ~key ~data;
            items := Map.remove !items key;
            incr length
          | None -> ());
        if !length < max_visible_items
        then (
          match Map.closest_key !items `Greater_or_equal_to selected_key with
          | Some (key, data) ->
            result := Map.set !result ~key ~data;
            items := Map.remove !items key;
            incr length
          | None -> ())
      done;
      Map.data !result
    | None ->
      Sequence.take (Map.to_sequence items) (max max_visible_items 0)
      |> Sequence.map ~f:snd
      |> Sequence.to_list
  in
  let%sub handle_keydown =
    let%arr inject = inject
    and selected_key = selected_key
    and on_select = on_select
    and expand_direction = expand_direction in
    let open Vdom in
    let open Js_of_ocaml in
    fun ev ->
      let move_next = Effect.Many [ inject Move_next; Effect.Prevent_default ] in
      let move_prev = Effect.Many [ inject Move_prev; Effect.Prevent_default ] in
      let up, down =
        match expand_direction with
        | Up -> move_next, move_prev
        | Down -> move_prev, move_next
      in
      match Dom_html.Keyboard_code.of_event ev with
      | ArrowUp -> up
      | Tab when Js.to_bool ev##.shiftKey ->
        (match selected_key with
         | Some _ -> up
         | None -> Effect.Ignore)
      | ArrowDown -> down
      | Tab ->
        (match selected_key with
         | Some _ -> down
         | None -> Effect.Ignore)
      | Escape -> inject Action.Close_suggestions
      | Enter ->
        (match selected_key with
         | Some key ->
           Effect.Many
             [ on_select key
             ; inject (Set_query "")
             ; inject Close_suggestions
             ; Effect.Prevent_default
             ]
         | None -> inject Open_suggestions)
      | _ -> Effect.Ignore
  in
  let%arr query = query
  and selected_key = selected_key
  and inject = inject
  and handle_keydown = handle_keydown
  and suggestion_list_kind = suggestion_list_kind
  and expand_direction = expand_direction
  and restricted_items = restricted_items
  and extra_list_container_attr = extra_list_container_attr
  and extra_input_attr = extra_input_attr
  and extra_attr = extra_attr in
  let container_position, suggestions_position, is_open =
    match suggestion_list_kind with
    | Suggestion_list_kind.Transient_overlay ->
      let is_open = Option.is_some selected_key in
      ( Attr.style (Css_gen.position `Relative)
      , Attr.style (Css_gen.position `Absolute)
      , is_open )
    | Permanent_fixture -> Attr.empty, Attr.empty, true
  in
  let input =
    Node.input
      ~attr:
        Attr.(
          string_property "value" query
          @ on_keydown handle_keydown
          @ on_input (fun _ query -> inject (Set_query query))
          @ on_focus (fun _ -> inject Open_suggestions)
          @ on_blur (fun _ -> inject Close_suggestions)
          @ extra_input_attr)
      []
  in
  let suggestions =
    match is_open with
    | false -> Node.div []
    | true ->
      let position_above_or_below, directed_items =
        match expand_direction with
        | Up -> Attr.style (Css_gen.bottom (`Px 0)), List.rev restricted_items
        | Down -> Attr.empty, restricted_items
      in
      let attr =
        Attr.(suggestions_position @ position_above_or_below @ extra_list_container_attr)
      in
      Node.div ~attr directed_items
  in
  let suggestions_container = Node.div ~attr:container_position [ suggestions ] in
  Node.div
    ~attr:extra_attr
    (match expand_direction with
     | Up -> [ suggestions_container; input ]
     | Down -> [ input; suggestions_container ])
;;

let stringable
      (type k cmp)
      (module Key : Bonsai.Comparator with type t = k and type comparator_witness = cmp)
      ?initial_query
      ?max_visible_items
      ?suggestion_list_kind
      ?expand_direction
      ?selected_item_attr
      ?extra_list_container_attr
      ?extra_input_attr
      ?extra_attr
      ?(to_view = fun _ string -> Vdom.Node.text string)
      ~on_select
      input
  =
  create
    (module Key)
    ?initial_query
    ?max_visible_items
    ?suggestion_list_kind
    ?expand_direction
    ?selected_item_attr
    ?extra_list_container_attr
    ?extra_input_attr
    ?extra_attr
    ~on_select
    ~f:(fun query ->
      Bonsai.Incr.compute (Value.both query input) ~f:(fun incr ->
        let%pattern_bind.Incr query, input = incr in
        Incr_map.filter_mapi' input ~f:(fun ~key ~data:string ->
          let%map.Incr string = string
          and query = query in
          if Fuzzy_match.is_match
               ~char_equal:Char.Caseless.equal
               ~pattern:query
               string
          then Some (to_view key string)
          else None)))
    ()
;;
