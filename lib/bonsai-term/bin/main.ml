module Ucharr = Uchar
open Core
open! Bonsai_term
open! Notty
open! Notty.Infix
open Bonsai.Let_syntax
module _ = Blocklist

let _outline i =
  let attr = A.(fg lightblack) in
  let w, h = I.(width i, height i) in
  let chr x = I.uchar ~attr (Ucharr.of_int x) 1 1
  and hbar = I.uchar ~attr (Ucharr.of_int 0x2500) w 1
  and vbar = I.uchar ~attr (Ucharr.of_int 0x2502) 1 h in
  let a, b, c, d = chr 0x256d, chr 0x256e, chr 0x256f, chr 0x2570 in
  let frame =
    a <|> hbar <|> b <-> (vbar <|> I.void w h <|> vbar) <-> (d <|> hbar <|> c)
  in
  frame </> I.pad ~t:1 ~l:1 i
;;

let bracket i =
  let attr = A.(fg lightblack) in
  let _w, h = I.(width i, height i) in
  let chr x = I.uchar ~attr (Ucharr.of_int x) 1 1
  and vbar = I.uchar ~attr (Ucharr.of_int 0x2502) 1 h in
  let a, d = chr 0x256d, chr 0x2570 in
  a <-> (vbar <|> i) <-> d
;;

let outline = bracket

module Event_list = struct
  type t = Event.t list [@@deriving sexp, equal]
end

let concatlike m a ~add =
  Bonsai.state_machine0 [%here] m a ~apply_action:(fun ~inject:_ ~schedule_event:_ m a ->
      add m a)
;;

let vcat (v1 : view) (v2 : view) : view =
  let width = Int.max v1.width v2.width in
  let height = v1.height + v2.height in
  let image = lazy (Lazy.force v1.image <-> Lazy.force v2.image) in
  let cursor =
    match v1.cursor, v2.cursor with
    | Some c1, _ -> Some c1
    | None, Some (x, y) -> Some (x, y + v1.height)
    | None, None -> None
  in
  { width; height; image; cursor }
;;

let unhandled =
  let%sub unhandled =
    concatlike
      (module Event_list)
      (module Event)
      ~default_model:[]
      ~add:(Fn.flip List.cons)
  in
  let%arr unhandled, append_unhandled = unhandled in
  let image =
    lazy
      (unhandled
      |> List.map ~f:(fun e -> e |> Event.sexp_of_t |> Sexp.to_string_mach |> I.string)
      |> I.vcat
      |> I.hsnap 40
      |> outline)
  in
  let height = List.length unhandled + 2 in
  let width = 40 in
  { image; height; width; cursor = None }, append_unhandled
;;

module Textbox = struct
  module Action = struct
    type t =
      | Append of char
      | Backspace
    [@@deriving sexp_of]
  end

  let textbox =
    let%sub textbox =
      Bonsai.state_machine0
        [%here]
        (module String)
        (module Action)
        ~default_model:""
        ~apply_action:
          (fun ~inject:_ ~schedule_event:_ m -> function
            | Append c -> m ^ Char.to_string c
            | Backspace -> String.drop_suffix m 1)
    in
    let%arr text, inject = textbox in
    let view =
      { width = String.length text + 5
      ; height = 3
      ; image = lazy (outline (I.string (" " ^ text ^ "  ")))
      ; cursor = Some (String.length text + 2, 1)
      }
    in
    view, text, inject
  ;;
end

let windows_2 = function
  | [] -> []
  | [ x ] -> [ None, x ]
  | l ->
    let len = List.length l in
    let optioned = List.map l ~f:Option.some in
    let leading = None :: List.take optioned (len - 1) in
    List.zip_exn leading l
;;

let render_many ~children ~focus =
  let width =
    Map.fold children ~init:0 ~f:(fun ~key:_ ~data acc -> Int.max acc data.width)
  in
  let height = Map.length children in
  let decorate (key, data) =
    ( (if [%equal: Blocklist.Id.t option] focus (Some key)
      then `Decorated
      else `Undecorated)
    , Lazy.force data.image )
  in
  let image =
    lazy
      (children
      |> Map.to_alist
      |> List.map ~f:decorate
      |> windows_2
      |> List.map ~f:(function
             | None, (`Undecorated, i) | Some (`Undecorated, _), (`Undecorated, i) ->
               I.void 1 1 <-> (I.void 2 1 <|> i)
             | _, (`Decorated, i) -> bracket (I.void 1 1 <|> i)
             | Some (`Decorated, _), (_, i) -> I.void 2 1 <|> i
             | _, (_, i) -> I.void 1 1 <|> i)
      |> I.vcat)
  in
  let cursor = None in
  { width; height; image; cursor }
;;

let textlist =
  let%sub blocklist = Blocklist.component (module String) in
  (*
  let%sub () =
    Bonsai.Edge.lifecycle
      ()
      ~on_activate:
        (let%map _, complete = blocklist in
         let { Blocklist.Actions.set; _ } = complete (fun _ -> assert false) in
         Blocklist.effect_of_maybe_do_thing (set [] [ "hello"; "there\nthis"; "world!" ]))
  in
  *)
  let%arr { children; focus }, actions = blocklist in
  let width =
    Map.fold children ~init:0 ~f:(fun ~key:_ ~data acc ->
        Int.max acc (String.length data))
  in
  let height = Map.length children in
  let decorate (key, data) =
    let paragraphize s = s |> String.split_lines |> List.map ~f:I.string |> I.vcat in
    ( (if [%equal: Blocklist.Id.t option] focus (Some key)
      then `Decorated
      else `Undecorated)
    , paragraphize data )
  in
  let image =
    lazy
      (children
      |> Map.to_alist
      |> List.map ~f:decorate
      |> windows_2
      |> List.map ~f:(function
             | None, (`Undecorated, i) | Some (`Undecorated, _), (`Undecorated, i) ->
               I.void 1 1 <-> (I.void 2 1 <|> i)
             | _, (`Decorated, i) -> bracket (I.void 1 1 <|> i)
             | Some (`Decorated, _), (_, i) -> I.void 2 1 <|> i
             | _, (_, i) -> I.void 1 1 <|> i)
      |> I.vcat
      |> fun i -> I.string "--start--" <-> i <-> I.string "--end--")
  in
  let cursor = None in
  { width; height; image; cursor }, actions
;;

module Kind = struct
  type t =
    | Blocklist
    | String
  [@@deriving equal, sexp]
end

module Out = struct
  type t =
    | Blocklist of t Map.M(Blocklist.Id).t
    | String of string
  [@@deriving sexp]
end

let rec blklick ~kind =
  match%sub kind with
  | Kind.Blocklist ->
    let%sub { children; focus }, actions = Blocklist.component (module Kind) in
    let%sub children =
      Bonsai.assoc
        (module Blocklist.Id)
        children
        ~f:(fun _key kind -> Bonsai.lazy_ (lazy (blklick ~kind)))
    in
    let%arr children = children
    and actions = actions
    and focus = focus in
    let children_views = Map.map children ~f:Tuple3.get1 in
    let children_results = Map.map children ~f:Tuple3.get2 in
    let children_actions = Map.map children ~f:Tuple3.get3 in
    ( render_many ~children:children_views ~focus
    , Out.Blocklist children_results
    , Blocklist.Actions.from_map actions children_actions )
  | String ->
    Bonsai.const
    @@
    let image = lazy (I.string "foo") in
    let view = { width = 3; height = 1; image; cursor = None } in
    view, Out.String "foo", Blocklist.Actions.always_continue
;;

let blklick =
  let%sub blklick = blklick ~kind:(Bonsai.Value.return Kind.Blocklist) in
  let%sub _views, results, actions = return blklick in
  let%sub get_results = Bonsai_extra.yoink results in
  let%sub get_actions = Bonsai_extra.yoink actions in
  let%sub () =
    Bonsai.Edge.lifecycle
      ()
      ~on_activate:
        (let%map { set; _ } = actions
         and get_results = get_results
         and get_actions = get_actions in
         let%bind.Ui_effect () =
           Blocklist.effect_of_maybe_do_thing (set [] [ String; Blocklist; String ])
         in
         let%bind.Ui_effect { set; _ } = get_actions in
         match%bind.Ui_effect get_results with
         | Out.String _ -> assert false
         | Out.Blocklist map ->
           map
           |> Map.to_alist
           |> List.map ~f:(function
                  | k, Blocklist _ ->
                    Blocklist.effect_of_maybe_do_thing (set [ k ] [ String; String ])
                  | _ -> Ui_effect.Ignore)
           |> Ui_effect.Many)
  in
  return blklick
;;

let component size =
  let%sub unhandled = unhandled in
  let%sub textbox = Textbox.textbox in
  let%sub textlist = textlist in
  let%sub blklick = blklick in
  let%arr _width, _height = size
  and textbox_view, _text, inject_textbox = textbox
  and unhandled_display, append_unhandled = unhandled
  and textlist, _ = textlist
  and blklick, _results, actions' = blklick in
  let _view = vcat (vcat (vcat unhandled_display textbox_view) textlist) blklick in
  let _view = textlist in
  let view = blklick in
  let handle_key = function
    | `Key (`ASCII 'j', []) -> Blocklist.effect_of_maybe_do_thing actions'.focus_down
    | `Key (`ASCII 'k', []) -> Blocklist.effect_of_maybe_do_thing actions'.focus_up
    | `Key (`ASCII 'o', []) ->
      Blocklist.effect_of_maybe_do_thing (actions'.insert_under_cursor String)
    | `Key (`ASCII c, []) -> inject_textbox (Append c)
    | `Key (`Backspace, _) -> inject_textbox Backspace
    | other -> append_unhandled other
  in
  view, handle_key
;;

let () =
  let handle = Handle.create component in
  Handle.loop handle
;;
