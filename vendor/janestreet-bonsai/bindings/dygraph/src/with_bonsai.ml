open Core
open Import

(* This top-level side-effect installs the CSS for dygraphs.
   We need it in this file because if the side-effect lives
   in an otherwise-empty file, or in a file that only contains
   module aliases, then the dead-code-eliminator will remove
   the file (including your side-effect).

   By putting it here, anyone that uses the With_bonsai module
   will force the side-effect to be evaluated. *)
let () = Css.install_css ()

module Legend_model = struct
  type t = { visibility : bool list } [@@deriving equal, fields]
end

let id = Type_equal.Id.create ~name:"dygraph" [%sexp_of: opaque]

let widget ?with_graph ?on_zoom data options =
  (* This function tells the graph to resize itself to fit its contents.  This is
     required because at the point when the graph is created, the element [el]
     (created down below in [init]) hasn't yet been attached to the Dom, so it
     initially detects that it's size should be 0x0.  When requestAnimationFrame
     completes, according to the semantics of Bonsai_web, our graph has been
     successfully inserted into the Dom, so we can trigger another resize and
     it'll compute the correct size. *)
  let resize_when_inserted_into_the_dom graph _time = Graph.resize graph in
  let override_zoom_callback ~graph options =
    match on_zoom with
    | None         -> options
    | Some on_zoom ->
      let zoomCallback =
        let caller's_zoom_callback = Options.zoomCallback options in
        fun ~xmin ~xmax ~yRanges ->
          Option.iter caller's_zoom_callback ~f:(fun f -> f ~xmin ~xmax ~yRanges);
          let is_zoomed = Graph.isZoomed graph in
          Vdom.Effect.Expert.handle_non_dom_event_exn (on_zoom ~is_zoomed)
      in
      let our_options = Options.create ~zoomCallback () in
      Options.merge options ~prefer:our_options
  in
  Vdom.Node.widget
    ()
    ~id
    ~destroy:(fun (_, _, _, graph, animation_id) _el ->
      (* Free resources allocated by the graph *)
      Graph.destroy graph;
      (* If for some reason the animation-frame never fired and we're already
         being removed, go ahead and cancel the callback. *)
      Dom_html.window##cancelAnimationFrame animation_id)
    ~init:(fun () ->
      let el = Dom_html.createDiv Dom_html.document in
      let graph = Graph.create el data options      in
      let () =
        let options       = override_zoom_callback ~graph options        in
        let updateOptions = Update_options.create ~options ?data:None () in
        Graph.updateOptions graph updateOptions
      in
      Option.iter with_graph ~f:(fun with_graph -> with_graph graph);
      let animation_id =
        Dom_html.window##requestAnimationFrame
          (Js.wrap_callback (resize_when_inserted_into_the_dom graph))
      in
      (data, options, on_zoom, graph, animation_id), el)
    ~update:(fun (old_data, old_options, old_on_zoom, graph, animation_id) el ->
      let () =
        let data = Option.some_if (not (phys_equal old_data data)) data in
        let options =
          match phys_equal old_options options, phys_equal old_on_zoom on_zoom with
          | true, true -> None
          | _          -> Some (override_zoom_callback ~graph options)
        in
        match data, options with
        | None, None -> ()
        | _          ->
          let updateOptions = Update_options.create ?options ?data () in
          Graph.updateOptions graph updateOptions
      in
      (data, options, on_zoom, graph, animation_id), el)
;;

let create_graph ?with_graph ?on_zoom data options =
  let on_zoom =
    match on_zoom with
    | None         -> Bonsai.Value.return None
    | Some on_zoom -> Bonsai.Value.map on_zoom ~f:Option.some
  in
  return
    (let%map.Bonsai data = data
     and options = options
     and on_zoom = on_zoom in
     widget ?with_graph ?on_zoom data options)
;;

let create_options ~x_label ~y_labels ~visibility ~legendFormatter =
  let labels        = x_label :: y_labels                  in
  (* We create an element but never actually put it anywhere. *)
  let hidden_legend = Dom_html.createDiv Dom_html.document in
  Options.create
    ()
    ~xlabel:x_label
    ~labels
    ~visibility
    ~legend:`always (* If [legend:`never], then [legendFormatter] doesn't fire. *)
    ~labelsDiv_el:hidden_legend
    ~legendFormatter
;;

let create_default_legend ~x_label ~per_series_info =
  let%sub model, view, inject = Default_legend.create ~x_label ~per_series_info in
  (* project out visibility *)
  let model =
    let%map model = model in
    { Legend_model.visibility =
        List.map model.series ~f:Default_legend.Model.Series.is_visible
    }
  in
  let inject =
    let%map inject = inject in
    fun data -> inject Default_legend.Action.(From_graph data)
  in
  return (Bonsai.Value.map3 model view inject ~f:Tuple3.create)
;;

let format_legend inject_legend_data options data =
  let caller's_legend_formatter = Option.bind options ~f:Options.legendFormatter in
  (* we call the legendFormatter option set on [options] in case the caller is relying
     on it for side effects. *)
  Option.iter caller's_legend_formatter ~f:(fun f -> ignore (f data : string));
  Vdom.Effect.Expert.handle_non_dom_event_exn (inject_legend_data data);
  (* we are pointing the legend managed by dygraph to a hidden div (see
     [create_options]) so this should be invisible. *)
  "this should not be visible"
;;

let build_options options visibility legendFormatter x_label y_labels =
  let our_options = create_options ~x_label ~y_labels ~visibility ~legendFormatter in
  match options with
  | None         -> our_options
  | Some options -> Options.merge options ~prefer:our_options
;;

let visibility ~legend_model ~num_series =
  let visibility =
    let%map.Bonsai visibility_from_legend = legend_model >>| Legend_model.visibility
    and num_series                        = num_series in
    let visibility_len = List.length visibility_from_legend in
    if visibility_len < num_series
    then (
      (* Dygraphs has a bug where it will throw an error if the length of [visibility] is
         ever less than the number of series in the data.  To work around this, we pad
         [visibility] with trues. *)
      let padding = List.init (num_series - visibility_len) ~f:(Fn.const true) in
      visibility_from_legend @ padding)
    else visibility_from_legend
  in
  visibility |> Bonsai.Value.cutoff ~equal:[%equal: bool list]
;;

let create
      ~key
      ~x_label
      ~per_series_info
      ?custom_legend
      ?options
      ?with_graph
      ?on_zoom
      ~data
      ()
  =
  let options =
    Option.value_map
      options
      ~default:(Bonsai.Value.return None)
      ~f:(Bonsai.Value.map ~f:Option.some)
  in
  let%sub legend =
    match custom_legend with
    | Some legend -> Bonsai.read legend
    | None        -> create_default_legend ~x_label ~per_series_info
  in
  let%pattern_bind legend_model, legend_view, inject_legend_data = legend in
  let inject_legend_data = Bonsai.Value.cutoff inject_legend_data ~equal:phys_equal in
  let y_labels =
    Bonsai.Value.map per_series_info ~f:(List.map ~f:Per_series_info.label)
  in
  let visibility =
    let num_series = Bonsai.Value.map per_series_info ~f:List.length in
    visibility ~legend_model ~num_series
  in
  let legendFormatter = Bonsai.Value.map2 inject_legend_data options ~f:format_legend in
  let options =
    Bonsai.Value.map5 options visibility legendFormatter x_label y_labels ~f:build_options
  in
  let%sub graph = create_graph ?with_graph ?on_zoom data options in
  return
  @@ let%map graph = graph
  and legend_view = legend_view
  and key         = key in
  Vdom.Node.div
    ~key
    ~attr:
      (Vdom.Attr.many_without_merge
         [ Vdom.Attr.class_ "dygraph"; Vdom.Attr.style (Css_gen.flex_container ()) ])
    [ graph; legend_view ]
;;
