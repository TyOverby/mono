module Caml_unix = Unix
open! Core
module Pandoc = Pandoc_ast_jane

module Util = struct
  let pandoc_path = "pandoc"

  let run prog args =
    let command = sprintf "%s %s" prog (String.concat args ~sep:" ") in
    let stdout = Caml_unix.open_process_in command in
    In_channel.input_all stdout
  ;;

  let communicate_with_pandoc ~data ~args =
    let tmpfile = String.strip (run "mktemp" []) in
    Out_channel.write_all tmpfile ~data;
    let args = tmpfile :: args in
    run "pandoc" args
  ;;

  let ast_of_markdown markdown =
    communicate_with_pandoc ~data:markdown ~args:[ "-f"; "markdown"; "-t"; "json" ]
  ;;

  let markdown_of_ast markdown =
    communicate_with_pandoc ~data:markdown ~args:[ "-f"; "json"; "-t"; "markdown"; "-s" ]
  ;;

  let read_md filepath = In_channel.read_all filepath |> ast_of_markdown
end

module Cat = struct
  let f (l : (string * Pandoc.t) list) =
    let content =
      List.concat_map l ~f:(fun (filename, ast) ->
          let header =
            let attrs = Pandoc.Attrs.make ~id:filename ~classes:[] ~attributes:[] in
            Pandoc.Block.Header (1, attrs, [ Pandoc.Inline.Str filename ])
          in
          let body =
            Pandoc.map ast ~inline:Fn.id ~block:(function
                | Header (l, attr, content) -> Header (l + 1, attr, content)
                | other -> other)
          in
          header :: Pandoc.top_level_blocks body)
    in
    match l with
    | [] -> ""
    | (_, first) :: _ ->
      Pandoc.with_top_level_blocks first content
      |> Pandoc.to_pandoc_ast_string
      |> Util.markdown_of_ast
  ;;
end

module Toc = struct
  let list l = Pandoc.Block.BulletList l

  let list_item ~children c =
    [ Pandoc.Block.Plain c; list children ]
    |> Pandoc.List_item.of_blocks
    |> Pandoc.List_item.conceal
  ;;

  let headers acc = function
    | Pandoc.Block.Header (i, _, c) -> (i, c, []) :: acc
    | _ -> acc
  ;;

  let rec loop = function
    | [] -> [], []
    | [ (_, c, children) ] -> [ list_item c ~children ], []
    | (a, c, children) :: ((b, _, _) :: _ as rest) when a < b ->
      let lower, rest = loop rest in
      loop ((a, c, children @ lower) :: rest)
    | (a, c, children) :: ((b, _, _) :: _ as rest) when a > b ->
      [ list_item c ~children ], rest
    | (_, c, children) :: rest ->
      let same, higher = loop rest in
      list_item c ~children :: same, higher
  ;;

  let f (ast : Pandoc.t) =
    ast
    |> Pandoc.fold ~init:[] ~block:headers
    |> List.rev
    |> loop
    |> (fun (l, _) -> [ list l ])
    |> Pandoc.with_top_level_blocks ast
  ;;
end
