module Caml_unix = Unix
open! Core
module Pandoc = Pandoc_ast_jane
module Md = Emdee

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

let parse c = c |> ast_of_markdown |> Pandoc.of_pandoc_ast_string
let unparse c = c |> Pandoc.to_pandoc_ast_string |> markdown_of_ast
let files l = List.map l ~f:(fun (f, c) -> f, parse c)

let%test_module "cat" =
  (module struct
    let%expect_test "cat nothing" =
      files [] |> Md.Cat.f |> print_endline;
      [%expect {| |}]
    ;;

    let%expect_test "basic cat" =
      files [ "a.md", {| hello |}; "b.md", {| world |} ] |> Md.Cat.f |> print_endline;
      [%expect {|
# a.md

hello

# b.md

world |}]
    ;;

    let%expect_test "increase headings " =
      files [ "a.md", {| 
# hello 
|}; "b.md", {| 
# world 
|} ]
      |> Md.Cat.f
      |> print_endline;
      [%expect {|
# a.md

## hello

# b.md

## world |}]
    ;;
  end)
;;

let%test_module "toc" =
  (module struct
    let%expect_test "basic toc" =
      {|

# a
# b
#### c
# d
## d
### c
#### b
### b
## b
# x
      |}
      |> parse
      |> Md.Toc.f
      |> unparse
      |> print_endline;
      [%expect
        {|
        -   a
        -   b
            -   c
        -   d
            -   d
                -   c
                    -   b
                -   b
            -   b
        -   x |}]
    ;;
  end)
;;
