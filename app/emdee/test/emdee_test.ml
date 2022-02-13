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
aaaa

# b
bbbb

#### c
cccccc

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

    let%expect_test _ =
      {|
## a
# b
## c
      |} |> parse |> Md.Toc.f |> unparse |> print_endline;
      [%expect {|
        -   a
        -   b
            -   c |}]
    ;;

    let%expect_test _ =
      {|
# x
### a
## b
### c
      |} |> parse |> Md.Toc.f |> unparse |> print_endline;
      [%expect
        {|
        -   x
            -   a
            -   b
                -   c |}]
    ;;
  end)
;;

let%test_module "group by header" =
  (module struct
    let%expect_test _ =
      {|
test test test

# a
sdsdf

### c
cccccccc

# b
bbbbbbb

## c
cccccccc

# d 
ddddddd
|}
      |> parse
      |> Md.Grouped_by_header.f
      |> [%sexp_of: Md.Grouped_by_header.t list]
      |> print_s;
      [%expect
        {|
        ((Block (Para (Str test) Space (Str test) Space (Str test)))
         (Header (title ((Str a))) (level 1) (attr (a () ()))
          (children
           ((Block (Para (Str sdsdf)))
            (Header (title ((Str c))) (level 3) (attr (c () ()))
             (children ((Block (Para (Str cccccccc)))))))))
         (Header (title ((Str b))) (level 1) (attr (b () ()))
          (children
           ((Block (Para (Str bbbbbbb)))
            (Header (title ((Str c))) (level 2) (attr (c-1 () ()))
             (children ((Block (Para (Str cccccccc)))))))))
         (Header (title ((Str d))) (level 1) (attr (d () ()))
          (children ((Block (Para (Str ddddddd))))))) |}]
    ;;

    let%expect_test _ =
      let doc =
        {|
test test test

# a
sdsdf

### c
cccccccc

# b
bbbbbbb

## c
cccccccc

# d 
ddddddd
|}
        |> parse
      in
      doc
      |> Md.Grouped_by_header.f
      |> Md.Grouped_by_header.un_f
      |> Pandoc.with_top_level_blocks doc
      |> unparse
      |> print_endline;
      [%expect
        {|
        test test test

        # a

        sdsdf

        ### c

        cccccccc

        # b

        bbbbbbb

        ## c

        cccccccc

        # d

        ddddddd |}]
    ;;
  end)
;;
