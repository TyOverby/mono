open! Core
module View = View_p

let%expect_test "default button" =
  let theme = View.Expert.default_theme in
  let button = View.button theme "hello" ~on_click:() in
  print_endline button;
  [%expect {| button: hello, foreground: black, background: white |}]
;;

let%expect_test "overridden button" =
  let theme =
    View.Expert.override View.Expert.default_theme ~f:(fun (module T) ->
        (module struct
          class t =
            object
              inherit T.t
              method! foreground_color = "blue"
            end
        end))
  in
  let button = View.button theme "hello" ~on_click:() in
  print_endline button;
  [%expect {| button: hello, foreground: blue, background: white |}]
;;
