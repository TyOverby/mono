open! Core
open! Bonsai_web
open! Bonsai_web_test
open  Bonsai.Let_syntax

let shared_computation =
  Bonsai_web_ui_typeahead.Typeahead.create
    (module Data)
    ~all_options:(Value.return Data.all)
    ~placeholder:"Select a value"
    ~to_string:(Bonsai.Value.return Data.to_string)
;;

let view_computation =
  let%sub _, view, _ = shared_computation in
  return view
;;

let view_and_inject_computation =
  let%sub _, view, inject = shared_computation in
  return (Value.both view inject)
;;

let view_and_result_computation =
  let%sub result, view, _ = shared_computation in
  return (Value.both view result)
;;

let%expect_test "Initial typeahead state" =
  let handle = Handle.create (Result_spec.vdom Fn.id) view_computation in
  Handle.show handle;
  [%expect
    {|
  <div>
    <input type="text"
           list="bonsai_path_replaced_in_test"
           placeholder="Select a value"
           value=""
           #value=""
           onchange> </input>
    <datalist id="bonsai_path_replaced_in_test">
      <option value="Option A"> Option A </option>
      <option value="Option B"> Option B </option>
      <option value="Option C"> Option C </option>
    </datalist>
  </div>
  |}]
;;

let%expect_test "Change typeahead contents" =
  let handle = Handle.create (Result_spec.vdom Fn.id) view_computation in
  Handle.show handle;
  let before = [%expect.output] in
  Handle.input_text
    handle
    ~get_vdom:Fn.id
    ~selector:"input"
    ~text:(Data.to_string Data.Option_C);
  Handle.show handle;
  let after = [%expect.output] in
  Expect_test_patdiff.print_patdiff before after;
  (* Expected change: input value should change. *)
  [%expect
    {|
-1,13 +1,13
  <div>
    <input type="text"
           list="bonsai_path_replaced_in_test"
           placeholder="Select a value"
-|         value=""
+|         value="Option C"
-|         #value=""
+|         #value="Option C"
           onchange> </input>
    <datalist id="bonsai_path_replaced_in_test">
      <option value="Option A"> Option A </option>
      <option value="Option B"> Option B </option>
      <option value="Option C"> Option C </option>
    </datalist>
  </div> |}]
;;

let%expect_test "use setter" =
  let handle =
    Handle.create
      (module struct
        type incoming = Data.t option
        type t        = Vdom.Node.t * (Data.t option -> unit Ui_effect.t)

        let view (vdom, _) =
          let module V = (val Result_spec.vdom Fn.id) in
          V.view vdom
        ;;

        let incoming (_, inject) = inject
      end)
      view_and_inject_computation
  in
  Handle.show handle;
  let _before = [%expect.output] in
  Handle.do_actions handle [ Some Data.Option_A ];
  Handle.show_diff  handle;
  [%expect
    {|
      <div>
        <input type="text"
               list="bonsai_path_replaced_in_test"
               placeholder="Select a value"
    -|         value=""
    +|         value="Option A"
    -|         #value=""
    +|         #value="Option A"
               onchange> </input>
        <datalist id="bonsai_path_replaced_in_test">
          <option value="Option A"> Option A </option>
          <option value="Option B"> Option B </option>
          <option value="Option C"> Option C </option>
        </datalist>
      </div>
|}];
  Handle.do_actions handle [ None ];
  Handle.show_diff  handle;
  [%expect
    {|
      <div>
        <input type="text"
               list="bonsai_path_replaced_in_test"
               placeholder="Select a value"
    -|         value="Option A"
    +|         value=""
    -|         #value="Option A"
    +|         #value=""
               onchange> </input>
        <datalist id="bonsai_path_replaced_in_test">
          <option value="Option A"> Option A </option>
          <option value="Option B"> Option B </option>
          <option value="Option C"> Option C </option>
        </datalist>
      </div>
 |}]
;;

let%expect_test "Select element using partial input" =
  let handle =
    Handle.create
      (Result_spec.sexp
         (module struct
           type t = Vdom.Node.t * Data.t option

           let sexp_of_t (_view, result) = [%sexp_of: Data.t option] result
         end))
      view_and_result_computation
  in
  Handle.show handle;
  [%expect {| () |}];
  (* "O" is not unique, nothing happens *)
  Handle.input_text handle ~get_vdom:Tuple2.get1 ~selector:"input" ~text:"O";
  Handle.show       handle;
  [%expect {| () |}];
  (* 'C' is unique, use it! *)
  Handle.input_text handle ~get_vdom:Tuple2.get1 ~selector:"input" ~text:"C";
  Handle.show       handle;
  [%expect {| (Option_C) |}]
;;
