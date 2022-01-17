open! Core
open! Types
module N = Vdom.Node
module A = Vdom.Attr

let box t = function
  | [] -> Vdom.Node.none
  | [ child ] -> child
  | children -> N.div ~attr:(A.class_ t) children
;;

let vbox, hbox = box "hbox", box "vbox"

let conn kind name =
  match kind with
  | `Provide -> A.data "src-name" (Name.to_string name)
  | `Consume -> A.class_ ("dest-class-" ^ Name.to_string name)
;;

let rec value_to_html ~point_to (me : Value.t) =
  match me with
  | Singleton ->
    [ [ N.div
          ~attr:
            (A.many
               [ A.classes [ "value" ]
               ; A.data "kind" "singleton"
               ; conn `Provide point_to
               ])
          []
      ]
    ]
  | Redirect name ->
    [ [ N.div
          ~attr:
            (A.many
               [ A.classes [ "value" ]
               ; A.data "kind" "redirect"
               ; conn `Provide point_to
               ; conn `Consume name
               ])
          []
      ]
    ]
  | Named name ->
    [ [ N.div
          ~attr:
            (A.many
               [ A.classes [ "value" ]
               ; A.data "kind" "named"
               ; conn `Provide point_to
               ; conn `Consume name
               ])
          []
      ]
    ]
  | Mapn [] -> failwith "mapn with an empty list?"
  | Mapn children ->
    let me = Name.create () in
    let children, attrs =
      List.fold
        children
        ~init:([], A.many [])
        ~f:(fun (children, attr) child ->
          match child with
          | Named name -> children, A.many [ attr; conn `Consume name ]
          | other -> other :: children, attr)
    in
    let children =
      children
      |> List.map ~f:(value_to_html ~point_to:me)
      |> List.reduce_balanced ~f:(fun a b ->
             let abs, rest = List.zip_with_remainder a b in
             let abs : N.t list list = List.map abs ~f:(fun (a, b) -> a @ b) in
             match rest with
             | None -> abs
             | Some (First a) -> abs @ a
             | Some (Second b) -> abs @ b)
      |> Option.value ~default:[]
    in
    [ N.div
        ~attr:
          (A.many
             [ A.classes [ "value" ]
             ; A.data "kind" "mapn"
             ; conn `Consume me
             ; conn `Provide point_to
             ; attrs
             ])
        []
    ]
    :: children
;;

let value_to_html ~point_to value =
  value |> value_to_html ~point_to |> List.rev_map ~f:hbox |> vbox
;;

let rec computation_to_html ~point_to (c : Computation.t) =
  match c with
  | { kind = Value v; free_variables = _ } -> value_to_html ~point_to v
  | { kind = Bindings { bindings = []; last_body }; free_variables = _ } ->
    computation_to_html ~point_to last_body
  | { kind = Bindings { bindings; last_body }; free_variables = _ } ->
    Transform.organize_bindings bindings ~last_body ~point_to
    |> List.map ~f:(function
           | [] -> Vdom.Node.none
           | [ { as_; bound } ] -> computation_to_html ~point_to:as_ bound
           | row ->
             row
             |> List.map ~f:(function
                    | { Binding.as_; bound = { kind = Value v; _ } } ->
                      value_to_html v ~point_to:as_
                    | { Binding.as_; bound } ->
                      N.div
                        ~attr:(A.class_ "sub")
                        [ computation_to_html ~point_to:as_ bound ])
             |> hbox)
    |> vbox
  | _ -> assert false
;;

let computation_to_html c =
  let out = Name.create () in
  computation_to_html c ~point_to:out
;;

let to_html t =
  N.div ~attr:(A.class_ "map") [ N.svg []; N.div [ computation_to_html t ] ]
  |> N.to_string
;;
