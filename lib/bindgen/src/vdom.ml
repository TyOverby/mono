open! Core

module Float = struct
  include Float

  let to_string f = sprintf "%.3f" f
end

module Attr = struct
  type t =
    { classes : string list
    ; transforms : string list
    ; other : string
    }

  let empty = { classes = []; other = ""; transforms = [] }
  let stringlike key data = { empty with other = key ^ "=\"" ^ data ^ "\"" }
  let floatlike key data = stringlike key (Float.to_string data)

  let combine a b =
    { transforms = a.transforms @ b.transforms
    ; classes = a.classes @ b.classes
    ; other = a.other ^ " " ^ b.other
    }
  ;;

  let id = stringlike "id"
  let classes l = { classes = l; other = ""; transforms = [] }
  let class_ c = classes [ c ]
  let data k = stringlike ("data-" ^ k)
  let many l = l |> List.reduce ~f:combine |> Option.value ~default:empty
  let transform s = { empty with transforms = [ s ] }

  let to_string { classes; transforms; other } =
    let classes =
      match classes with
      | [] -> ""
      | _ -> " class=\"" ^ String.concat classes ~sep:" " ^ "\""
    in
    let transforms =
      match transforms with
      | [] -> ""
      | _ -> " transform=\"" ^ String.concat transforms ~sep:" " ^ "\""
    in
    other ^ classes ^ transforms
  ;;
end

module Node = struct
  type t = string list list
  type creator = ?attr:Attr.t -> t list -> t

  let none = []

  let create : string -> creator =
   fun tag ?(attr = Attr.empty) children ->
    let children = children |> List.join |> List.map ~f:(fun xs -> "  " :: xs) in
    List.concat
      [ [ [ [%string "<%{tag} %{Attr.to_string attr}>"] ] ]
      ; children
      ; [ [ [%string "</%{tag}>"] ] ]
      ]
 ;;

  let text s = [ [ s ] ]
  let svg = create "svg"
  let div = create "div"
  let span = create "span"

  let to_string (t : t) : string =
    t |> List.map ~f:String.concat |> String.concat ~sep:"\n"
  ;;
end

module Svg = struct
  module Vec2 = struct
    type t =
      { x : float
      ; y : float
      }
    [@@deriving sexp]
  end

  module Path = struct
    type t =
      | Move of Vec2.t
      | Bezier_to of
          { start_handle : Vec2.t
          ; end_handle : Vec2.t
          ; dest : Vec2.t
          }
      | Arc_to of
          { radius : float
          ; large_arc : bool
          ; sweep : bool
          ; dest : Vec2.t
          }
    [@@deriving sexp]
  end

  module Stroke = struct
    type t =
      { width : float
      ; color : string
      ; linecap : [ `Round ]
      }

    let create ?(width = 1.0) ?(color = "black") ?(linecap = `Round) () =
      { width; color; linecap }
    ;;

    let to_attrs { width; color; linecap } =
      let linecap =
        match linecap with
        | `Round -> "round"
      in
      Attr.many
        [ Attr.stringlike "stroke-width" [%string "%{width#Float}px"]
        ; Attr.stringlike "stroke" color
        ; Attr.stringlike "stroke-linecap" linecap
        ]
    ;;
  end

  let path
      ?(attr = Attr.empty)
      ?(stroke = Stroke.create ())
      ?(fill = "black")
      (path : Path.t list)
    =
    let d =
      path
      |> List.map ~f:(function
             | Move { x; y } -> [%string "M %{x#Float} %{y#Float}"]
             | Bezier_to
                 { start_handle = { x = f_h_x; y = f_h_y }
                 ; end_handle = { x = t_h_x; y = t_h_y }
                 ; dest = { x; y }
                 } ->
               [%string
                 "C %{f_h_x#Float} %{f_h_y#Float}, %{t_h_x#Float} %{t_h_y#Float}, \
                  %{x#Float} %{y#Float}"]
             | Arc_to { radius; large_arc; sweep; dest = { x; y } } ->
               let large_arc = if large_arc then 1 else 0 in
               let sweep = if sweep then 1 else 0 in
               [%string
                 "A %{radius#Float} %{radius#Float} 0 %{large_arc#Int} %{sweep#Int} \
                  %{x#Float} %{y#Float}"])
      |> String.concat ~sep:" "
    in
    let attr =
      Attr.many
        [ attr
        ; Attr.stringlike "d" d
        ; Stroke.to_attrs stroke
        ; Attr.stringlike "fill" fill
        ]
    in
    Node.create ~attr "path" []
  ;;

  let line
      ?(attr = Attr.empty)
      ?(stroke = Stroke.create ())
      { Vec2.x = x1; y = y1 }
      { Vec2.x = x2; y = y2 }
    =
    let attr =
      Attr.many
        [ attr
        ; Attr.floatlike "x1" x1
        ; Attr.floatlike "y1" y1
        ; Attr.floatlike "x2" x2
        ; Attr.floatlike "y2" y2
        ; Stroke.to_attrs stroke
        ]
    in
    Node.create ~attr "line" []
  ;;

  let rect
      ?(attr = Attr.empty)
      ?(stroke = Stroke.create ())
      ?(fill = "black")
      { Vec2.x = x1; y = y1 }
      { Vec2.x = x2; y = y2 }
    =
    let attr =
      Attr.many
        [ attr
        ; Attr.floatlike "x" x1
        ; Attr.floatlike "y" y1
        ; Attr.floatlike "width" (x2 -. x1)
        ; Attr.floatlike "height" (y2 -. y1)
        ; Stroke.to_attrs stroke
        ; Attr.stringlike "fill" fill
        ]
    in
    Node.create ~attr "rect" []
  ;;

  let circle
      ?(attr = Attr.empty)
      ?(stroke = Stroke.create ())
      ?(fill = "black")
      { Vec2.x = x1; y = y1 }
      ~r
    =
    let attr =
      Attr.many
        [ attr
        ; Attr.floatlike "cx" x1
        ; Attr.floatlike "cy" y1
        ; Attr.floatlike "r" r
        ; Stroke.to_attrs stroke
        ; Attr.stringlike "fill" fill
        ]
    in
    Node.create ~attr "circle" []
  ;;

  let group ?(attr = Attr.empty) ?(transforms = []) children =
    let transforms = List.map transforms ~f:Attr.transform in
    let attr = Attr.many (attr :: transforms) in
    Node.create "g" ~attr children
  ;;
end
