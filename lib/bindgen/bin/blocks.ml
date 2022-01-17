open! Core
module N = Bindgen.Vdom.Node
module A = Bindgen.Vdom.Attr
module S = Bindgen.Vdom.Svg

module V = struct
  type t = S.Vec2.t =
    { x : float
    ; y : float
    }
  [@@deriving sexp]

  let length { x; y } = Float.sqrt ((x *. x) +. (y *. y))

  let norm { x; y } =
    let l = Float.sqrt ((x *. x) +. (y *. y)) in
    { x = x /. l; y = y /. l }
  ;;

  let perp { x; y } = [ { x = y; y = -.x }; { x = -.y; y = x } ]
  let scale { x; y } s = { x = x *. s; y = y *. s }

  let midpoint { x = x1; y = y1 } { x = x2; y = y2 } =
    { x = (x1 +. x2) /. 2.0; y = (y1 +. y2) /. 2.0 }
  ;;

  let sub { x = x1; y = y1 } { x = x2; y = y2 } = { x = x1 -. x2; y = y1 -. y2 }
  let add { x = x1; y = y1 } { x = x2; y = y2 } = { x = x1 +. x2; y = y1 +. y2 }
  let cross { x = x1; y = y1 } { x = x2; y = y2 } = (x1 *. y2) -. (y1 *. x2)
  let angle { x; y } = Float.atan2 y x
  let of_angle theta = { x = Float.cos theta; y = Float.sin theta }
  let swap { x; y } = { x = y; y = -.x }

  module Infix = struct
    let ( + ) = add
    let ( - ) = sub
    let ( * ) = cross
    let ( *. ) = scale
  end
end

let line_line =
  let open V in
  let open Float in
  fun ({ x = x1; y = y1 }, { x = x2; y = y2 }) ({ x = x3; y = y3 }, { x = x4; y = y4 }) ->
    let ua =
      (((x4 - x3) * (y1 - y3)) - ((y4 - y3) * (x1 - x3)))
      / (((y4 - y3) * (x2 - x1)) - ((x4 - x3) * (y2 - y1)))
    in
    let ub =
      (((x2 - x1) * (y1 - y3)) - ((y2 - y1) * (x1 - x3)))
      / (((y4 - y3) * (x2 - x1)) - ((x4 - x3) * (y2 - y1)))
    in
    if ua >= 0.0 && ua <= 1.0 && ub >= 0.0 && ub <= 1.0
    then (
      let x = x1 + (ua * (x2 - x1)) in
      let y = y1 + (ua * (y2 - y1)) in
      Some { x; y })
    else None
;;

let line_box (box_min, box_max) line =
  let open V in
  let line_line a b = Option.map (line_line (a, b) line) ~f:(fun r -> r, (a, b)) in
  let c1 = box_min in
  let c2 = box_max in
  let c3 = { x = box_min.x; y = box_max.y } in
  let c4 = { x = box_max.x; y = box_min.y } in
  Option.first_some
    (Option.first_some (line_line c1 c3) (line_line c1 c4))
    (Option.first_some (line_line c3 c2) (line_line c2 c4))
;;

let corners (box_min, box_max) point =
  let open V in
  let open Float in
  let c1 = box_min in
  let c2 = box_max in
  let c3 = { x = box_min.x; y = box_max.y } in
  let c4 = { x = box_max.x; y = box_min.y } in
  let { x; y } = point in
  if x >. box_max.x && y >. box_max.y
  then c4, c3
  else if x <. box_min.x && y <. box_min.y
  then c3, c4
  else if x <. box_min.x && y >. box_max.y
  then c2, c1
  else if x >. box_max.x && y <. box_min.y
  then c1, c2
  else if y >. box_max.y
  then c2, c3
  else if y <. box_min.y
  then c1, c4
  else if x <. box_min.x
  then c3, c1
  else c4, c2
;;

module Example1 = struct
  open! V
  open! V.Infix

  let box_l, box_b = { x = 0.0; y = 0.0 }, { x = 30.0; y = 30.0 }

  let box_midpoint =
    let x = (box_l.x +. box_b.x) /. 2.0 in
    let y = (box_l.y +. box_b.y) /. 2.0 in
    { x; y }
  ;;

  let shape ~offset target =
    let intersection, (i_line_a, i_line_b) =
      Option.value_exn (line_box (box_l, box_b) (box_midpoint, target))
    in
    let mid_midpoint = midpoint target intersection in
    let curve ~stroke ~fill =
      S.path
        ~stroke
        ~fill
        [ Move i_line_a
        ; Bezier_to
            { start_handle = i_line_a + V.swap (i_line_a - midpoint i_line_a intersection)
            ; end_handle = intersection
            ; dest = mid_midpoint
            }
        ; Bezier_to
            { start_handle = intersection
            ; end_handle = i_line_b - V.swap (i_line_b - midpoint i_line_b intersection)
            ; dest = i_line_b
            }
        ]
    in
    let offset =
      let dx, dy = offset in
      sprintf "translate(%.3f, %.3f)" dx dy
    in
    S.group
      ~transforms:[ "translate(10, 10)"; offset ]
      [ S.rect box_l box_b
      ; curve ~stroke:(S.Stroke.create ~color:"black" ()) ~fill:"black"
      ; S.group
          [ S.line i_line_a i_line_b ~stroke:(S.Stroke.create ~color:"purple" ())
          ; S.circle
              i_line_a
              ~r:1.0
              ~fill:"grey"
              ~stroke:(S.Stroke.create ~color:"none" ())
          ; S.circle
              i_line_b
              ~r:1.0
              ~fill:"black"
              ~stroke:(S.Stroke.create ~color:"none" ())
          ; S.circle box_midpoint ~r:1.5 ~fill:"red"
          ; S.circle target ~r:1.5 ~fill:"red"
          ; S.line box_midpoint target ~stroke:(S.Stroke.create ~color:"red" ())
          ; S.circle intersection ~r:2.0 ~fill:"pink"
          ; S.circle mid_midpoint ~r:2.0 ~fill:"teal"
          ; curve ~stroke:(S.Stroke.create ~color:"green" ()) ~fill:"none"
          ]
      ]
  ;;
end

module Example2 = struct
  open! V
  open! V.Infix

  let c_mid = { x = 15.0; y = 15.0 }
  let c_rad = 10.0

  let transl offset =
    let dx, dy = offset in
    sprintf "translate(%.3f, %.3f)" dx dy
  ;;

  let shape ~offset target =
    let intersection = (norm (target - c_mid) *. c_rad) + c_mid in
    let join_point = midpoint intersection target in
    let theta = angle (join_point - c_mid) in
    let delta_theta =
      Float.acos (c_rad /. (c_rad +. length (join_point - intersection)))
    in
    let p1 = (of_angle (theta -. delta_theta) *. c_rad) + c_mid in
    let p2 = (of_angle (theta +. delta_theta) *. c_rad) + c_mid in
    let curve ~stroke ~fill =
      S.path
        ~stroke
        ~fill
        [ Move p1
        ; Bezier_to
            { start_handle = midpoint p1 (midpoint p1 join_point)
            ; end_handle = intersection
            ; dest = join_point
            }
        ; Bezier_to
            { start_handle = intersection
            ; end_handle = midpoint p2 (midpoint p2 join_point)
            ; dest = p2
            }
        ; Arc_to { radius = c_rad; dest = p1; large_arc = false; sweep = false }
        ]
    in
    S.group
      ~transforms:[ "translate(10, 10)"; transl offset ]
      [ S.circle ~stroke:(S.Stroke.create ~color:"black" ()) ~fill:"black" c_mid ~r:c_rad
      ; curve ~stroke:(S.Stroke.create ~color:"black" ()) ~fill:"black"
        (*
      ; S.group
          [ S.circle target ~r:1.5 ~fill:"red"
          ; S.circle intersection ~r:1.5 ~fill:"green"
          ; S.circle join_point ~r:1.5 ~fill:"blue"
          ; S.circle p1 ~r:1.5 ~fill:"pink"
          ; S.circle p2 ~r:1.5 ~fill:"purple"
          ] *)
      ]
  ;;
end

let shape =
  V.
    [ { x = 60.0; y = 15.0 }
    ; { x = 60.0; y = 30.0 }
    ; { x = 60.0; y = 60.0 }
    ; { x = 30.0; y = 60.0 }
    ; { x = 15.0; y = 60.0 }
    ]
  |> List.folding_map ~init:0.0 ~f:(fun acc pt ->
         let out = Example2.shape ~offset:(0.0, acc) pt in
         let bump = acc +. Float.max 30.0 pt.y +. 5.0 in
         bump, out)
  |> N.svg
;;

let () =
  print_endline
    [%string
      {|
  <!DOCTYPE html>
  <html> 
  <head>
    <link rel="stylesheet" href="./style.css">
    <script src="./arrows.js"></script>
    <style>
html, body, .map {
  width:100%;
  height:100%;
}
    </style>
  </head>
  <body> 
    <div class="map"> %{shape#N} </div>
  </body>
  </html>
  |}]
;;
