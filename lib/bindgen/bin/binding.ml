open! Core
open! Bindgen

module Let_syntax = struct
  module Let_syntax = struct
    let sub
        :  ?here:_ -> (unit -> Computation.t) -> f:(Value.t -> unit -> Computation.t)
        -> unit -> Computation.t
      =
     fun ?here:_ t ~f () ->
      let name = Name.create () in
      Computation.sub ~bound:(t ()) ~as_:name ~for_:(f (Value.named name) ())
   ;;

    let map a ~f:_ () = Computation.return (Value.mapn [ a ])
    let map2 a b ~f:_ () = Computation.return (Value.mapn [ a; b ])
    let map3 a b c ~f:_ () = Computation.return (Value.mapn [ a; b; c ])
    let return t () = Computation.return t
  end

  let return = Let_syntax.return
end

open Let_syntax

let print name t =
  printf "<h1> %s </h1>\n" name;
  print_endline "<div class='testcase'>";
  print_endline (To_html.to_html t);
  print_endline "</div>"
;;

let print' name t = print name (t ())

let () =
  print_endline
    {|
  <!DOCTYPE html>
  <html> 
  <head>
    <link rel="stylesheet" href="./style.css">
    <script src="./arrows.js"></script>
  </head>
  <body>
  |}
;;

let () = Computation.return (Value.singleton ()) |> print "return named value"

let () =
  Computation.return (Value.mapn [ Value.singleton () ]) |> print "mapn with single child"
;;

let () =
  Computation.return
    (Value.mapn [ Value.singleton (); Value.singleton (); Value.singleton () ])
  |> print "mapn with multiple children "
;;

let () =
  Computation.return
    (Value.mapn
       [ Value.mapn [ Value.singleton (); Value.singleton (); Value.singleton () ]
       ; Value.mapn [ Value.singleton (); Value.singleton (); Value.singleton () ]
       ])
  |> print "multiple mapns"
;;

let () =
  Computation.return
    (Value.mapn
       [ Value.mapn [ Value.singleton (); Value.singleton (); Value.singleton () ]
       ; Value.mapn
           [ Value.mapn [ Value.singleton (); Value.singleton (); Value.singleton () ] ]
       ])
  |> print "multiple mapns (offset)"
;;

let () =
  let bound_as = Name.create () in
  Computation.sub
    ~bound:(Computation.return (Value.singleton ()))
    ~as_:bound_as
    ~for_:(Computation.return (Value.named bound_as))
  |> print "sub"
;;

let () =
  (let%sub a = return (Value.singleton ()) in
   let%sub b = return (Value.singleton ()) in
   return (Value.mapn [ a; Value.mapn [ a ]; b ]))
  |> print' "sub 3"
;;

let () =
  (let%sub a =
     let%mapn _ = Value.singleton () in
     ()
   in
   let%sub b = return (Value.singleton ()) in
   let%sub c =
     let%mapn _ = a
     and _ = b in
     ()
   in
   let%mapn _ = a
   and _ = b
   and _ = c in
   ())
  |> print' "sub and arr"
;;

let () =
  (let%sub a = return (Value.singleton ()) in
   let%sub b = return (Value.singleton ()) in
   let%sub c =
     let%mapn _ = a
     and _ = b in
     ()
   in
   let%sub c =
     let%mapn _ = c
     and _ = a in
     ()
   in
   let%mapn _ = a
   and _ = b
   and _ = c in
   ())
  |> print' "sub and arr 2"
;;

let () =
  let x =
    let%sub a = return (Value.singleton ()) in
    let%sub b = return (Value.singleton ()) in
    let%sub c =
      let%mapn _ = a
      and _ = b in
      ()
    in
    let%sub c =
      let%mapn _ = c
      and _ = a in
      ()
    in
    let%mapn _ = a
    and _ = b
    and _ = c in
    ()
  in
  (let%sub a = x in
   let%sub b = x in
   let%mapn _ = a
   and _ = b in
   ())
  |> print' "sub and arr 2"
;;

let () = print_endline {|
  </body>
  </html>
  |}
