(* Copyright (c) 2016-2017 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

(**
 * Demonstrates mouse input.
 *)
open Notty
open Common

let lnv = Uchar.of_int 0x2502
and lnh = Uchar.of_int 0x2500
and crs = Uchar.of_int 0x253c

let clip a b x = min b (max a x)

let () =
  simpleterm ~s:(`Down, (0, 0), [], 11)
    ~f:(fun (st, pos, mods, scr as s) -> function
      | `Mouse ((`Press `Left|`Drag), pos, mods) -> Some (`Drag, pos, mods, scr)
      | `Mouse (`Press (`Scroll s), _, _) ->
          Some (st, pos, mods, clip 0 23 (scr + match s with `Up -> 1 | _ -> -1))
      | `Mouse (`Release, pos, _) -> Some (`Down, pos, [], scr)
      | _ -> Some s)
    ~imgf:I.(fun (w, h) (st, (x, y), mods, scr) ->
      let cross =
        (uchar lnh x 1 |> vpad y 0) <|>
        (uchar lnv 1 y <-> uchar crs 1 1 <-> uchar lnv 1 (h - y)) <|>
        (uchar lnh (w - x - 1) 1 |> vpad y 0)
        |> attr (match st with `Drag -> A.(fg lightred) | `Down -> A.(fg red))
        |> crop ~t:1 ~l:1 ~r:3
        |> hpad 1 1
        |> vsnap ~align:`Top (h - 1)
      and scroll =
        0 -- scr |> List.rev
                 |> List.map (fun i -> Images.dot |> attr A.(gray i |> fg))
                 |> vcat |> vsnap ~align:`Bottom (h - 1)
      and status =
        let on m = if List.mem m mods then A.(fg lightgreen) else A.empty in
        hcat [ string "["
             ; string ~attr:(on `Ctrl) "C"
             ; string ~attr:(on `Meta) "M"
             ; strf "] @(%03d, %03d)" x y ]
        |> attr A.(fg lightblack ++ bg black) |> hsnap ~align:`Right w
      in (cross <|> scroll) <-> status
    )
