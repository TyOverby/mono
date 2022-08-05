open! Core
include Braid_intf
include High

module type Universe = Braid_intf.Universe with module Value := Value

let compile t =
  let mid, mid_lookup, res = High.Expert.lower t in
  let low, low_lookup = Mid.Expert.lower mid in
  let m =
    (module struct
      let is_computed v_high =
        match mid_lookup.f v_high with
        | Node v_mid ->
          let v_low = low_lookup.f v_mid in
          stage (fun () -> Low.Node.has_value low v_low)
        | Constant _ -> stage (fun () -> true)
        | Exception _ -> stage (fun () -> true)
      ;;

      let stabilize () = Low.stabilize low

      let value_exn v_high =
        match mid_lookup.f v_high with
        | Node v_mid ->
          let v_low = low_lookup.f v_mid in
          stage (fun () ->
              if Low.Node.has_value low v_low
              then Low.Node.read_value low v_low
              else failwith "value not yet comptued")
        | Constant v -> stage (fun () -> v)
        | Exception e -> stage (fun () -> raise e)
      ;;

      let unsafe_value v_high =
        match mid_lookup.f v_high with
        | Node v_mid ->
          let v_low = low_lookup.f v_mid in
          stage (fun () -> Low.Node.read_value low v_low)
        | Constant v -> stage (fun () -> v)
        | Exception e -> stage (fun () -> raise e)
      ;;

      let watch v_high =
        match mid_lookup.f v_high with
        | Node v_mid ->
          let v_low = low_lookup.f v_mid in
          stage (fun () -> Low.Node.incr_refcount low v_low)
        | Constant _ | Exception _ -> stage (fun () -> ())
      ;;

      let unwatch v_high =
        match mid_lookup.f v_high with
        | Node v_mid ->
          let v_low = low_lookup.f v_mid in
          stage (fun () -> Low.Node.decr_refcount low v_low)
        | Constant _ | Exception _ -> stage (fun () -> ())
      ;;

      let set v_high =
        match mid_lookup.f v_high with
        | Node v_mid ->
          let v_low = low_lookup.f v_mid in
          stage (fun value -> Low.Node.write_value low v_low value)
        | Constant _ ->
          (* that's an issue... *)
          stage (fun _ -> assert false)
        | Exception _ ->
          (* that's an issue... *)
          stage (fun _ -> ())
      ;;
    end : Universe)
  in
  res, m
;;

module Private = struct
  module Low = Low
  module Mid = Mid
  module High = High
end
