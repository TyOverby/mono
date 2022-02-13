open Core
module Which_range = Collate.Which_range

module Parametrized = struct
  type ('k, 'v) t =
    { data : ('k * 'v) Map_list.t
    ; num_filtered_rows : int
    ; key_range : 'k Which_range.t (** Ranges that this value was computed for *)
    ; rank_range : int Which_range.t
    ; num_before_range : int
    ; num_unfiltered_rows : int
    }
  [@@deriving sexp, compare, fields, equal, bin_io]

  let empty =
    { data = Int63.Map.empty
    ; num_filtered_rows = 0
    ; key_range = All_rows
    ; rank_range = All_rows
    ; num_before_range = 0
    ; num_unfiltered_rows = 0
    }
  ;;

  let num_after_range { num_before_range; num_filtered_rows; data; _ } =
    num_filtered_rows - num_before_range - Map.length data
  ;;

  let fold t ~init ~f = Map.fold t.data ~init ~f:(fun ~key:_ ~data acc -> f acc data)
  let iter t ~f = Map.iter t.data ~f
  let to_alist t = Map.data t.data
  let to_map_list t = t.data
  let first t = Map.min_elt t.data |> Option.map ~f:snd
  let last t = Map.max_elt t.data |> Option.map ~f:snd
  let mapi t ~f = { t with data = Map.map t.data ~f:(fun (k, v) -> k, f k v) }
  let length t = Map.length t.data

  module Private = struct
    let create = Fields.create
  end

  module For_testing = struct
    let of_list
          ~num_filtered_rows
          ~key_range
          ~rank_range
          ~num_before_range
          ~num_unfiltered_rows
          data
      =
      { data =
          List.mapi data ~f:(fun i x -> 100 * i |> Int63.of_int, x)
          |> Int63.Map.of_alist_exn
      ; num_filtered_rows
      ; rank_range
      ; key_range
      ; num_before_range
      ; num_unfiltered_rows
      }
    ;;
  end
end

include Parametrized

module type Concrete = Collated_intf.Concrete with type ('k, 'v) parametrized = ('k, 'v) t

module Make_concrete
    (Key : Collated_intf.Bin_comp_sexp)
    (Value : Collated_intf.Bin_comp_sexp) =
struct
  module Key = Key
  module Value = Value
  include Parametrized

  type ('k, 'v) parametrized = ('k, 'v) t

  module T = struct
    type t = (Key.t, Value.t) Parametrized.t [@@deriving sexp, bin_io, compare, equal]

    (* We have to implement this by hand, as ppx_diff (or Diffable really)
       doesn't support parametrized types *)

      end

  include T

  let findi_by_key t key =
    let found =
      Map.filteri ~f:(fun ~key:_pos ~data:(key', _value) -> Key.equal key key') t.data
      |> Map.to_alist
    in
    match found with
    | [ x ] -> Some x
    | [] -> None
    | _ ->
      raise_s
        [%message "[Collated.findi_by_key] BUG: multiple entries found" (key : Key.t)]
  ;;

  let find_by_key t key =
    Option.map (findi_by_key t key) ~f:(fun (_pos, (_key, value)) -> value)
  ;;

  let prev t key =
    let%bind.Option pos, _ = findi_by_key t key in
    let%map.Option _pos, res = Map.closest_key t.data `Less_than pos in
    res
  ;;

  let next t key =
    let%bind.Option pos, _ = findi_by_key t key in
    let%map.Option _pos, res = Map.closest_key t.data `Greater_than pos in
    res
  ;;
end
