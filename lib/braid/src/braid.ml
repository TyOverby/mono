open! Core

module Env_id = struct
  type t = int
end

module Node = struct
  type t = string
end

module Env = struct
  module Handle0 = struct
    type 'a t = private int
    type packed = T : 'a t -> packed [@@unboxed]
  end

  type t =
    { id : Env_id.t
    ; values : Obj.t Uopt.t Uniform_array.t
    ; cutoffs : Obj.t Uopt.t Uniform_array.t
    ; mutable dirty : Handle0.packed list
    }

  module Handle = struct
    include Handle0

    let value env id =
      if id < 0 || id >= Uniform_array.length env.values
      then failwithf "Braid.Env.Handle.value: id (%d) out of bounds" id ();
      let uo = Uniform_array.get env.values id in
      match Uopt.to_option uo with
      | None ->
        failwithf "Braid.Env.Handle.value: value for id (%d) not computed yet" id ()
      | Some a -> a
    ;;
  end

  module Builder = struct end
end

let count () =
  In_channel.stdin
  |> In_channel.input_lines
  |> List.concat_map ~f:(String.split_on_chars ~on:[ ' '; '\t' ])
  |> List.sort_and_group ~compare:String.Caseless.compare
  |> List.iter ~f:(function
         | [] -> assert false
         | s :: _ as l -> printf "%s : %d" (String.lowercase s) (List.length l))
;;
