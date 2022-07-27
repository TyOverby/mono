open! Core

module rec Serializer : sig
  module type Atoms = sig
    type t
  end

  module type S = sig
    type t

    val write_int : (t -> int -> unit[@inline])
    val write_bool : (t -> bool -> unit[@inline])

    module Record : sig
      type t

      module Field : sig
        type t

        val value : (t -> (module Serializable.S with type t = 'a) -> 'a -> unit[@inline])
        val finish : (t -> unit[@inline])
      end

      val field : (t -> string -> Field.t[@inline])
      val finish : (t -> unit[@inline])
    end

    val record : (t -> Record.t[@inline])
  end
end =
  Serializer

and Serializable : sig
  module type S = sig
    type t

    val serialize : (module Serializer.S with type t = 'a) -> 'a -> t -> unit [@@inline]
  end
end =
  Serializable

module Int = struct
  include Int

  let[@inline] serialize (type ser) (module T : Serializer.S with type t = ser) ser t =
    T.write_int ser t
  ;;
end

module Bool = struct
  include Bool

  let[@inline] serialize (type ser) (module T : Serializer.S with type t = ser) ser t =
    T.write_bool ser t
  ;;
end
