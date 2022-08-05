open! Core

module Common = struct
  module Option_array = struct
    type 'a t = 'a Option_array.t

    let init = Option_array.init
  end

  module Array = struct
    type 'a t = 'a Array.t [@@deriving sexp_of]

    let length = Array.length
    let empty = [||]
    let of_array = Fn.id
    let create = Array.create
    let init = Array.init
    let iter = Array.iter
  end
end

module Unsafe = struct
  module Option_array = struct
    include Common.Option_array

    let get = Option_array.unsafe_get
    let set = Option_array.unsafe_set
    let is_some = Option_array.unsafe_is_some
    let get_some = Option_array.unsafe_get_some_assuming_some
    let set_some = Option_array.unsafe_set_some
  end

  module Array = struct
    include Common.Array

    let set = Array.unsafe_set
    let get = Array.unsafe_get
  end
end

module Safe = struct
  module Option_array = struct
    include Common.Option_array

    let get = Option_array.get
    let set = Option_array.set
    let is_some = Option_array.is_some
    let get_some = Option_array.get_some_exn
    let set_some = Option_array.set_some
  end

  module Array = struct
    include Common.Array

    let get = Array.get
    let set = Array.set
  end
end

let profile = `Fast

include
  (val match profile with
       | `Safe -> (module Safe : Safety_intf.S)
       | `Fast -> (module Unsafe : Safety_intf.S))
