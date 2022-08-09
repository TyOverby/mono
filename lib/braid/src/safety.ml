open! Core

module Common = struct
  module Array = struct
    type 'a t = 'a Array.t [@@deriving sexp_of]

    let length = Array.length
    let empty = [||]
    let of_array = Fn.id
    let create = Array.create
    let init = Array.init
    let iter = Array.iter
    let blit = Array.blit
  end
end

module Unsafe = struct
  module Obj_array = struct
    type t = Obj.t Uniform_array.t

    let init_empty len = Uniform_array.create_obj_array ~len
    let get_some = Uniform_array.unsafe_get
    let set_some = Uniform_array.unsafe_set_omit_phys_equal_check

    let set_some_int_assuming_currently_int t i v =
      Uniform_array.unsafe_set_int_assuming_currently_int t i (Obj.magic v : int)
    ;;

    let set_some_assuming_currently_int t i v =
      Uniform_array.unsafe_set_assuming_currently_int t i v
    ;;

    let set_some_int t i v = Uniform_array.unsafe_set_int t i (Obj.magic v : int)
  end

  module Array = struct
    include Common.Array

    let set = Array.unsafe_set
    let get = Array.unsafe_get
  end
end

module Safe = struct
  module Obj_array = struct
    type t = Obj.t Option_array.t

    let init_empty len = Option_array.init len ~f:(Fn.const None)
    let get_some = Option_array.get_some_exn
    let set_some = Option_array.set_some

    let set_some_int_assuming_currently_int t i v =
      assert (Obj.is_int v);
      (match Option_array.get t i with
      | None -> ()
      | Some v -> assert (Obj.is_int v));
      Option_array.set_some t i v
    ;;

    let set_some_assuming_currently_int t i v =
      (match Option_array.get t i with
      | None -> ()
      | Some v -> assert (Obj.is_int v));
      Option_array.set_some t i v
    ;;

    let set_some_int t i v =
      assert (Obj.is_int v);
      Option_array.set_some t i v
    ;;
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

module Option_array = struct end
