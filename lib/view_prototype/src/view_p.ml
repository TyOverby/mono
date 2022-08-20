open! Core
open! Import
module Constants = Constants

module Underlying = struct
  class type t =
    object
      method background_color : string
      method foreground_color : string
      method button : string -> on_click:unit Effect.t -> Vdom.Node.t
    end

  class type t' = t

  module type S = sig
    class t : t'
  end
end

module Theme = struct
  module type Impl_s = sig
    include Underlying.S

    val t : t
  end

  type t = (module Impl_s)

  let default : t =
    let module M : Impl_s = struct
      class t : Underlying.t =
        object (self)
          method background_color = "white"
          method foreground_color = "black"

          method button text ~on_click:() =
            sprintf
              "button: %s, foreground: %s, background: %s"
              text
              self#foreground_color
              self#background_color
        end

      let t = new t
    end
    in
    (module M)
  ;;

  let override_constants (theme : t) ~(f : Constants.t -> Constants.t) : t =
    let module T = (val theme) in
    let prev_constants =
      { Constants.background_color = T.t#background_color
      ; foreground_color = T.t#background_color
      }
    in
    let { Constants.background_color; foreground_color } = f prev_constants in
    let module M = struct
      class t =
        object
          inherit T.t
          method! background_color = background_color
          method! foreground_color = foreground_color
        end
    end
    in
    (module struct
      include M

      let t = new M.t
    end)
  ;;
end

let background_color ((module T) : Theme.t) = T.t#background_color
let foreground_color ((module T) : Theme.t) = T.t#foreground_color
let button ((module T) : Theme.t) text ~on_click = T.t#button text ~on_click

module Expert = struct
  module type S = Underlying.S

  type t = (module S)

  let default_theme : Theme.t = Theme.default

  let override (theme : Theme.t) ~(f : t -> t) : Theme.t =
    let m =
      f
        (module struct
          include (val theme)
        end)
    in
    let module M = (val m) in
    (module struct
      include M

      let t = new M.t
    end)
  ;;
end
