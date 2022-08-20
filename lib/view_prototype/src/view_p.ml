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

  let override_constants (theme : t) ~(f : Constants.t -> Constants.t) : t =
    let module T = (val theme) in
    let prev_constants : Constants.t =
      { background_color = T.t#background_color; foreground_color = T.t#background_color }
    in
    let { Constants.background_color; foreground_color } = f prev_constants in
    (module struct
      class t =
        object
          inherit T.t
          method! background_color = background_color
          method! foreground_color = foreground_color
        end

      let t = new t
    end)
  ;;
end

let background_color ((module T) : Theme.t) = T.t#background_color
let foreground_color ((module T) : Theme.t) = T.t#foreground_color
let button ((module T) : Theme.t) text ~on_click = T.t#button text ~on_click

module Expert = struct
  module type S = Underlying.S

  type t = (module S)

  let make_theme (m : t) : Theme.t =
    (module struct
      include (val m)

      let t = new t
    end)
  ;;

  let override_theme ((module M) : Theme.t) ~(f : t -> t) : Theme.t =
    (module struct
      include (val f (module M))

      let t = new t
    end)
  ;;

  let default_theme =
    make_theme
      (module struct
        class t =
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
      end)
  ;;
end
