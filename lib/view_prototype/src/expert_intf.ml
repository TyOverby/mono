open! Core
open! Import

module type S = sig
  module Theme : T

  module type S = sig
    class t :
      object
        method background_color : string
        method foreground_color : string
        method button : string -> on_click:unit Effect.t -> Vdom.Node.t
      end
  end

  val default_theme : Theme.t
  val make_theme : (module S) -> Theme.t
  val override_theme : Theme.t -> f:((module S) -> (module S)) -> Theme.t
end
