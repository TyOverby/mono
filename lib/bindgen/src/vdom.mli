open! Core

module Attr : sig
  type t

  val class_ : string -> t
  val classes : string list -> t
  val id : string -> t
  val data : string -> string -> t
  val many : t list -> t
end

module Node : sig
  type t
  type creator := ?attr:Attr.t -> t list -> t

  val none : t
  val text : string -> t
  val svg : creator
  val div : creator
  val span : creator
  val to_string : t -> string
end

module Svg : sig
  module Vec2 : sig
    type t =
      { x : float
      ; y : float
      }
    [@@deriving sexp]
  end

  module Stroke : sig
    type t

    val create : ?width:float -> ?color:string -> ?linecap:[ `Round ] -> unit -> t
  end

  module Path : sig
    type t =
      | Move of Vec2.t
      | Bezier_to of
          { start_handle : Vec2.t
          ; end_handle : Vec2.t
          ; dest : Vec2.t
          }
      | Arc_to of
          { radius : float
          ; large_arc : bool
          ; sweep : bool
          ; dest : Vec2.t
          }
    [@@deriving sexp]
  end

  val path : ?attr:Attr.t -> ?stroke:Stroke.t -> ?fill:string -> Path.t list -> Node.t
  val line : ?attr:Attr.t -> ?stroke:Stroke.t -> Vec2.t -> Vec2.t -> Node.t

  val rect
    :  ?attr:Attr.t
    -> ?stroke:Stroke.t
    -> ?fill:string
    -> Vec2.t
    -> Vec2.t
    -> Node.t

  val circle
    :  ?attr:Attr.t
    -> ?stroke:Stroke.t
    -> ?fill:string
    -> Vec2.t
    -> r:float
    -> Node.t

  val group : ?attr:Attr.t -> ?transforms:string list -> Node.t list -> Node.t
end
