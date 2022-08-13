open! Core

module Value : sig
  type 'a t
end

type 'a t

val return : 'a -> 'a t
val const : 'a -> 'a Value.t t
val const_node : 'a -> 'a Value.t t
val arr1 : 'a Value.t -> f:('a -> 'b) -> 'b Value.t t
val arr2 : 'a Value.t -> 'b Value.t -> f:('a -> 'b -> 'c) -> 'c Value.t t

val arr3
  :  'a Value.t
  -> 'b Value.t
  -> 'c Value.t
  -> f:('a -> 'b -> 'c -> 'd)
  -> 'd Value.t t

val arr4
  :  'a Value.t
  -> 'b Value.t
  -> 'c Value.t
  -> 'd Value.t
  -> f:('a -> 'b -> 'c -> 'd -> 'e)
  -> 'e Value.t t

val if_ : bool Value.t -> then_:'a Value.t -> else_:'a Value.t -> 'a Value.t t
val bind : 'a t -> f:('a -> 'b t) -> 'b t
val state : 'a -> ('a Value.t * (('a -> 'a) -> unit) Value.t) t

module Let_syntax : sig
  val return : 'a -> 'a t

  module Let_syntax : sig
    val bind : 'a t -> f:('a -> 'b t) -> 'b t
    val return : 'a -> 'a t
  end
end

module Expert : sig
  module Value_or_node : sig
    type 'a t =
      | Constant of 'a
      | Exception of exn
      | Node of 'a Mid.Node.t
  end

  type lookup = { f : 'a. 'a Value.t -> 'a Value_or_node.t }

  val lower : 'a t -> Mid.t * lookup * 'a
end
