open! Core

module Value : sig
  type 'a t
end

type 'a t

val return : 'a -> 'a t
val const : 'a -> 'a Value.t t
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

val bind : 'a t -> f:('a -> 'b t) -> 'b t
val lower : 'a Value.t t -> Mid.t * 'a Value.t
