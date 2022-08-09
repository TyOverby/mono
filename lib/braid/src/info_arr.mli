open! Core

type info

val info : info -> int -> int
val set_info : info -> int -> int -> unit

(**)
type depends_on
type depends_on_idx

val depends_on_idx : info -> int -> depends_on_idx
val depends_on_length : depends_on -> depends_on_idx -> int
val get_depends_on : depends_on -> depends_on_idx -> int -> int

(**)
type depended_on_by
type depended_on_by_idx

val depended_on_by_idx : info -> int -> depended_on_by_idx
val depended_on_by_length : depended_on_by -> depended_on_by_idx -> int
val get_depended_on_by : depended_on_by -> depended_on_by_idx -> int -> int

module Builder : sig
  type t

  val create : int -> t
  val add : t -> int -> depends_on:int array -> depended_on_by:int array -> unit
  val finalize : t -> info * depends_on * depended_on_by
end
