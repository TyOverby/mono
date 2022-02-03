open! Core

module F_bool : sig
  type t [@@deriving sexp_of]

  val of_float : float -> t
  val of_bool : bool -> t
  val to_float : t -> float
end

type 'a t
type 'a missing_falloff

val with_falloff : 'a missing_falloff -> falloff:float t -> 'a t
val float_const : float -> float t
val bool_const : F_bool.t -> F_bool.t t
val if_ : F_bool.t t -> 'a t -> 'a t -> 'a t
val ( + ) : float t -> float t -> float t
val ( < ) : float t -> float t -> F_bool.t missing_falloff

(* *)
val eval : 'a t -> 'a

module Let_syntax : sig
  module Let_syntax : sig
    val return : 'a t -> 'a t
    val map : 'a t -> f:(_ -> _) -> 'a t
    val sub : ?here:Source_code_position.t -> 'a t -> f:('a t -> 'b t) -> 'b t
    val switch : match_:F_bool.t t -> branches:int -> with_:(int -> 'a t) -> 'a t
  end
end
