open! Core
module Vdom = Vdom

module Name : sig
  type t

  val create : unit -> t
end

module Value : sig
  type t

  val named : Name.t -> t
  val mapn : t list -> t
  val singleton : unit -> t
end

module Computation : sig
  type t

  val sub : bound:t -> as_:Name.t -> for_:t -> t
  val return : Value.t -> t
  val wrap : name:string -> introduces:Name.t list -> bodies:t list -> t
end

module To_html : sig
  val to_html : Computation.t -> string
end
