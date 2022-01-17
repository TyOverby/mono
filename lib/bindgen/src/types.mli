open! Core

module Name : sig
  type t

  val to_string : t -> string
  val create : unit -> t

  include Comparable.S_binable with type t := t
  include Sexpable.S with type t := t
end

module rec Kind : sig
  type t =
    | Bindings of
        { bindings : Binding.t list
        ; last_body : Computation.t
        }
    | Value of Value.t
    | Wrapping of
        { name : string
        ; introduces : Name.t list
        ; bodies : Computation.t list
        }
end

and Binding : sig
  type t =
    { bound : Computation.t
    ; as_ : Name.t
    }
  [@@deriving sexp]
end

and Value : sig
  type t =
    | Redirect of Name.t
    | Named of Name.t
    | Singleton
    | Mapn of Value.t list
end

and Computation : sig
  type nonrec t =
    { kind : Kind.t
    ; free_variables : Name.Set.t
    }
end
