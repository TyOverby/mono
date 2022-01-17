open! Core

module Name = struct
  module T = Unique_id.Int ()
  include T
  include Comparable.Make_binable (T)

  let to_string t = "name_" ^ to_string t
end

(* This ugly recursive type / recursive module structure is 
   required in order to get sexp-deriving to work correctly *)

type kind =
  | Bindings of
      { bindings : binding list
      ; last_body : computation
      }
  | Value of value
  | Wrapping of
      { name : string
      ; introduces : Name.t list
      ; bodies : computation list
      }

and binding =
  { bound : computation
  ; as_ : Name.t
  }

and value =
  | Redirect of Name.t
  | Named of Name.t
  | Singleton
  | Mapn of value list

and computation =
  { kind : kind
  ; free_variables : Name.Set.t
  }
[@@deriving sexp]

module rec Kind : sig
  type t = kind =
    | Bindings of
        { bindings : binding list
        ; last_body : computation
        }
    | Value of value
    | Wrapping of
        { name : string
        ; introduces : Name.t list
        ; bodies : computation list
        }
  [@@deriving sexp]
end =
  Kind

and Binding : sig
  type t = binding =
    { bound : computation
    ; as_ : Name.t
    }
  [@@deriving sexp]
end =
  Binding

and Value : sig
  type t = value =
    | Redirect of Name.t
    | Named of Name.t
    | Singleton
    | Mapn of t list
  [@@deriving sexp]
end =
  Value

and Computation : sig
  type nonrec t = computation =
    { kind : kind
    ; free_variables : Name.Set.t
    }
  [@@deriving sexp]
end =
  Computation
