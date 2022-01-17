(** This module extends {{!Base.Int}[Base.Int]}. *)

include Base.Int.Int_without_module_types (** @inline *)

(** Note that [int] is already stable by itself, since as a primitive type it is an
    integral part of the sexp / bin_io protocol.  [Int.Stable] exists only to introduce
    [Int.Stable.Set] and [Int.Stable.Map], and provide interface uniformity with other
    stable types. *)

include
  Int_intf.Extension_with_stable
  with type t := t
   and type comparator_witness := comparator_witness
