(** See {{!Iobuf}[Iobuf]} for documentation. *)

open! Core

(** [no_seek] and [seek] are phantom types used in a similar manner to [read] and
    [read_write]. *)

(** Like [read]. *)
type no_seek [@@deriving sexp_of]

(** Like [read_write]. *)
type seek = private no_seek [@@deriving sexp_of]

(** Collections of access functions.  These abstract over [Iobuf.Consume], [Iobuf.Fill],
    [Iobuf.Peek], and [Iobuf.Poke].

    Make all labeled arguments mandatory in [string] and [bigstring] to avoid accidental
    allocation in, e.g., [Iobuf.Poke.string].  For convenience, [stringo] and [bigstringo]
    are available by analogy between [blit] and [blito].

    [_trunc] functions silently truncate values that don't fit.  For example,
    [Iobuf.Unsafe.Poke.int8 128] effectively writes -128. *)
module type Accessors_common = sig

  (** [('d, 'w) Iobuf.t] accessor function manipulating ['a], either writing it to the
      iobuf or reading it from the iobuf. *)

  type ('a, 'd, 'w) t constraint 'd = [> read ]
  type 'a bin_prot

  val char : (char, 'd, 'w) t
  val int64_t_be : (Int64.t, 'd, 'w) t
  val int64_t_le : (Int64.t, 'd, 'w) t
  val head_padded_fixed_string : padding:char -> len:int -> (string, 'd, 'w) t
  val tail_padded_fixed_string : padding:char -> len:int -> (string, 'd, 'w) t
  val string : str_pos:int -> len:int -> (string, 'd, 'w) t
  val bytes : str_pos:int -> len:int -> (Bytes.t, 'd, 'w) t
  val bigstring : str_pos:int -> len:int -> (Bigstring.t, 'd, 'w) t
  val stringo : ?str_pos:int -> ?len:int -> (string, 'd, 'w) t
  val byteso : ?str_pos:int -> ?len:int -> (Bytes.t, 'd, 'w) t
  val bigstringo : ?str_pos:int -> ?len:int -> (Bigstring.t, 'd, 'w) t

  val bin_prot : 'a bin_prot -> ('a, 'd, 'w) t
end

module type Accessors_read = sig
  include Accessors_common

  val int8 : (int, 'd, 'w) t
  val int16_be : (int, 'd, 'w) t
  val int16_le : (int, 'd, 'w) t
  val int32_be : (int, 'd, 'w) t
  val int32_le : (int, 'd, 'w) t
  val int64_be_exn : (int, 'd, 'w) t
  val int64_le_exn : (int, 'd, 'w) t
  val int64_be_trunc : (int, 'd, 'w) t
  val int64_le_trunc : (int, 'd, 'w) t
  val uint8 : (int, 'd, 'w) t
  val uint16_be : (int, 'd, 'w) t
  val uint16_le : (int, 'd, 'w) t
  val uint32_be : (int, 'd, 'w) t
  val uint32_le : (int, 'd, 'w) t
  val uint64_be_exn : (int, 'd, 'w) t
  val uint64_le_exn : (int, 'd, 'w) t

  module Int_repr : sig
    val int8 : (Int_repr.Int8.t, 'd, 'w) t
    val int16_be : (Int_repr.Int16.t, 'd, 'w) t
    val int16_le : (Int_repr.Int16.t, 'd, 'w) t
    val int32_be : (Int_repr.Int32.t, 'd, 'w) t
    val int32_le : (Int_repr.Int32.t, 'd, 'w) t
    val int64_be : (Int_repr.Int64.t, 'd, 'w) t
    val int64_le : (Int_repr.Int64.t, 'd, 'w) t
    val uint8 : (Int_repr.Uint8.t, 'd, 'w) t
    val uint16_be : (Int_repr.Uint16.t, 'd, 'w) t
    val uint16_le : (Int_repr.Uint16.t, 'd, 'w) t
    val uint32_be : (Int_repr.Uint32.t, 'd, 'w) t
    val uint32_le : (Int_repr.Uint32.t, 'd, 'w) t
    val uint64_be : (Int_repr.Uint64.t, 'd, 'w) t
    val uint64_le : (Int_repr.Uint64.t, 'd, 'w) t
  end
end

module type Accessors_write = sig
  include Accessors_common

  val int8_trunc : (int, 'd, 'w) t
  val int16_be_trunc : (int, 'd, 'w) t
  val int16_le_trunc : (int, 'd, 'w) t
  val int32_be_trunc : (int, 'd, 'w) t
  val int32_le_trunc : (int, 'd, 'w) t
  val int64_be : (int, 'd, 'w) t
  val int64_le : (int, 'd, 'w) t
  val uint8_trunc : (int, 'd, 'w) t
  val uint16_be_trunc : (int, 'd, 'w) t
  val uint16_le_trunc : (int, 'd, 'w) t
  val uint32_be_trunc : (int, 'd, 'w) t
  val uint32_le_trunc : (int, 'd, 'w) t
  val uint64_be_trunc : (int, 'd, 'w) t
  val uint64_le_trunc : (int, 'd, 'w) t

  module Int_repr : sig
    val int8 : (Int_repr.Int8.t, 'd, 'w) t
    val int16_be : (Int_repr.Int16.t, 'd, 'w) t
    val int16_le : (Int_repr.Int16.t, 'd, 'w) t
    val int32_be : (Int_repr.Int32.t, 'd, 'w) t
    val int32_le : (Int_repr.Int32.t, 'd, 'w) t
    val int64_be : (Int_repr.Int64.t, 'd, 'w) t
    val int64_le : (Int_repr.Int64.t, 'd, 'w) t
    val uint8 : (Int_repr.Uint8.t, 'd, 'w) t
    val uint16_be : (Int_repr.Uint16.t, 'd, 'w) t
    val uint16_le : (Int_repr.Uint16.t, 'd, 'w) t
    val uint32_be : (Int_repr.Uint32.t, 'd, 'w) t
    val uint32_le : (Int_repr.Uint32.t, 'd, 'w) t
    val uint64_be : (Int_repr.Uint64.t, 'd, 'w) t
    val uint64_le : (Int_repr.Uint64.t, 'd, 'w) t
  end
end

(** An iobuf window bound, either upper or lower.  You can't see its int value, but you
    can save and restore it. *)
module type Bound = sig
  type ('d, 'w) iobuf

  type t = private int (*_ performance hack: avoid the write barrier *)
  [@@deriving compare, sexp_of]

  val window : (_, _) iobuf -> t
  val limit : (_, _) iobuf -> t
  val restore : t -> (_, seek) iobuf -> unit

end

(** The [src_pos] argument of {!Core.Blit.blit} doesn't make sense here. *)

type ('src, 'dst) consuming_blit = src:'src -> dst:'dst -> dst_pos:int -> len:int -> unit

type ('src, 'dst) consuming_blito =
  src:'src
  -> ?src_len:int (** Default is [Iobuf.length src]. *)
  -> dst:'dst
  -> ?dst_pos:int (** Default is [0]. *)
  -> unit
  -> unit

module type Consuming_blit = sig
  type src
  type dst

  val blito : (src, dst) consuming_blito
  val blit : (src, dst) consuming_blit
  val unsafe_blit : (src, dst) consuming_blit

  (** [subo] defaults to using [Iobuf.length src] *)
  val subo : ?len:int -> src -> dst

  val sub : src -> len:int -> dst
end

module type Compound_hexdump = sig
  type ('rw, 'seek) t

  module Hexdump : sig
    type nonrec ('rw, 'seek) t = ('rw, 'seek) t [@@deriving sexp_of]

    val to_string_hum : ?max_lines:int -> (_, _) t -> string
    val to_sequence : ?max_lines:int -> (_, _) t -> string Sequence.t
  end
end

module type Peek = sig
  type ('rw, 'seek) iobuf
  type 'seek src = (read, 'seek) iobuf

  (** Similar to [Consume.To_*], but do not advance the buffer. *)

  module To_bytes :
    Blit.S1_distinct with type 'seek src := 'seek src with type _ dst := Bytes.t

  module To_bigstring :
    Blit.S1_distinct with type 'seek src := 'seek src with type _ dst := Bigstring.t

  module To_string : sig
    val sub : (_ src, string) Base.Blit.sub
    val subo : (_ src, string) Base.Blit.subo
  end

  include
    Accessors_read
    with type ('a, 'd, 'w) t = ('d, 'w) iobuf -> pos:int -> 'a
    with type 'a bin_prot := 'a Bin_prot.Type_class.reader
end
