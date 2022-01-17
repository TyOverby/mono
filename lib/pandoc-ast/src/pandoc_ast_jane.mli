open! Core

module Attrs : sig
  type t [@@deriving sexp, equal, quickcheck, compare]

  val id : t -> string
  val classes : t -> string list
  val attributes : t -> (string * string) list
  val make : id:string -> classes:string list -> attributes:(string * string) list -> t
end

module Target : sig
  type t [@@deriving sexp, equal, quickcheck, compare]

  val url : t -> string
  val title : t -> string
end

module Ordered_list_attrs : sig
  module Style : sig
    type t =
      | DefaultStyle
      | Example
      | Decimal
      | LowerRoman
      | UpperRoman
      | LowerAlpha
      | UpperAlpha
    [@@deriving sexp, equal, quickcheck, compare]
  end

  module Delim : sig
    type t =
      | DefaultDelim
      | Period
      | OneParen
      | TwoParensPeriod
    [@@deriving sexp, equal, quickcheck, compare]
  end

  type t [@@deriving sexp, equal, quickcheck, compare]

  val starting_at : t -> int
  val number_style : t -> Style.t
  val number_delim : t -> Delim.t
end

module Quote_type : sig
  type t =
    | DoubleQuote
    | SingleQuote
  [@@deriving sexp, equal, quickcheck, compare]
end

module Metadata : sig
  type t [@@deriving sexp_of, equal, compare]

  val lookup_bool : t -> field:string -> bool option
  val lookup_string : t -> field:string -> string option
end

module Inline : sig
  module Additional : sig
    type t

    type extra =
      | SoftBreak
      | LineBreak
      | Other of Yojson.Basic.t
    [@@deriving sexp]

    val reveal : t -> extra
    val conceal : extra -> t
  end

  type t =
    | Code of Attrs.t * string
    | Emph of t list
    | Image of Attrs.t * t list * Target.t
    | Link of Attrs.t * t list * Target.t
    | Quoted of Quote_type.t * t list
    | RawInline of string * string
    | Space
    | SmallCaps of t list
    | Str of string
    | UnhandledInline of Additional.t
  [@@deriving sexp, equal, quickcheck, compare]
end

module rec List_item : sig
  type t
  type details [@@deriving sexp, equal, quickcheck, compare]

  val kind : details -> [ `Checked | `Normal | `Unchecked ]
  val reveal : t -> details
  val conceal : details -> t
  val check : details -> details
  val uncheck : details -> details
  val remove_checkbox : details -> details
end

and Block : sig
  type t =
    | BulletList of List_item.t list
    | CodeBlock of Attrs.t * string
    | Header of int * Attrs.t * Inline.t list
    | OrderedList of Ordered_list_attrs.t * List_item.t list
    | Para of Inline.t list
    | Plain of Inline.t list
    | RawBlock of string * string
    | Div of Attrs.t * t list
    | UnhandledBlock of Yojson.Basic.t
  [@@deriving sexp, equal, quickcheck, compare]
end

type t [@@deriving sexp, equal, quickcheck, compare]

val of_pandoc_ast : Yojson.Basic.t -> t
val of_pandoc_ast_string : string -> t
val to_pandoc_ast : t -> Yojson.Basic.t
val to_pandoc_ast_string : t -> string

(* *)
val pandoc_api_version : t -> int list
val metadata : t -> Metadata.t
val top_level_blocks : t -> Block.t list
val with_top_level_blocks : t -> Block.t list -> t

(* *)
val exists : inline:(Inline.t -> bool) -> block:(Block.t -> bool) -> t -> bool
val for_all : inline:(Inline.t -> bool) -> block:(Block.t -> bool) -> t -> bool
val map : inline:(Inline.t -> Inline.t) -> block:(Block.t -> Block.t) -> t -> t
val filter : inline:(Inline.t -> bool) -> block:(Block.t -> bool) -> t -> t

val concat_map
  :  inline:(Inline.t -> Inline.t list)
  -> block:(Block.t -> Block.t list)
  -> t
  -> t

val folding_concat_map
  :  inline:('a -> Inline.t -> 'a * Inline.t list)
  -> block:('a -> Block.t -> 'a * Block.t list)
  -> init:'a
  -> t
  -> t

val folding_map
  :  inline:('a -> Inline.t -> 'a * Inline.t)
  -> block:('a -> Block.t -> 'a * Block.t)
  -> init:'a
  -> t
  -> t

val iter : ?inline:(Inline.t -> unit) -> ?block:(Block.t -> unit) -> t -> unit
