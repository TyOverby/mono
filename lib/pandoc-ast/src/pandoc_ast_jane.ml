open! Core
module P = Pandoc_ast

module Attrs = struct
  type t = string * string list * (string * string) list
  [@@deriving sexp, equal, quickcheck, compare]

  let (T : (t, P.attr) Type_equal.t) = T
  let id (id, _, _) = id
  let classes (_, classes, _) = classes
  let attributes (_, _, attributes) = attributes
  let make ~id ~classes ~attributes = id, classes, attributes
end

module Target = struct
  type t = string * string [@@deriving sexp, quickcheck, compare, equal]

  let (T : (t, P.target) Type_equal.t) = T
  let url (url, _) = url
  let title (title, _) = title
end

module Ordered_list_attrs = struct
  module Style = struct
    type t = P.list_number_style =
      | DefaultStyle
      | Example
      | Decimal
      | LowerRoman
      | UpperRoman
      | LowerAlpha
      | UpperAlpha
    [@@deriving sexp, equal, quickcheck, compare]
  end

  module Delim = struct
    type t = P.list_number_delim =
      | DefaultDelim
      | Period
      | OneParen
      | TwoParensPeriod
    [@@deriving sexp, equal, quickcheck, compare]
  end

  type t = int * Style.t * Delim.t [@@deriving sexp, equal, quickcheck, compare]

  let (T : (t, P.list_attributes) Type_equal.t) = T
  let starting_at (i, _, _) = i
  let number_style (_, number_style, _) = number_style
  let number_delim (_, _, number_delim) = number_delim
end

module Quote_type = struct
  type t = P.quote_type =
    | DoubleQuote
    | SingleQuote
  [@@deriving sexp, equal, quickcheck, compare]
end

module Yojson_with_sexp = struct
  type t =
    [ `Null
    | `Bool of bool
    | `Int of int
    | `Float of float
    | `String of string
    | `Assoc of (string * t) list [@sexp.list]
    | `List of t list [@sexp.list]
    ]
    constraint t = Yojson.Basic.t
  [@@deriving sexp, equal, quickcheck, compare]
end

module Inline = struct
  module Additional = struct
    include Yojson_with_sexp

    type extra =
      | SoftBreak
      | LineBreak
      | Other of Yojson_with_sexp.t
    [@@deriving sexp]

    let reveal : t -> extra = function
      | `Assoc [ ("t", `String "SoftBreak") ] -> SoftBreak
      | `Assoc [ ("t", `String "LineBreak") ] -> LineBreak
      | other -> Other other
    ;;

    let conceal : extra -> t = function
      | SoftBreak -> `Assoc [ "t", `String "SoftBreak" ]
      | LineBreak -> `Assoc [ "t", `String "LineBreak" ]
      | Other other -> other
    ;;

    let sexp_of_t t = t |> reveal |> sexp_of_extra
    let t_of_sexp s = s |> extra_of_sexp |> conceal
  end

  type t = P.inline =
    | Code of Attrs.t * string
    | Emph of t list [@sexp.list]
    | Image of Attrs.t * t list * Target.t
    | Link of Attrs.t * t list * Target.t
    | Quoted of Quote_type.t * t list
    | RawInline of string * string
    | Space
    | SmallCaps of t list [@sexp.list]
    | Str of string
    | UnhandledInline of Additional.t
  [@@deriving sexp, equal, quickcheck, compare]
end

module Block' = struct
  type t = P.block =
    | BulletList of t list list [@sexp.list]
    | CodeBlock of Attrs.t * string
    | Header of int * Attrs.t * Inline.t list
    | OrderedList of Ordered_list_attrs.t * t list list
    | Para of Inline.t list [@sexp.list]
    | Plain of Inline.t list [@sexp.list]
    | RawBlock of string * string
    | Div of Attrs.t * t list
    | UnhandledBlock of Yojson_with_sexp.t
  [@@deriving sexp, equal, quickcheck, compare]
end

module More = struct
  module Block = Block'

  type t = P.t =
    { api_version : int list [@sexp.list]
    ; meta : Yojson_with_sexp.t
    ; blocks : Block.t list [@sexp.list]
    }
  [@@deriving sexp, equal, quickcheck, compare]

  module Metadata = struct
    type nonrec t = t

    let sexp_of_t t = Yojson_with_sexp.sexp_of_t t.meta
    let compare a b = Yojson_with_sexp.compare a.meta b.meta
    let equal a b = Yojson_with_sexp.equal a.meta b.meta
    let lookup_bool t ~field = Option.try_with (fun () -> P.meta_bool t field)
    let lookup_string t ~field = Option.try_with (fun () -> P.meta_string t field)
  end

  let of_pandoc_ast = P.of_json
  let of_pandoc_ast_string s = s |> Yojson.Basic.from_string |> of_pandoc_ast
  let to_pandoc_ast = P.to_json
  let to_pandoc_ast_string a = a |> to_pandoc_ast |> Yojson.Basic.to_string
  let pandoc_api_version { api_version; _ } = api_version
  let metadata = Fn.id
  let top_level_blocks { blocks; _ } = blocks
  let with_top_level_blocks t blocks = { t with blocks }

  let exists ~inline ~block t =
    let rec visit_block (b : Block.t) =
      block b
      ||
      match b with
      | BulletList blocks -> blocks |> List.exists ~f:(List.exists ~f:visit_block)
      | Header (_, _, inlines) -> List.exists inlines ~f:visit_inline
      | OrderedList (_, blocks) -> blocks |> List.exists ~f:(List.exists ~f:visit_block)
      | Para inlines -> List.exists inlines ~f:visit_inline
      | Plain inlines -> List.exists inlines ~f:visit_inline
      | Div (_, blocks) -> List.exists blocks ~f:visit_block
      | RawBlock _ | CodeBlock _ | UnhandledBlock _ -> false
    and visit_inline (i : Inline.t) =
      inline i
      ||
      match i with
      | Emph inlines -> List.exists inlines ~f:visit_inline
      | Image (_, inlines, _) -> List.exists inlines ~f:visit_inline
      | Link (_, inlines, _) -> List.exists inlines ~f:visit_inline
      | Quoted (_, inlines) -> List.exists inlines ~f:visit_inline
      | SmallCaps inlines -> List.exists inlines ~f:visit_inline
      | Code _ | RawInline _ | Space | Str _ | UnhandledInline _ -> false
    in
    List.exists t.blocks ~f:visit_block
  ;;

  let for_all ~inline ~block t =
    let rec visit_block (b : Block.t) =
      block b
      &&
      match b with
      | BulletList blocks -> blocks |> List.for_all ~f:(List.for_all ~f:visit_block)
      | Header (_, _, inlines) -> List.for_all inlines ~f:visit_inline
      | OrderedList (_, blocks) -> blocks |> List.for_all ~f:(List.for_all ~f:visit_block)
      | Para inlines -> List.for_all inlines ~f:visit_inline
      | Plain inlines -> List.for_all inlines ~f:visit_inline
      | Div (_, blocks) -> List.for_all blocks ~f:visit_block
      | CodeBlock _ | RawBlock _ | UnhandledBlock _ -> true
    and visit_inline (i : Inline.t) =
      inline i
      &&
      match i with
      | Emph inlines -> List.for_all inlines ~f:visit_inline
      | Image (_, inlines, _) -> List.for_all inlines ~f:visit_inline
      | Link (_, inlines, _) -> List.for_all inlines ~f:visit_inline
      | Quoted (_, inlines) -> List.for_all inlines ~f:visit_inline
      | SmallCaps inlines -> List.for_all inlines ~f:visit_inline
      | Code _ | RawInline _ | Space | Str _ | UnhandledInline _ -> true
    in
    List.for_all t.blocks ~f:visit_block
  ;;

  let concat_map ~inline ~block t =
    let rec visit_block (b : Block.t) : Block.t list =
      block
        (match b with
        | BulletList blocks ->
          Block.BulletList (blocks |> List.map ~f:(List.concat_map ~f:visit_block))
        | Header (a, b, inlines) -> Header (a, b, List.concat_map inlines ~f:visit_inline)
        | OrderedList (a, blocks) ->
          OrderedList (a, blocks |> List.map ~f:(List.concat_map ~f:visit_block))
        | Para inlines -> Para (List.concat_map inlines ~f:visit_inline)
        | Plain inlines -> Plain (List.concat_map inlines ~f:visit_inline)
        | Div (a, blocks) -> Div (a, List.concat_map blocks ~f:visit_block)
        | (CodeBlock _ | RawBlock _ | UnhandledBlock _) as other -> other)
    and visit_inline (i : Inline.t) : Inline.t list =
      inline
        (match i with
        | Emph inlines -> Inline.Emph (List.concat_map inlines ~f:visit_inline)
        | Image (a, inlines, c) -> Image (a, List.concat_map inlines ~f:visit_inline, c)
        | Link (a, inlines, c) -> Link (a, List.concat_map inlines ~f:visit_inline, c)
        | Quoted (a, inlines) -> Quoted (a, List.concat_map inlines ~f:visit_inline)
        | SmallCaps inlines -> SmallCaps (List.concat_map inlines ~f:visit_inline)
        | (Code _ | RawInline _ | Space | Str _ | UnhandledInline _) as other -> other)
    in
    let blocks = List.concat_map t.blocks ~f:visit_block in
    { t with blocks }
  ;;

  let folding_concat_map ~inline ~block ~init t =
    let acc = ref init in
    concat_map
      ~inline:(fun i ->
        let next, inlines = inline !acc i in
        acc := next;
        inlines)
      ~block:(fun b ->
        let next, blocks = block !acc b in
        acc := next;
        blocks)
      t
  ;;

  let folding_map ~inline ~block ~init t =
    folding_concat_map
      t
      ~init
      ~inline:(fun acc i ->
        let acc, i = inline acc i in
        acc, [ i ])
      ~block:(fun acc b ->
        let acc, b = block acc b in
        acc, [ b ])
  ;;

  let filter ~inline ~block t =
    concat_map
      ~inline:(fun i -> if inline i then [ i ] else [])
      ~block:(fun b -> if block b then [ b ] else [])
      t
  ;;

  let map ~inline ~block t =
    concat_map ~inline:(fun i -> [ inline i ]) ~block:(fun b -> [ block b ]) t
  ;;

  let iter ?(inline = Fn.ignore) ?(block = Fn.ignore) t =
    let (_ : t) =
      map
        t
        ~inline:(fun i ->
          inline i;
          i)
        ~block:(fun b ->
          block b;
          b)
    in
    ()
  ;;
end

module List_item = struct
  open More
  module Block = Block'

  type t = Block.t list [@@deriving sexp, equal, quickcheck, compare]

  type details =
    | Empty
    | Normal of Block.t list [@sexp.list]
    | No_inline_strings of Block.t list [@sexp.list]
    | Unchecked of Block.t list [@sexp.list]
    | Checked of Block.t list [@sexp.list]
  [@@deriving sexp, equal, quickcheck, compare]

  let kind = function
    | Empty -> `Normal
    | Normal _ -> `Normal
    | No_inline_strings _ -> `Normal
    | Unchecked _ -> `Unchecked
    | Checked _ -> `Checked
  ;;

  let checked_str = "\226\152\146"
  let unchecked_str = "\226\152\144"
  let with_blocks blocks ~f = (f { api_version = []; meta = `Null; blocks }).blocks

  let conceal = function
    | Empty -> []
    | Normal blocks | No_inline_strings blocks | Unchecked blocks | Checked blocks ->
      blocks
  ;;

  let reveal (blocks : Block.t list) =
    match blocks with
    | [] -> Empty
    | ( BulletList _
      | CodeBlock _
      | Header _
      | OrderedList _
      | RawBlock _
      | Div _
      | UnhandledBlock _ )
      :: _ -> No_inline_strings blocks
    | (Para spans | Plain spans) :: rest ->
      (match spans, rest with
      | [], [] -> Empty
      | [], rest -> No_inline_strings rest
      | Str s :: _, _ when String.equal s unchecked_str -> Unchecked blocks
      | Str s :: _, _ when String.equal s checked_str -> Checked blocks
      | _ -> Normal blocks)
  ;;

  let empty_to_checked () = Checked [ Block.Plain [ Str checked_str; Space ] ]
  let empty_to_unchecked () = Unchecked [ Block.Plain [ Str unchecked_str; Space ] ]

  let normal_to_checked_or_unchecked checked_or_unchecked ~f blocks =
    blocks
    |> with_blocks ~f:(fun t ->
           folding_concat_map
             t
             ~init:true
             ~block:(fun acc b -> acc, [ b ])
             ~inline:(fun acc i ->
               match acc with
               | true -> false, [ Str checked_or_unchecked; Space; i ]
               | false -> false, [ i ]))
    |> f
  ;;

  let normal_to_checked =
    normal_to_checked_or_unchecked checked_str ~f:(fun b -> Checked b)
  ;;

  let normal_to_unchecked =
    normal_to_checked_or_unchecked unchecked_str ~f:(fun b -> Unchecked b)
  ;;

  let no_inline_to_checked_or_unchecked checked_or_unchecked ~f blocks =
    f (Block.Plain [ Str checked_or_unchecked ] :: blocks)
  ;;

  let no_inline_to_checked =
    no_inline_to_checked_or_unchecked checked_str ~f:(fun b -> Checked b)
  ;;

  let no_inline_to_unchecked =
    no_inline_to_checked_or_unchecked unchecked_str ~f:(fun b -> Unchecked b)
  ;;

  let checked_or_unchecked_to_checked_or_unchecked checked_or_unchecked ~f blocks =
    blocks
    |> with_blocks ~f:(fun t ->
           folding_concat_map
             t
             ~init:true
             ~block:(fun acc b -> acc, [ b ])
             ~inline:(fun acc i ->
               match acc with
               | true -> false, [ Str checked_or_unchecked ]
               | false -> false, [ i ]))
    |> f
  ;;

  let checked_or_unchecked_to_checked =
    checked_or_unchecked_to_checked_or_unchecked checked_str ~f:(fun b -> Checked b)
  ;;

  let checked_or_unchecked_to_unchecked =
    checked_or_unchecked_to_checked_or_unchecked unchecked_str ~f:(fun b -> Unchecked b)
  ;;

  let checked_or_unchecked_to_normal blocks =
    match blocks with
    | [ Block.Plain [ Str _ ] ] -> Empty
    | Block.Plain [ Str _ ] :: blocks -> No_inline_strings blocks
    | other ->
      let blocks =
        with_blocks other ~f:(fun t ->
            folding_concat_map
              t
              ~init:true
              ~block:(fun acc b -> acc, [ b ])
              ~inline:(fun acc i ->
                match acc with
                | true -> false, []
                | false -> false, [ i ]))
      in
      (match blocks with
      | (Plain [] | Para []) :: rest -> rest
      | (Plain [ Space ] | Para [ Space ]) :: rest -> rest
      | (Plain (Space :: more) | Para (Space :: more)) :: rest -> Plain more :: rest
      | other -> other)
      |> Normal
  ;;

  let check = function
    | Empty -> empty_to_checked ()
    | Normal blocks -> normal_to_checked blocks
    | No_inline_strings blocks -> no_inline_to_checked blocks
    | Checked _ as t -> t
    | Unchecked blocks -> checked_or_unchecked_to_checked blocks
  ;;

  let uncheck = function
    | Empty -> empty_to_unchecked ()
    | Normal blocks -> normal_to_unchecked blocks
    | No_inline_strings blocks -> no_inline_to_unchecked blocks
    | Unchecked _ as t -> t
    | Checked blocks -> checked_or_unchecked_to_unchecked blocks
  ;;

  let remove_checkbox = function
    | (Empty | Normal _ | No_inline_strings _) as t -> t
    | Checked blocks | Unchecked blocks -> checked_or_unchecked_to_normal blocks
  ;;
end

include More

module Block = struct
  include Block'
end
