open Batteries

type simpleBlock =
    | NullBlock
    | AtxHeader of UTF8.t
    | SetexHeader of UTF8.t * UTF8.t
    | CodeBlock of UTF8.t list
    | BlockQuote of UTF8.t list
    | HorizontalRule
    | UnorderedList of UTF8.t list * int
    | OrderedList of UTF8.t list * int
    | Paragraph of UTF8.t
    | ReferenceResolutionBlock of UTF8.t list

type spanElement =
    | Ilink of UTF8.t * UTF8.t
    (*alt * url*)
    | Iimage of UTF8.t * UTF8.t
    (*alt * url*)
    | Istrong of UTF8.t
    | Iemphasis of UTF8.t
    | Icode of UTF8.t
    | Itext of UTF8.t

and blockElement =
    | Bheading of int * spanElement list
    | Bparagraph of spanElement list
    | Bhorizontal
    | Bcode of UTF8.t
    | Bblockquote of blockElement list
    | BorderedList of blockElement list
    | BunorderedList of blockElement list
    | Bnull

type md_ast = blockElement list
