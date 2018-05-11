open Batteries

type simpleBlock =
    | NullBlock
    | AtxHeader of string
    | SetexHeader of string * string
    | CodeBlock of string list
    | BlockQuote of string list
    | HorizontalRule
    | UnorderedList of string list * int
    | OrderedList of string list * int
    | Paragraph of string
    | ReferenceResolutionBlock of string list

type spanElement =
    | Stext of string
    | Scode of string
    | Semphasis of spanElement list
    | Sstrong of spanElement list
    | Slink of spanElement list * string
    | Simage of string * string

and blockElement =
    | Bheading of int * spanElement list
    | Bparagraph of spanElement list
    | Bhorizontal
    | Bcode of string
    | Bblockquote of blockElement list
    | BorderedList of blockElement list
    | BunorderedList of blockElement list
    | Bnull

type md_ast = blockElement list
