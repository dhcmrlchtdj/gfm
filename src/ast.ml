open Batteries

type inlineElement =
    | Ilink of UTF8.t * UTF8.t
    (*alt * url*)
    | Iimage of UTF8.t * UTF8.t
    (*alt * url*)
    | Istrong of UTF8.t
    | Iemphasis of UTF8.t
    | Icode of UTF8.t
    | Itext of UTF8.t

and inlineElements = inlineElement list

and blockElement =
    | Bheading of int * inlineElements
    | Bparagraph of inlineElements
    | Bhorizontal
    | Bcode of UTF8.t
    | Bblockquote of blockElements
    | BorderedList of blockElements
    | BunorderedList of blockElements

and blockElements = blockElement list

type md_ast = blockElements
