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
