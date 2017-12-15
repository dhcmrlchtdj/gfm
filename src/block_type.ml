open Batteries

type block =
    | NullBlock
    | AtxHeader of UTF8.t
    | SetexHeader of UTF8.t * UTF8.t
    | CodeBlock of UTF8.t list
    | BlockQuote of UTF8.t list
    | HorizontalRule
    | UnorderedList of UTF8.t list
    | OrderedList of UTF8.t list
    | Paragraph of UTF8.t
    | ReferenceResolutionBlock of UTF8.t list
