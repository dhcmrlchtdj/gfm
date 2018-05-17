open Batteries
open Types

let html_render (input: md_ast) : string =
    let sprintf = Printf.sprintf in
    let rec span_to_html : spanElement -> string = function
        | Slink (text, url) ->
            sprintf "<a href=\"%s\">%s</a>" url (spans_to_html text)
        | Simage (alt, url) -> sprintf "<img src=\"%s\" alt=\"%s\"" url alt
        | Sstrong t -> sprintf "<strong>%s</strong>" (spans_to_html t)
        | Semphasis t -> sprintf "<em>%s</em>" (spans_to_html t)
        | Scode t -> sprintf "<code>%s</code>" t
        | Stext t -> sprintf "%s" t
    and spans_to_html (spans: spanElement list) : string =
        spans |> List.map span_to_html |> String.concat ""
    in
    let rec block_to_html : blockElement -> string = function
        | Bline -> sprintf "<hr/>"
        | Bheading (i, t) -> sprintf "<h%d>%s</h%d>" i (spans_to_html t) i
        | Bparagraph p -> sprintf "<p>%s</p>" (spans_to_html p)
        | Bcode c -> sprintf "<pre><code>%s</code></pre>" c
        | Bquote b -> sprintf "<blockquote>\n%s\n</blockquote>" (blocks_to_html b)
        | Blist ul -> sprintf "<ul>\n%s\n</ul>" (listItems_to_html ul)
        | Bseq s -> sprintf "%s" (spans_to_html s)
    and listItem_to_html (item: blockElement) =
        sprintf "<li>%s</li>" (block_to_html item)
    and listItems_to_html (blocks: blockElement list) : string =
        blocks |> List.map listItem_to_html |> String.concat "\n"
    and blocks_to_html (blocks: blockElement list) : string =
        blocks |> List.map block_to_html |> String.concat "\n"
    in
    let md_to_html (md: md_ast) : string = sprintf "%s\n" (blocks_to_html md) in
    md_to_html input