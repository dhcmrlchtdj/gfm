open Batteries
open TypeAst

let html_render (input: md_ast) : string =
    let sprintf = Printf.sprintf in
    let rec span_to_html : spanElement -> string = function
        | Ilink (alt, url) -> sprintf "<a href=\"%s\">%s</a>" url alt
        | Iimage (alt, url) -> sprintf "<img src=\"%s\" alt=\"%s\"" url alt
        | Istrong t -> sprintf "<strong>%s</strong>" t
        | Iemphasis t -> sprintf "<em>%s</em>" t
        | Icode t -> sprintf "<code>%s</code>" t
        | Itext t -> sprintf "%s" t
    and spans_to_html (spans: spanElement list) : string =
        spans |> List.map span_to_html |> String.concat ""
    in
    let rec block_to_html : blockElement -> string = function
        | Bheading (i, t) -> sprintf "<h%d>%s</h%d>" i (spans_to_html t) i
        | Bparagraph p -> sprintf "<p>%s</p>" (spans_to_html p)
        | Bhorizontal -> sprintf "<hr/>"
        | Bcode c -> sprintf "<pre><code>%s</code></pre>" c
        | Bblockquote b -> sprintf "<blockquote>%s</blockquote>" (blocks_to_html b)
        | BorderedList ol -> sprintf "<ol>%s</ol>" (listItems_to_html ol)
        | BunorderedList ul -> sprintf "<ul>%s</ul>" (listItems_to_html ul)
        | Bnull -> ""
    and listItem_to_html (item: blockElement) =
        sprintf "<li>%s</li>" (block_to_html item)
    and listItems_to_html (blocks: blockElement list) : string =
        blocks |> List.map listItem_to_html |> String.concat ""
    and blocks_to_html (blocks: blockElement list) : string =
        blocks |> List.map block_to_html |> String.concat "\n"
    in
    let md_to_html (md: md_ast) : string = sprintf "%s\n" (blocks_to_html md) in
    md_to_html input
