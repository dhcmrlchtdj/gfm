open Batteries
open Ast

let html_render (input: md_ast) : string =
    let sprintf = Printf.sprintf in
    let rec inline_to_html : inlineElement -> string = function
        | Ilink (alt, url) -> sprintf "<a href=\"%s\">%s</a>" url alt
        | Iimage (alt, url) -> sprintf "<img src=\"%s\" alt=\"%s\"" url alt
        | Istrong t -> sprintf "<strong>%s</strong>" t
        | Iemphasis t -> sprintf "<em>%s</em>" t
        | Icode t -> sprintf "<code>%s</code>" t
        | Itext t -> sprintf "%s" t
    and inlines_to_html (inlines: inlineElement list) : string =
        inlines |> List.map inline_to_html |> String.concat " "
    in
    let rec block_to_html : blockElement -> string = function
        | Bheading (i, t) -> sprintf "<h%d>%s</h%d>" i (inlines_to_html t) i
        | Bparagraph p -> sprintf "<p>%s</p>" (inlines_to_html p)
        | Bhorizontal -> sprintf "<hr/>"
        | Bcode c -> sprintf "<pre><code>%s</code></pre>" c
        | Bblockquote b -> sprintf "<blockquote>%s</blockquote>" (blocks_to_html b)
        | BorderedList ol -> sprintf "<ol>%s</ol>" (listItems_to_html ol)
        | BunorderedList ul -> sprintf "<ul>%s</ul>" (listItems_to_html ul)
    and listItem_to_html (item: blockElement) =
        sprintf "<li>%s</li>" (block_to_html item)
    and listItems_to_html (blocks: blockElement list) : string =
        blocks |> List.map listItem_to_html |> String.concat ""
    and blocks_to_html (blocks: blockElement list) : string =
        blocks |> List.map block_to_html |> String.concat "\n"
    in
    let md_to_html (md: md_ast) : string = sprintf "%s\n" (blocks_to_html md) in
    md_to_html input
