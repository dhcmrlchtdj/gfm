open! Containers
open Types

let html_render (input : md_ast) : string =
  let encode (s : string) : string =
    let f = function
      | '<' -> "&lt;"
      | '>' -> "&gt;"
      | '&' -> "&amp;"
      | '"' -> "&quot;"
      | '\'' -> "&#x27;"
      | c -> String.of_char c
    in
    s |> String.to_list |> List.map f |> String.concat ""
  in
  let sprintf = Printf.sprintf in
  let rec span_to_html : spanElement -> string = function
    | Slink (text, url) ->
      sprintf
        {|<a rel="noopener" href="%s">%s</a>|}
        (encode url)
        (spans_to_html text)
    | Simage (alt, url) ->
      sprintf {|<img src="%s" alt="%s"/>|} (encode url) (encode alt)
    | Sstrong t -> sprintf "<strong>%s</strong>" (spans_to_html t)
    | Semphasis t -> sprintf "<em>%s</em>" (spans_to_html t)
    | Sdelete t -> sprintf "<del>%s</del>" (spans_to_html t)
    | Scode t -> sprintf "<code>%s</code>" (encode t)
    | Stext t -> encode t
  and spans_to_html (spans : spanElement list) : string =
    spans |> List.map span_to_html |> String.concat ""
  in
  let rec block_to_html : blockElement -> string = function
    | Bnewline -> "<br/>"
    | Bline -> "<hr/>"
    | Bheading (i, t) -> sprintf "<h%d>%s</h%d>" i (spans_to_html t) i
    | Bparagraph p -> sprintf "<p>%s</p>" (spans_to_html p)
    | Bcode (Some l, c) ->
      sprintf
        {|<pre><code class="language-%s">%s</code></pre>|}
        (encode l)
        (encode c)
    | Bcode (None, c) -> sprintf "<pre><code>%s</code></pre>" (encode c)
    | Bquote b -> sprintf "<blockquote>%s</blockquote>" (blocks_to_html b)
    | Blist ul -> sprintf "<ul>%s</ul>" (listItems_to_html ul)
    | Bseq s -> spans_to_html s
  and listItem_to_html (item : blockElement list) =
    sprintf "<li>%s</li>" (blocks_to_html item)
  and listItems_to_html (blocks : blockElement list list) : string =
    blocks |> List.map listItem_to_html |> String.concat ""
  and blocks_to_html (blocks : blockElement list) : string =
    blocks |> List.map block_to_html |> String.concat ""
  in
  let md_to_html (md : md_ast) : string = blocks_to_html md in
  md_to_html input

let default_style = [%blob "./style-gfm.css"]

let with_style ?(style = default_style) (body : string) : string =
  let elements =
    [
      "<html>";
      "<head>";
      {|<meta charset="utf-8"/>|};
      {|<meta name="viewport" content="width=device-width,initial-scale=1"/>|};
      "<style>";
      style;
      "</style>";
      "</head>";
      {|<body class="markdown-body">|};
      body;
      "</body>";
      "</html>";
    ]
  in
  String.concat "" elements
