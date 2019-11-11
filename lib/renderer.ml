open Types
module String = CCString

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

let html_render (input : md_ast) : string =
    let sprintf = Printf.sprintf in
    let rec span_to_html : spanElement -> string = function
        | Slink (text, url) ->
            sprintf "<a href='%s'>%s</a>" (encode url) (spans_to_html text)
        | Simage (alt, url) ->
            sprintf "<img src='%s' alt='%s'/>" (encode url) (encode alt)
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
              "<pre><code class='language-%s'>%s</code></pre>"
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

let gfm_style =
    ".markdown-body{-webkit-text-size-adjust:100%;color:#24292e;font-family:sans-serif;font-size:16px;line-height:1.5;margin:0 auto;max-width:790px;min-width:200px;padding:30px;word-wrap:break-word}.markdown-body,.markdown-body *{box-sizing:border-box}.markdown-body>:first-child{margin-top:0!important}.markdown-body>:last-child{margin-bottom:0!important}.markdown-body a{background-color:transparent;color:#0366d6;text-decoration:none}.markdown-body a:active,.markdown-body a:hover{text-decoration:underline}.markdown-body a:not([href]){color:inherit}.markdown-body strong{font-weight:700}.markdown-body img{border-style:none;max-width:100%;background-color:#fff}.markdown-body br{display:none}.markdown-body hr{overflow:hidden;background:0 0;height:2px;padding:0;margin:0 0 16px;background-color:#e1e4e8;border:0;border-bottom-color:#eee}.markdown-body hr::before,.markdown-body::before{display:table;content:''}.markdown-body hr::after,.markdown-body::after{display:table;clear:both;content:''}.markdown-body p{margin-top:0;margin-bottom:16px}.markdown-body p+p{margin-top:-16px}.markdown-body blockquote{margin:0 0 16px;padding:0 16px;color:#6a737d;border-left:4px solid #dfe2e5}.markdown-body blockquote>:first-child{margin-top:0}.markdown-body blockquote>:last-child{margin-bottom:0}.markdown-body h1,.markdown-body h2,.markdown-body h3,.markdown-body h4,.markdown-body h5,.markdown-body h6{margin-top:16px;margin-bottom:16px;font-weight:700}.markdown-body h1{font-size:22px}.markdown-body h2{font-size:20px}.markdown-body h3{font-size:18px}.markdown-body h4{font-size:16px}.markdown-body h5{font-size:14px}.markdown-body h6{font-size:12px}.markdown-body ul{margin-top:0;margin-bottom:16px;padding-left:32px}.markdown-body ul ul{margin-top:0;margin-bottom:0}.markdown-body li{word-wrap:break-all}.markdown-body li>p{margin-top:16px}.markdown-body li+li{margin-top:4px}.markdown-body code,.markdown-body pre{font-family:monospace;font-size:85%;border-radius:3px}.markdown-body code{margin:0;background-color:rgba(27,31,35,.05);padding:3px 6px}.markdown-body pre{margin-top:0;margin-bottom:16px;padding:16px;word-wrap:normal;overflow:auto;line-height:1.45;background-color:#f6f8fa}.markdown-body pre>code{padding:0;margin:0;font-size:100%;word-break:normal;white-space:pre;background:0 0;border:0}.markdown-body pre code{display:inline;max-width:auto;padding:0;margin:0;overflow:visible;line-height:inherit;word-wrap:normal;background-color:transparent;border:0}"

let with_style (body : string) : string =
    let elements =
        [
          "<html>";
          "<head>";
          "<meta charset='utf-8'/>";
          "<meta name='viewport' content='width=device-width,initial-scale=1'/>";
          "<style>";
          gfm_style;
          "</style>";
          "</head>";
          "<body class='markdown-body'>";
          body;
          "</body>";
          "</html>";
        ]
    in
    String.concat "" elements
