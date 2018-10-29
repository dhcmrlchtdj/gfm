open Batteries
open Types

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
            sprintf "<img src='%s' alt='%s'>" (encode url) (encode alt)
        | Sstrong t -> sprintf "<strong>%s</strong>" (spans_to_html t)
        | Semphasis t -> sprintf "<em>%s</em>" (spans_to_html t)
        | Sdelete t -> sprintf "<del>%s</del>" (spans_to_html t)
        | Scode t -> sprintf "<code>%s</code>" (encode t)
        | Stext t -> encode t
    and spans_to_html (spans : spanElement list) : string =
        spans |> List.map span_to_html |> String.concat ""
    in
    let rec block_to_html : blockElement -> string = function
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
    ".markdown-body{-webkit-text-size-adjust:100%;color:#24292e;font-family:sans-serif;font-size:16px;line-height:1.5;word-wrap:break-word;min-width:200px;max-width:790px;margin:0 auto;padding:30px}.markdown-body a{background-color:transparent;color:#0366d6;text-decoration:none}.markdown-body a:active,.markdown-body a:hover{outline-width:0}.markdown-body strong{font-weight:600}.markdown-body h1{margin:.67em 0}.markdown-body img{border-style:none;max-width:100%;box-sizing:content-box;background-color:#fff}.markdown-body hr{box-sizing:content-box;overflow:hidden;background:0 0;height:.25em;padding:0;margin:24px 0;background-color:#e1e4e8;border:0;border-bottom-color:#eee}.markdown-body *{box-sizing:border-box}.markdown-body a:hover{text-decoration:underline}.markdown-body hr::before,.markdown-body::before{display:table;content:''}.markdown-body hr::after,.markdown-body::after{display:table;clear:both;content:''}.markdown-body blockquote{margin:0}.markdown-body code{padding:.2em .4em;margin:0;font-size:85%;background-color:rgba(27,31,35,.05);border-radius:3px}.markdown-body code,.markdown-body pre{font-family:monospace}.markdown-body>:first-child{margin-top:0!important}.markdown-body>:last-child{margin-bottom:0!important}.markdown-body a:not([href]){color:inherit;text-decoration:none}.markdown-body blockquote,.markdown-body p,.markdown-body pre,.markdown-body ul{margin-top:0;margin-bottom:16px}.markdown-body blockquote{padding:0 1em;color:#6a737d;border-left:.25em solid #dfe2e5}.markdown-body blockquote>:first-child{margin-top:0}.markdown-body blockquote>:last-child{margin-bottom:0}.markdown-body h1,.markdown-body h2,.markdown-body h3,.markdown-body h4,.markdown-body h5,.markdown-body h6{margin-top:24px;margin-bottom:16px;font-weight:600;line-height:1.25}.markdown-body h1,.markdown-body h2{padding-bottom:.3em;font-size:2em;border-bottom:1px solid #eaecef}.markdown-body h2{font-size:1.5em}.markdown-body h3{font-size:1.25em}.markdown-body h4{font-size:1em}.markdown-body h5{font-size:.875em}.markdown-body h6{font-size:.85em;color:#6a737d}.markdown-body ul{padding-left:2em}.markdown-body ul ul{margin-top:0;margin-bottom:0}.markdown-body li{word-wrap:break-all}.markdown-body li>p{margin-top:16px}.markdown-body li+li{margin-top:.25em}.markdown-body pre{word-wrap:normal;padding:16px;overflow:auto;font-size:85%;line-height:1.45;background-color:#f6f8fa;border-radius:3px}.markdown-body pre>code{padding:0;margin:0;font-size:100%;word-break:normal;white-space:pre;background:0 0;border:0}.markdown-body pre code{display:inline;max-width:auto;padding:0;margin:0;overflow:visible;line-height:inherit;word-wrap:normal;background-color:transparent;border:0}"


let spectre_style =
    ".markdown-body{color:#545454;font-family:-apple-system,system-ui,BlinkMacSystemFont,Segoe UI,Roboto,PingFang SC,Hiragino Sans GB,Microsoft YaHei,Helvetica Neue,sans-serif;font-size:16px;line-height:1.75;padding:16px 20px;-webkit-text-size-adjust:100%;min-width:200px;max-width:790px;margin:0 auto}.markdown-body h1,.markdown-body h2,.markdown-body h3,.markdown-body h4,.markdown-body h5,.markdown-body h6{color:#3b3b3b;font-weight:700;line-height:1.5;margin-bottom:24px;margin-top:8px}.markdown-body h1+hr,.markdown-body h2+hr,.markdown-body h3+hr,.markdown-body h4+hr,.markdown-body h5+hr,.markdown-body h6+hr{margin-top:-8px}.markdown-body h1{font-size:28px}.markdown-body h2{font-size:24px}.markdown-body h3{font-size:22px}.markdown-body h4{font-size:20px}.markdown-body h5{font-size:18px}.markdown-body h6{font-size:16px}.markdown-body p{-webkit-hyphens:auto;-ms-hyphens:auto;hyphens:auto;margin:0 0 32px}.markdown-body a{color:#0056ff;text-decoration:none}.markdown-body a:active,.markdown-body a:focus,.markdown-body a:hover{text-decoration:underline}.markdown-body a,.markdown-body ins,.markdown-body u{-webkit-text-decoration-skip:ink edges;text-decoration-skip:ink edges}.markdown-body del+del,.markdown-body del+s,.markdown-body ins+ins,.markdown-body ins+u,.markdown-body s+del,.markdown-body s+s,.markdown-body u+ins,.markdown-body u+u{margin-left:.125em}.markdown-body hr{background:#ededed;border:none;display:block;height:1px;margin:8px 0 24px;overflow:hidden}.markdown-body hr:after{clear:both;content:'';display:table}.markdown-body ol,.markdown-body ul{margin:32px 0 32px 32px;padding:0}.markdown-body ol ol,.markdown-body ol ul,.markdown-body ul ol,.markdown-body ul ul{margin:16px 0 16px 32px}.markdown-body ol li,.markdown-body ul li{margin-top:8px}.markdown-body ul{list-style:disc inside}.markdown-body ul ul{list-style-type:circle}.markdown-body ol{list-style:decimal inside}.markdown-body ol ol{list-style-type:lower-alpha}.markdown-body img{border:0;height:auto;max-width:100%}.markdown-body img+em{color:#a1a1a1;display:inline-block;font-size:14px;text-align:center;width:100%}.markdown-body blockquote{background:#fafafa;margin:32px 0;padding:24px}.markdown-body blockquote:before{color:#ededed;content:open-quote;font-family:arial;font-size:6em;line-height:.1em;vertical-align:-.5em}.markdown-body blockquote p:last-child{margin-bottom:0}.markdown-body table{border-collapse:collapse;border-spacing:0;margin:32px 0;text-align:left;width:100%}.markdown-body table tbody tr:nth-of-type(odd){background:#fafafa}.markdown-body table td,.markdown-body table th{border-bottom:1px solid #ededed;padding:8px 4px}.markdown-body table th{border-bottom-width:2px}.markdown-body code{background:#fafafa;font-family:SF Mono,Segoe UI Mono,Roboto Mono,Menlo,Courier,monospace;font-size:.9em;padding:.05em}.markdown-body pre{background:#fafafa;margin:32px 0;overflow-x:auto}.markdown-body pre code{color:inherit;display:block;line-height:inherit;padding:16px}"


let with_style (body : string) : string =
    let elements =
        [ "<html>"
        ; "<head>"
        ; "<meta charset='utf-8'>"
        ; "<meta name='viewport' content='width=device-width,initial-scale=1'>"
        ; "<style>"
        ; spectre_style
        ; "</style>"
        ; "</head>"
        ; "<body class='markdown-body'>"
        ; body
        ; "</body>"
        ; "</html>" ]
    in
    String.concat "" elements
