open Batteries
open R.Infix
open Block_type
module P = Printf

let re_header_text = R.compile "^(#+)(.*[^#])#*$"

let re_header_empty = R.compile "^(#+)$"

let html_encode (s: string) : string =
    let f = function
        | '<' -> "&lt;"
        | '>' -> "&gt;"
        | '&' -> "&amp;"
        | '"' -> "&quot;"
        | '\'' -> "&#x27;"
        | c -> Char.escaped c
    in
    s |> String.to_list |> List.map f |> String.join ""


let interp_block refs = function
    | ReferenceResolutionBlock _ -> []
    | NullBlock -> ["\n"]
    | AtxHeader h -> (
            match R.exec re_header_text h with
                | Some [|_; x; y|] ->
                    let len = Int.min 6 (String.length x) in
                    let open_tag = P.sprintf "<h%d>" len in
                    let close_tag = P.sprintf "</h%d>\n" len in
                    let title = String.trim y in
                    (* TODO *)
                    [open_tag; title; close_tag]
                | _ ->
                    match R.exec re_header_empty h with
                        | Some [|_; x|] ->
                            let len = Int.min 6 (String.length x) in
                            let open_tag = P.sprintf "<h%d>" len in
                            let close_tag = P.sprintf "</h%d>\n" len in
                            [open_tag; close_tag]
                        | _ -> failwith "never" )
    | SetexHeader (h, l) -> (
            (* TODO *)
            let title = String.trim h in
            match l.[0] with
                | '=' -> ["<h1>"; title; "</h1>\n"]
                | '-' -> ["<h2>"; title; "</h2>\n"]
                | _ -> failwith "never" )
    | CodeBlock lines ->
        let open_tag = "<pre><code>" in
        let close_tag = "</code></pre>\n" in
        let f line acc =
            let t = line |> String.lchop ~n:4 |> html_encode in
            t :: "\n" :: acc
        in
        let l = List.fold_right f lines [close_tag] in
        open_tag :: l
    | BlockQuote _ -> ["TODO"]
    | HorizontalRule -> ["<hr/>\n"]
    | UnorderedList lines ->
        let open_tag = "<ul>\n" in
        let close_tag = "</ul>\n" in
        let f line acc =
            (* TODO *)
            let t = P.sprintf "<li>%s</li>\n" line in
            t :: acc
        in
        let l = List.fold_right f lines [close_tag] in
        open_tag :: l
    | OrderedList lines ->
        let open_tag = "<ol>\n" in
        let close_tag = "</ol>\n" in
        let f line acc =
            (* TODO *)
            let t = P.sprintf "<li>%s</li>\n" line in
            t :: acc
        in
        let l = List.fold_right f lines [close_tag] in
        open_tag :: l
    | Paragraph line ->
        (* TODO *)
        let p = line in
        ["<p>"; p; "</p>\n"]


let interp blocks =
    let refs =
        blocks
        |> List.filter (function ReferenceResolutionBlock _ -> true | _ -> false)
    in
    let f = interp_block refs in
    blocks |> List.map f |> List.flatten |> String.join ""

