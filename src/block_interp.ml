open Batteries
open R.Infix
open Block_type
module P = Printf

let re_header_text = R.compile "^(#+)(.*[^#])#*$"

let re_header_empty = R.compile "^(#+)$"

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
            let t = String.lchop ~n:4 line in
            t :: "\n" :: acc
        in
        let l = List.fold_right f lines [close_tag] in
        open_tag :: l
    | BlockQuote _ -> ["TODO"]
    | HorizontalRule -> ["<hr/>\n"]
    | UnorderedList _ -> ["TODO"]
    | OrderedList _ -> ["TODO"]
    | Paragraph _ -> ["TODO"]


let interp blocks =
    let refs =
        blocks
        |> List.filter (function ReferenceResolutionBlock _ -> true | _ -> false)
    in
    let f = interp_block refs in
    blocks |> List.map f |> List.flatten |> String.join ""

