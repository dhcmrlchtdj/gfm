open Batteries
open Regexp.Infix
open TypeSimpleBlock
open TypeAst
module P = Printf

let re_header_text = Regexp.compile "^(#+)(.*[^#])#*$"

let re_header_empty = Regexp.compile "^(#+)$"

let re_space = Regexp.compile "^( *)"

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

let lchop_space_at_most (starter_len: int) (line: UTF8.t) : UTF8.t =
    let m = Regexp.exec re_space line in
    match m with
        | Some [|_; sp|] ->
            let len = String.length sp in
            let l = Int.min len starter_len in
            String.lchop ~n:l line
        | _ -> line

let unordered_list_item_process (starter_len: int) (lines: UTF8.t list)
    : UTF8.t list =
    let lchop = lchop_space_at_most starter_len in
    match lines with
        | [] -> []
        | h :: t ->
            let hh = String.lchop ~n:starter_len h in
            hh :: (t |> List.map lchop)

let ordered_list_item_process (starter_len: int) (lines: UTF8.t list)
    : UTF8.t list =
    let lchop = lchop_space_at_most starter_len in
    match lines with
        | [] -> []
        | h :: t ->
            let hh = String.lchop ~n:starter_len h in
            hh :: (t |> List.map lchop)

let simple_block_to_block (blocks: simpleBlock list) : md_ast =
    let aux : simpleBlock -> blockElement = function
        | ReferenceResolutionBlock _ -> Bnull
        | NullBlock -> Bnull
        | HorizontalRule -> Bhorizontal
        | AtxHeader h -> (
                match Regexp.exec re_header_text h with
                    | Some [|_; x; y|] ->
                        let len = Int.min 6 (String.length x) in
                        let title = y |> String.trim |> ParseSpan.parse in
                        Bheading (len, title)
                    | _ ->
                        match Regexp.exec re_header_empty h with
                            | Some [|_; x|] ->
                                let len = Int.min 6 (String.length x) in
                                Bheading (len, [])
                            | _ -> failwith "never" )
        | SetexHeader (h, l) -> (
                let title = h |> String.trim |> ParseSpan.parse in
                match l.[0] with
                    | '=' -> Bheading (1, title)
                    | '-' -> Bheading (2, title)
                    | _ -> failwith "never" )
        | Paragraph line ->
            let p = line |> ParseSpan.parse in
            Bparagraph p
        | CodeBlock lines ->
            let f line acc =
                let t = line |> String.lchop ~n:4 |> html_encode in
                t :: "\n" :: acc
            in
            let codes = List.fold_right f lines [] |> String.concat "" in
            Bcode codes
        | BlockQuote t -> ParseSpan.parse t
        | UnorderedList (lines, starter_len) ->
            let open_tag = "<ul>\n" in
            let close_tag = "</ul>\n" in
            let f line acc =
                (* TODO *)
                let t = line |> P.sprintf "<li>%s</li>\n" in
                t :: acc
            in
            let l =
                lines |> unordered_list_item_process starter_len
                |> fun x -> List.fold_right f x [close_tag]
            in
            open_tag :: l
        | OrderedList (lines, starter_len) ->
            let open_tag = "<ol>\n" in
            let close_tag = "</ol>\n" in
            let f line acc =
                (* TODO *)
                let t = line |> P.sprintf "<li>%s</li>\n" in
                t :: acc
            in
            let l =
                lines |> ordered_list_item_process starter_len
                |> fun x -> List.fold_right f x [close_tag]
            in
            open_tag :: l
    in
    blocks |> List.map aux
