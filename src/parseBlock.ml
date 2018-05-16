open Batteries
open Types

type simpleBlock =
    | Line
    | Code of string list
    | Paragraph of string
    | Heading of string
    | Quote of string list
    | List of string list

let advance_code_block (input: string list) =
    let rec aux acc = function
        | [] -> (List.rev acc, [])
        | "```" :: t -> (List.rev acc, t)
        | h :: t -> aux (h :: acc) t
    in
    aux [] input

let advance_quote_block (input: string list) =
    let rec aux acc = function
        | h :: t when String.starts_with h "> " ->
            let line = String.sub h 2 (String.length h - 2) in
            aux (line :: acc) t
        | [] -> (List.rev acc, [])
        | _ as t -> (List.rev acc, t)
    in
    aux [] input

let advance_list_block (input: string list) =
    let rec aux acc = function
        | h :: t when String.starts_with h "- " ->
            let line = String.sub h 2 (String.length h - 2) in
            aux (line :: acc) t
        | [] -> (List.rev acc, [])
        | _ as t -> (List.rev acc, t)
    in
    aux [] input

let lines_to_simple (input: string list) : simpleBlock list =
    let rec aux acc = function
        | [] -> List.rev acc
        | "" :: t -> aux acc t
        | h :: t when String.starts_with h "---" ->
            let block = Line in
            aux (block :: acc) t
        | h :: t when String.starts_with h "#" ->
            let block = Heading h in
            aux (block :: acc) t
        | h :: t when String.starts_with h "```" ->
            let codes, tt = advance_code_block t in
            let block = Code codes in
            aux (block :: acc) tt
        | h :: t when String.starts_with h "> " ->
            let b, tt = advance_quote_block (h :: t) in
            let block = Quote b in
            aux (block :: acc) tt
        | h :: t when String.starts_with h "- " ->
            let b, tt = advance_list_block (h :: t) in
            let block = List b in
            aux (block :: acc) tt
        | h :: t ->
            let block = Paragraph h in
            aux (block :: acc) t
    in
    aux [] input

(* *** *)

let re_header_text = Regexp.compile "^(#+)(.*[^#])#*$"

let re_header_empty = Regexp.compile "^(#+)$"

let html_encode (s: string) : string =
    let f = function
        | '<' -> "&lt;"
        | '>' -> "&gt;"
        | '&' -> "&amp;"
        | '"' -> "&quot;"
        | '\'' -> "&#x27;"
        | c -> String.of_char c
    in
    s |> String.to_list |> List.map f |> String.concat ""

let rec simple_to_block (input: simpleBlock list) : md_ast =
    let aux : simpleBlock -> blockElement = function
        | Line -> Bline
        | Paragraph line -> Bparagraph (ParseSpan.parse line)
        | Heading line -> (
                match Regexp.exec re_header_text line with
                    | Some [|_; x; y|] ->
                        let len = Int.min 6 (String.length x) in
                        let title = y |> String.trim |> ParseSpan.parse in
                        Bheading (len, title)
                    | _ ->
                        match Regexp.exec re_header_empty line with
                            | Some [|_; x|] ->
                                let len = Int.min 6 (String.length x) in
                                Bheading (len, [])
                            | _ -> failwith "never" )
        | Code lines ->
            let code = lines |> List.map html_encode |> String.concat "\n" in
            Bcode code
        | Quote lines ->
            let q = lines |> lines_to_simple |> simple_to_block in
            Bquote q
        | List lines ->
            let l = lines |> List.map (fun line -> Bseq (ParseSpan.parse line)) in
            Blist l
    in
    List.map aux input

(* *** *)

let parse (input: string list) : md_ast =
    input |> lines_to_simple |> simple_to_block
