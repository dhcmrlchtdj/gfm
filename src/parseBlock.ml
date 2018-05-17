open Batteries
open Types

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

let parse (input: string list) : blockElement list =
    let rec aux acc = function
        | [] -> List.rev acc
        | "" :: t -> aux acc t
        | h :: t when String.starts_with h "---" ->
            let block = Bline in
            aux (block :: acc) t
        | h :: t when String.starts_with h "#" ->
            let block =
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
                            | _ -> failwith "never"
            in
            aux (block :: acc) t
        | h :: t when String.starts_with h "```" ->
            let lines, tt = advance_code_block t in
            let codes = lines |> List.map html_encode |> String.concat "\n" in
            let block = Bcode codes in
            aux (block :: acc) tt
        | h :: t when String.starts_with h "> " ->
            let lines, tt = advance_quote_block (h :: t) in
            let quote = aux [] lines in
            let block = Bquote quote in
            aux (block :: acc) tt
        | h :: t when String.starts_with h "- " ->
            let lines, tt = advance_list_block (h :: t) in
            let lst =
                lines |> List.map (fun line -> Bseq (ParseSpan.parse line))
            in
            let block = Blist lst in
            aux (block :: acc) tt
        | h :: t ->
            let block = Bparagraph (ParseSpan.parse h) in
            aux (block :: acc) t
    in
    aux [] input
