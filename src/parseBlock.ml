open Batteries
open Regexp.Infix
open Types

let re_heading = Regexp.compile "^(#+)\\s*(.*)$"

let re_list_prefix = Regexp.compile "^(    )*- "

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
            let line = String.lchop ~n:2 h in
            aux (line :: acc) t
        | [] -> (List.rev acc, [])
        | _ as t -> (List.rev acc, t)
    in
    aux [] input

let advance_list_block (input: string list) =
    let rec aux acc = function
        | h :: t when re_list_prefix =~ h -> aux (h :: acc) t
        | [] -> (List.rev acc, [])
        | _ as t -> (List.rev acc, t)
    in
    aux [] input

let split_list_block (input: string list) =
    let rec aux acc curr = function
        | [] -> List.rev (List.rev curr :: acc)
        | h :: t when String.starts_with h "- " -> aux (List.rev curr :: acc) [h] t
        | h :: t -> aux acc (h :: curr) t
    in
    aux [] [] input

let parse_list_item (input: string list) =
    List.map (fun line -> Bseq (ParseSpan.parse line)) input

let parse (input: string list) : blockElement list =
    let rec aux acc = function
        | [] -> List.rev acc
        | "" :: t -> aux acc t
        | h :: t when String.starts_with h "---" ->
            let block = Bline in
            aux (block :: acc) t
        | h :: t when String.starts_with h "#" ->
            let block =
                match Regexp.exec re_heading h with
                    | Some [|_; x; y|] ->
                        let len = Int.min 6 (String.length x) in
                        let title = y |> String.trim |> ParseSpan.parse in
                        Bheading (len, title)
                    | _ -> failwith "never"
            in
            aux (block :: acc) t
        | h :: t when String.starts_with h "```" ->
            let l = h |> String.lchop ~n:3 |> String.trim in
            let lang = if l = "" then None else Some l in
            let lines, tt = advance_code_block t in
            let codes = lines |> List.map html_encode |> String.concat "\n" in
            let block = Bcode (lang, codes) in
            aux (block :: acc) tt
        | h :: t when String.starts_with h "> " ->
            let lines, tt = advance_quote_block (h :: t) in
            let quote = aux [] lines in
            let block = Bquote quote in
            aux (block :: acc) tt
        | h :: t when String.starts_with h "- " ->
            let lines, tt = advance_list_block (h :: t) in
            let list_items = split_list_block lines in
            let lst = list_items |> List.map parse_list_item in
            let block = Blist lst in
            aux (block :: acc) tt
        | h :: t ->
            let block = Bparagraph (ParseSpan.parse h) in
            aux (block :: acc) t
    in
    aux [] input
