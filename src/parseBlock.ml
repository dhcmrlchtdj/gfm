open Batteries
open Regexp.Infix
open Types

let re_heading = Regexp.compile "^(#+)\\s*(.*)$"

let re_list_prefix = Regexp.compile "^(    )*- "

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
    let read_block (input: string list) : string list * string list =
        let rec aux acc = function
            | h :: t when re_list_prefix =~ h -> aux (h :: acc) t
            | [] -> (List.rev acc, [])
            | _ as t -> (List.rev acc, t)
        in
        aux [] input
    in
    let split_block (input: string list) : string list list =
        let rec aux acc curr = function
            | [] -> (
                    match curr with
                        | [] -> List.rev acc
                        | _ -> List.rev (List.rev curr :: acc) )
            | h :: t when String.starts_with h "- " -> (
                    match curr with
                        | [] -> aux acc [h] t
                        | _ -> aux (List.rev curr :: acc) [h] t )
            | h :: t -> aux acc (h :: curr) t
        in
        aux [] [] input
    in
    let block, tt = read_block input in
    let list_items = split_block block in
    (list_items, tt)

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
            let codes = lines |> String.concat "\n" in
            let block = Bcode (lang, codes) in
            aux (block :: acc) tt
        | h :: t when String.starts_with h "> " ->
            let lines, tt = advance_quote_block (h :: t) in
            let quote = aux [] lines in
            let block = Bquote quote in
            aux (block :: acc) tt
        | h :: t when String.starts_with h "- " ->
            let list_items, tt = advance_list_block (h :: t) in
            let lst = List.map parse_list_item list_items in
            let block = Blist lst in
            aux (block :: acc) tt
        | h :: t ->
            let block = Bparagraph (ParseSpan.parse h) in
            aux (block :: acc) t
    and parse_list_item (input: string list) : blockElement list =
        match input with
            | [] -> failwith "parse_list_item"
            | [h] ->
                let line = String.lchop ~n:2 h in
                [Bseq (ParseSpan.parse line)]
            | h :: t ->
                let hh = String.lchop ~n:2 h in
                let hhh = Bseq (ParseSpan.parse hh) in
                let tt = List.map (fun l -> String.lchop ~n:4 l) t in
                let ttt = aux [] tt in
                hhh :: ttt
    in
    aux [] input
