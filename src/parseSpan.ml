open Batteries
open Types

type token =
    | TstrongA
    | TstrongU
    | TemphasisA
    | TemphasisU
    | TimgOpen
    | TlinkOpen
    | TlinkClose of string
    | Tcode of string
    | Tchar of char

type t_n_s = T of token | S of spanElement

let chars_to_tokens (chars: char list) : token list =
    let read_util (ch: char) (chars: char list) : (string * char list) option =
        let rec aux acc = function
            | '\\' :: h :: t when h = ch -> aux (ch :: '\\' :: acc) t
            | h :: t when h = ch ->
                let s = acc |> List.rev |> String.of_list in
                Some (s, t)
            | h :: t -> aux (h :: acc) t
            | [] -> None
        in
        aux [] chars
    in
    let read_code = read_util '`' in
    let read_link = read_util ')' in
    let rec aux acc = function
        | [] -> List.rev acc
        | '*' :: '*' :: t -> aux (TstrongA :: acc) t
        | '_' :: '_' :: t -> aux (TstrongU :: acc) t
        | '*' :: t -> aux (TemphasisA :: acc) t
        | '_' :: t -> aux (TemphasisU :: acc) t
        | '`' :: t -> (
                match read_code t with
                    | Some (code, tt) -> aux (Tcode code :: acc) tt
                    | None -> aux (Tchar '`' :: acc) t )
        | '!' :: '[' :: t -> aux (TimgOpen :: acc) t
        | '[' :: t -> aux (TlinkOpen :: acc) t
        | ']' :: '(' :: t -> (
                match read_link t with
                    | Some (link, tt) -> aux (TlinkClose link :: acc) tt
                    | None -> aux (Tchar '(' :: Tchar ']' :: acc) t )
        | h :: t -> aux (Tchar h :: acc) t
    in
    aux [] chars

let tokens_to_spans (tokens: token list) : spanElement list =
    let sprintf = Printf.sprintf in
    let to_span (t: token) =
        let convert = function
            | TstrongA -> "**"
            | TstrongU -> "__"
            | TemphasisA -> "*"
            | TemphasisU -> "_"
            | TimgOpen -> "!["
            | TlinkOpen -> "["
            | TlinkClose l -> sprintf "](%s" l
            | Tcode s -> sprintf "`%s`" s
            | Tchar c -> String.of_char c
        in
        Stext (convert t)
    in
    let to_spans tnss =
        let rec aux acc = function
            | S s :: t -> aux (s :: acc) t
            | T s :: t -> aux (to_span s :: acc) t
            | [] -> List.rev acc
        in
        aux [] tnss
    in
    let rec to_text spans =
        let rec aux acc = function
            | Stext s :: t -> aux (s :: acc) t
            | Scode s :: t -> aux (s :: acc) t
            | Sstrong s :: t ->
                let ss = sprintf "**%s**" (to_text s) in
                aux (ss :: acc) t
            | Semphasis s :: t ->
                let ss = sprintf "*%s*" (to_text s) in
                aux (ss :: acc) t
            | Slink (text, url) :: t ->
                let ss = sprintf "[%s](%s)" (to_text text) url in
                aux (ss :: acc) t
            | Simage (alt, url) :: t ->
                let ss = sprintf "![%s](%s)" alt url in
                aux (ss :: acc) t
            | [] -> List.rev acc
        in
        let ss = aux [] spans in
        String.concat "" ss
    in
    let rec read_chars acc = function
        | Tchar c :: t -> read_chars (c :: acc) t
        | _ as t ->
            let s = acc |> List.rev |> String.of_list in
            (s, t)
    in
    let find_open (link: string) acc =
        let rec aux tmp_acc acc =
            match acc with
                | T TimgOpen :: t ->
                    let s = to_text (to_spans tmp_acc) in
                    let ss = S (Simage (s, link)) in
                    Some (ss :: t)
                | T TlinkOpen :: t ->
                    let s = to_spans tmp_acc in
                    let ss = S (Slink (s, link)) in
                    Some (ss :: t)
                | h :: t -> aux (h :: tmp_acc) t
                | [] -> None
        in
        aux [] acc
    in
    let find_match (tok: token) acc =
        let rec aux tmp_acc acc =
            match acc with
                | T x :: t when x = tok ->
                    if x = TstrongA || x = TstrongU
                    then
                        let s = S (Sstrong (to_spans tmp_acc)) in
                        Some (s :: t)
                    else if x = TemphasisA || x = TemphasisU
                    then
                        let s = S (Semphasis (to_spans tmp_acc)) in
                        Some (s :: t)
                    else None
                | h :: t -> aux (h :: tmp_acc) t
                | [] -> None
        in
        aux [] acc
    in
    let rec aux acc = function
        | Tchar c :: t ->
            let s, tt = read_chars [c] t in
            aux (S (Stext s) :: acc) tt
        | Tcode code :: t -> aux (S (Scode code) :: acc) t
        | TimgOpen :: t -> aux (T TimgOpen :: acc) t
        | TlinkOpen :: t -> aux (T TlinkOpen :: acc) t
        | (TlinkClose l as h) :: t -> (
                match find_open l acc with
                    | Some acc2 -> aux acc2 t
                    | None -> aux (S (to_span h) :: acc) t )
        | h :: t -> (
                match find_match h acc with
                    | Some acc2 -> aux acc2 t
                    | None -> aux (T h :: acc) t )
        | [] -> to_spans (List.rev acc)
    in
    aux [] tokens

let parse (text: string) : spanElement list =
    text |> String.to_list |> chars_to_tokens |> tokens_to_spans
