open Batteries
open Types

type token =
    | TstrongA
    | TstrongU
    | TemphasisA
    | TemphasisU
    | TsimpleLink of string
    | TimgOpen
    | TlinkOpen
    | TlinkClose of string
    | Tcode of string
    | Tstring of string

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
    let read_simple_link = read_util '>' in
    let concat_string (tokens: token list) : token list =
        let rec read_string acc = function
            | Tstring s :: t -> read_string (s :: acc) t
            | _ as t ->
                let s = acc |> List.rev |> String.concat "" in
                (Tstring s, t)
        in
        let rec aux acc = function
            | Tstring s :: t ->
                let ss, tt = read_string [s] t in
                aux (ss :: acc) tt
            | h :: t -> aux (h :: acc) t
            | [] -> List.rev acc
        in
        aux [] tokens
    in
    let rec aux acc = function
        | [] -> List.rev acc
        | '*' :: '*' :: t -> aux (TstrongA :: acc) t
        | '_' :: '_' :: t -> aux (TstrongU :: acc) t
        | '*' :: t -> aux (TemphasisA :: acc) t
        | '_' :: t -> aux (TemphasisU :: acc) t
        | '`' :: t -> (
                match read_code t with
                    | Some (code, tt) -> aux (Tcode code :: acc) tt
                    | None -> aux (Tstring "`" :: acc) t )
        | '!' :: '[' :: t -> aux (TimgOpen :: acc) t
        | '[' :: t -> aux (TlinkOpen :: acc) t
        | ']' :: '(' :: t -> (
                match read_link t with
                    | Some (link, tt) -> aux (TlinkClose link :: acc) tt
                    | None -> aux (Tstring "(" :: Tstring "]" :: acc) t )
        | '<' :: t -> (
                match read_simple_link t with
                    | Some (link, tt) -> aux (TsimpleLink link :: acc) tt
                    | None -> aux (Tstring "<" :: acc) t )
        | h :: t -> aux (Tstring (String.of_char h) :: acc) t
    in
    let r = aux [] chars in
    concat_string r

let tokens_to_spans (tokens: token list) : spanElement list =
    let sprintf = Printf.sprintf in
    let to_span (t: token) =
        let convert = function
            | TstrongA -> "**"
            | TstrongU -> "__"
            | TemphasisA -> "*"
            | TemphasisU -> "_"
            | TsimpleLink l -> sprintf "<%s>" l
            | TimgOpen -> "!["
            | TlinkOpen -> "["
            | TlinkClose l -> sprintf "](%s" l
            | Tcode s -> sprintf "`%s`" s
            | Tstring s -> s
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
        | Tstring s :: t -> aux (S (Stext s) :: acc) t
        | Tcode code :: t -> aux (S (Scode code) :: acc) t
        | TsimpleLink l :: t -> aux (S (Slink ([Stext l], l)) :: acc) t
        | TimgOpen :: t -> aux (T TimgOpen :: acc) t
        | TlinkOpen :: t -> aux (T TlinkOpen :: acc) t
        | (TlinkClose l as h) :: t -> (
                match find_open l acc with
                    | Some acc2 -> aux acc2 t
                    | None -> aux (S (to_span h) :: acc) t )
        | (TstrongA as h) :: t
        |(TstrongU as h) :: t
        |(TemphasisA as h) :: t
        |(TemphasisU as h) :: t -> (
                match find_match h acc with
                    | Some acc2 -> aux acc2 t
                    | None -> aux (T h :: acc) t )
        | [] -> to_spans (List.rev acc)
    in
    aux [] tokens

let parse (text: string) : spanElement list =
    text |> String.to_list |> chars_to_tokens |> tokens_to_spans
