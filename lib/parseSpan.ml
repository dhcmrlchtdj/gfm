open Types
module String = CCString

let re_url = Regexp.compile "\\bhttps?://\\S+"

type token =
    | TstrongA
    | TstrongU
    | TemphasisA
    | TemphasisU
    | Tdelete
    | TautoLink of string
    | TsimpleLink of string
    | TimgOpen
    | TlinkOpen
    | TlinkClose of string
    | Tcode of string
    | Tstring of string
[@@deriving eq]

type t_n_s =
    | T of token
    | S of spanElement

let concat_string (tokens : token list) : token list =
    let rec read_string acc = function
        | Tstring s :: t -> read_string (s :: acc) t
        | t ->
            let s = acc |> List.rev |> String.concat "" in
            (Tstring s, t)
    in
    let rec aux acc = function
        | Tstring s :: t ->
            let (ss, tt) = read_string [ s ] t in
            aux (ss :: acc) tt
        | h :: t -> aux (h :: acc) t
        | [] -> List.rev acc
    in
    aux [] tokens


let split_link (tokens : token list) : token list =
    let try_autolink s =
        let f = function
            | Regexp.SplitText s -> Tstring s
            | Regexp.SplitDelim s -> TautoLink s
        in
        let matched = Regexp.split_full re_url s in
        List.map f matched
    in
    let rec aux acc = function
        | Tstring s :: t ->
            let ss = try_autolink s in
            let f prev curr = curr :: prev in
            let acc2 = List.fold_left f acc ss in
            aux acc2 t
        | h :: t -> aux (h :: acc) t
        | [] -> List.rev acc
    in
    aux [] tokens


let chars_to_tokens (chars : char list) : token list =
    let read_util (ch : char) (chars : char list) : (string * char list) option
      =
        let rec aux acc = function
            | '\\' :: h :: t -> aux (h :: '\\' :: acc) t
            | h :: t when Char.equal h ch ->
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
    let rec aux acc = function
        | [] -> List.rev acc
        | '~' :: '~' :: t -> aux (Tdelete :: acc) t
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
                | None -> aux (Tstring "](" :: acc) t )
        | '<' :: t -> (
            match read_simple_link t with
                | Some (link, tt) -> aux (TsimpleLink link :: acc) tt
                | None -> aux (Tstring "<" :: acc) t )
        | h :: t -> aux (Tstring (String.of_char h) :: acc) t
    in
    let r = aux [] chars in
    r |> concat_string |> split_link


let tokens_to_spans (tokens : token list) : spanElement list =
    let sprintf = Printf.sprintf in
    let convert = function
        | Tdelete -> "~~"
        | TstrongA -> "**"
        | TstrongU -> "__"
        | TemphasisA -> "*"
        | TemphasisU -> "_"
        | TsimpleLink l -> sprintf "<%s>" l
        | TautoLink l -> l
        | TimgOpen -> "!["
        | TlinkOpen -> "["
        | TlinkClose l -> sprintf "](%s" l
        | Tcode s -> sprintf "`%s`" s
        | Tstring s -> s
    in
    let to_span (t : token) = Stext (convert t) in
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
            | Sdelete s :: t ->
                let ss = sprintf "~~%s~~" (to_text s) in
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
    let find_open (link : string) acc =
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
    let find_match (tok : token) acc =
        let rec aux tmp_acc acc =
            match acc with
                | (T (TstrongA as x) :: t | T (TstrongU as x) :: t)
                  when equal_token x tok ->
                    let s = S (Sstrong (to_spans tmp_acc)) in
                    Some (s :: t)
                | (T (TemphasisA as x) :: t | T (TemphasisU as x) :: t)
                  when equal_token x tok ->
                    let s = S (Semphasis (to_spans tmp_acc)) in
                    Some (s :: t)
                | T Tdelete :: t when equal_token tok Tdelete ->
                    let s = S (Sdelete (to_spans tmp_acc)) in
                    Some (s :: t)
                | T x :: _ when equal_token x tok -> None
                | h :: t -> aux (h :: tmp_acc) t
                | [] -> None
        in
        aux [] acc
    in
    let rec aux acc = function
        | Tstring s :: t -> aux (S (Stext s) :: acc) t
        | Tcode code :: t -> aux (S (Scode code) :: acc) t
        | TautoLink l :: t -> aux (S (Slink ([ Stext l ], l)) :: acc) t
        | TsimpleLink l :: t -> aux (S (Slink ([ Stext l ], l)) :: acc) t
        | TimgOpen :: t -> aux (T TimgOpen :: acc) t
        | TlinkOpen :: t -> aux (T TlinkOpen :: acc) t
        | (TlinkClose l as h) :: t -> (
            match find_open l acc with
                | Some acc2 -> aux acc2 t
                | None -> aux (S (to_span h) :: acc) t )
        | (Tdelete as h) :: t
        | (TstrongA as h) :: t
        | (TstrongU as h) :: t
        | (TemphasisA as h) :: t
        | (TemphasisU as h) :: t -> (
            match find_match h acc with
                | Some acc2 -> aux acc2 t
                | None -> aux (T h :: acc) t )
        | [] -> to_spans (List.rev acc)
    in
    aux [] tokens


let parse (text : string) : spanElement list =
    text |> String.to_list |> chars_to_tokens |> tokens_to_spans
