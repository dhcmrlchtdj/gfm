open Batteries

module R = struct
    let regexp (str: string) : Re.re = str |> Re_perl.re ~opts:[] |> Re.compile

    let matches (re: Re.re) (str: string) : bool =
        Re.exec_opt re str |> Option.is_some


    module Infix = struct
        let ( =~ ) = matches
    end
end

open R.Infix

module U = struct
    let trim_bom (input: UTF8.t) : UTF8.t =
        if String.starts_with input "\239\187\191" then
            String.sub input 3 (String.length input - 3)
        else input


    let replace_crlf (input: UTF8.t) : UTF8.t =
        String.nreplace ~str:input ~sub:"\r\n" ~by:"\n"


    let expand_tab (input: UTF8.t) : UTF8.t =
        if String.exists input "\t" then
            let f (p, acc) curr =
                if curr = '\t' then
                    let n = 4 - p mod 4 in
                    (p + n, List.make n ' ' @ acc)
                else (p + 1, curr :: acc)
            in
            input |> String.explode |> List.fold_left f (0, []) |> snd
            |> String.implode
        else input


    let normalize (input: string) : UTF8.t =
        UTF8.validate input ;
        input |> trim_bom |> replace_crlf


    let split_to_line (input: UTF8.t) : UTF8.t list =
        input |> String.split_on_char '\n' |> List.map expand_tab


    type block =
        | AtxHeader of UTF8.t
        | SetexHeader of UTF8.t * UTF8.t
        | CodeBlock of UTF8.t list
        | BlockQuote of UTF8.t list
        | HorizontalRule
        | UnorderedList of UTF8.t list
        | OrderedList of UTF8.t list
        | Paragraph of UTF8.t list
        | ReferenceResolutionBlock
        | NullBlock

    let split_to_block (input: UTF8.t list) =
        let re_horizontal =
            R.regexp
                "^ *((\\* *\\* *\\* *[\\* ]*)|(\\- *\\- *\\- *[\\- ]*)|(_ *_ *_ *[_ ]*))$"
        in
        let re_order = R.regexp "^ *[0-9]+\\. +[^ ]" in
        let re_unorder = R.regexp "^ *[-+*] +[^ ]" in
        let re_header = R.regexp "^(-+|=+) *$" in
        let begin_with_four_space = String.starts_with "    " in
        let rec advance_code_block acc = function
            | [] -> (acc, [])
            | "" :: h :: t when not (begin_with_four_space h) -> ("" :: acc, h :: t)
            | h :: t when h <> "" && not (begin_with_four_space h) -> (h :: acc, t)
            | h :: t -> advance_code_block (h :: acc) t
        in
        let first_non_space (s: string) : char option =
            let rec aux = function
                | [] -> None
                | ' ' :: t -> aux t
                | h :: _ -> Some h
            in
            aux (String.explode s)
        in
        let non_space (len: int) (s: string) : bool =
            let rec aux l cs =
                if l = 0 then false
                else
                    match cs with
                        | [] -> false
                        | ' ' :: t -> aux (l - 1) t
                        | _ :: _ -> true
            in
            aux len (String.explode s)
        in
        let rec advance_quote_block acc = function
            | [] -> (acc, [])
            | "" :: "" :: t -> ("" :: acc, "" :: t)
            | "" :: h :: t when begin_with_four_space h -> ("" :: acc, h :: t)
            | "" :: h :: t when first_non_space h <> Some '>' -> ("" :: acc, h :: t)
            | a :: b :: c
                when a <> "" && not (begin_with_four_space b) && re_horizontal =~ b ->
                (a :: acc, b :: c)
            | h :: t -> advance_quote_block (h :: acc) t
        in
        let rec advance_unordered_list f starter acc = function
            | [] -> (acc, [])
            | "" :: "" :: a -> ("" :: acc, "" :: a)
            | "" :: a :: b when not (String.starts_with a starter) && f a ->
                ("" :: acc, a :: b)
            | a :: b :: c
                when a <> "" && not (String.starts_with starter b) && f b
                     && not (begin_with_four_space b)
                     && (re_unorder =~ b || re_order =~ b || re_horizontal =~ b) ->
                (a :: acc, b :: c)
            | a :: b -> advance_unordered_list f starter (a :: acc) b
        in
        let rec advance_ordered_list f acc = function
            | [] -> (acc, [])
            | "" :: "" :: a -> ("" :: acc, "" :: a)
            | "" :: a :: b when not (re_order =~ a) && true -> ("" :: acc, a :: b)
            | a :: b :: c
                when a <> "" && not (re_order =~ b) && f b
                     && not (begin_with_four_space b)
                     && (re_unorder =~ b || re_horizontal =~ b) ->
                (a :: acc, b :: c)
            | a :: b -> advance_ordered_list f (a :: acc) b
        in
        let rec aux acc = function
            | [] -> List.rev acc
            | "" :: t ->
                let block = NullBlock in
                aux (block :: acc) t
            | h :: t when not (begin_with_four_space h) && true ->
                (* TODO *)
                let block = ReferenceResolutionBlock in
                aux (block :: acc) t
            | h1 :: h2 :: t when re_header =~ h2 ->
                let block = SetexHeader (h1, h2) in
                aux (block :: acc) t
            | h :: t when String.starts_with "    " h ->
                let b, t = advance_code_block [] (h :: t) in
                let block = CodeBlock (List.rev b) in
                aux (block :: acc) t
            | h :: t when String.starts_with "#" h ->
                let block = AtxHeader h in
                aux (block :: acc) t
            | h :: t when first_non_space h = Some '>' ->
                let b, t = advance_quote_block [] (h :: t) in
                let block = BlockQuote (List.rev b) in
                aux (block :: acc) t
            | h :: t when re_horizontal =~ h ->
                let block = HorizontalRule in
                aux (block :: acc) t
            | h :: t when re_unorder =~ h ->
                (* TODO *)
                let f = non_space 1 in
                let s = "TODO" in
                let b, t = advance_unordered_list f s [] (h :: t) in
                let block = UnorderedList (List.rev b) in
                aux (block :: acc) t
            | h :: t when re_order =~ h ->
                (* TODO *)
                let f = non_space 1 in
                let b, t = advance_ordered_list f [] (h :: t) in
                let block = OrderedList (List.rev b) in
                aux (block :: acc) t
            | _ :: t ->
                (* TODO *)
                let block = Paragraph [] in
                aux (block :: acc) t
        in
        aux [] input

end

let parse (input: string) : string =
    let x = input |> U.normalize |> U.split_to_line |> U.split_to_block in
    dump x


(* let doc = U.src_to_doc md in *)
(* U.str_of_doc doc *)
