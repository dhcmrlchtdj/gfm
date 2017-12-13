open Batteries

module R = struct
    let regexp (str: string) = str |> Re_perl.re ~opts:[] |> Re.compile

    let matches re str = Re.exec_opt re str |> Option.is_some

    module Infix = struct
        let ( =~ ) s re = matches re s
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
        | CodeBlock
        | BlockQuote
        | HorizontalRule
        | UnorderedList
        | OrderList
        | Paragraph
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
        let rec aux acc = function
            | [] -> List.rev acc
            | "" :: t ->
                let block = NullBlock in
                aux (block :: acc) t
            | h :: t when not (String.starts_with "    " h) && true -> (*TODO*)
                acc
            | h1 :: h2 :: t when h2 =~ re_header ->
                let block = SetexHeader (h1, h2) in
                aux (block :: acc) t
            | h :: t when String.starts_with "    " h ->
                (* TODO *)
                let block = CodeBlock in
                aux (block :: acc) t
            | h :: t when String.starts_with "#" h ->
                let block = AtxHeader h in
                aux (block :: acc) t
            | h :: t
                when String.starts_with ">" h || String.starts_with " >" h
                     || String.starts_with "  >" h || String.starts_with "   >" h ->
                (* TODO *)
                let block = BlockQuote in
                aux (block :: acc) t
            | h :: t when h =~ re_horizontal ->
                let block = HorizontalRule in
                aux (block :: acc) t
            | h :: t when h =~ re_unorder ->
                (* TODO *)
                let block = UnorderedList in
                aux (block :: acc) t
            | h :: t when h =~ re_order ->
                (* TODO *)
                let block = OrderList in
                aux (block :: acc) t
            | h :: t ->
                let block = Paragraph in
                aux (block :: acc) t
        in
        aux [] input

end

let parse (input: string) : string =
    let x = input |> U.normalize |> U.split_to_line |> U.split_to_block in
    dump x


(* let doc = U.src_to_doc md in *)
(* U.str_of_doc doc *)
