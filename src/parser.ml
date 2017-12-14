open Batteries

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
        input |> String.to_list |> List.fold_left f (0, []) |> snd
        |> String.of_list
    else input


let normalize (input: string) : UTF8.t =
    UTF8.validate input ;
    input |> trim_bom |> replace_crlf


let split_to_line (input: UTF8.t) : UTF8.t list =
    input |> String.split_on_char '\n' |> List.map expand_tab


let parse (input: string) : string =
    let x = input |> normalize |> split_to_line |> Block_parser.split_to_block in
    dump x


(* let doc = U.src_to_doc md in *)
(* U.str_of_doc doc *)
