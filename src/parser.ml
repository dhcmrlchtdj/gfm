let trim_bom (input : string) : string =
    if CCString.prefix ~pre:"\239\187\191" input
    then CCString.drop 3 input
    else input


let replace_crlf (input : string) : string =
    CCString.replace ~which:`All ~sub:"\r\n" ~by:"\n" input


let expand_tab (input : string) : string =
    if String.contains input '\t'
    then
        let f (p, acc) curr =
            if curr = '\t'
            then
                let n = 4 - (p mod 4) in
                (* (p + n, List.init n (fun _ -> ' ') @ acc) *)
                (p + n, CCList.take n [' '; ' '; ' '; ' '] @ acc)
            else (p + 1, curr :: acc)
        in
        input
        |> CCString.to_list
        |> List.fold_left f (0, [])
        |> snd
        |> CCString.of_list
    else input


let parse (input : string) : Types.md_ast =
    input
    |> trim_bom
    |> replace_crlf
    |> String.split_on_char '\n'
    |> List.map expand_tab
    |> ParseBlock.parse
