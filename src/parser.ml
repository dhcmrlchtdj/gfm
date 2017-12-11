open Batteries
module Str = Humane_re.Str

module U = struct
    module Utf8 = struct
        type utf8 = int list

        let utf8map f input =
            let acc = ref [] in
            UTF8.iter
                (fun x ->
                     let xx = f x in
                     acc := xx :: !acc)
                input ;
            !acc |> List.rev


        let trim_bom input =
            if String.left input 3 = "\239\187\191" then String.lchop ~n:3 input
            else input


        let str_to_utf8 (input: string) : utf8 =
            input |> trim_bom |> utf8map UChar.int_of


        let str_of_utf8 (input: utf8) : string =
            let module B = UTF8.Buf in
            let src = List.map UChar.of_int input in
            let buf = B.create (List.length src) in
            List.iter (B.add_char buf) src ;
            B.contents buf


        let replace_crlf (input: utf8) : utf8 =
            let f curr (next, acc) =
                if (curr, next) = (0xd, 0xa) then (curr, acc) else (curr, curr :: acc)
            in
            let _, acc = List.fold_right f input (0, []) in
            acc

    end

    let is_space = function 0x20 -> true | _ -> false

    let is_tab = function 0x9 -> true | _ -> false

    let is_line_break = function 0xa -> true | _ -> false

    let is_whitespace = function 0x9 | 0xc | 0xd | 0x20 -> true | _ -> false

    module Doc = struct
        type line = int list

        type doc = line list

        let rec is_blank_line = function
            | [] -> true
            | 0x20 :: t | 0x9 :: t -> is_blank_line t
            | _ -> false


        let expand_tab (line: line) : line =
            let f (p, acc) curr =
                if curr = 0x9 then
                    let n = 4 - p mod 4 in
                    (p + n, List.make n 0x20 @ acc)
                else (p + 1, curr :: acc)
            in
            let _, acc = List.fold_left f (0, []) line in
            List.rev acc


        let utf8_to_doc (input: Utf8.utf8) : doc =
            input |> Utf8.replace_crlf |> List.nsplit is_line_break
            |> List.map expand_tab


        let utf8_of_doc (input: doc) : Utf8.utf8 =
            input |> List.map (fun l -> l @ [0xa]) |> List.flatten

    end

    let trimming (input: Doc.line) : Doc.line =
        let rec trim_left dq =
            match Deque.front dq with
                | Some (el, q) when is_whitespace el -> trim_left q
                | _ -> dq
        in
        let rec trim_right dq =
            match Deque.rear dq with
                | Some (q, el) when is_whitespace el -> trim_right q
                | _ -> dq
        in
        input |> Deque.of_list |> trim_left |> trim_right |> Deque.to_list


    let simplifying (input: Doc.line) : Doc.line =
        let tr lst =
            let f curr (prev_is_whitespace, acc) =
                let curr_is_whitespace = is_whitespace curr in
                if prev_is_whitespace && curr_is_whitespace then
                    (curr_is_whitespace, acc)
                else (curr_is_whitespace, curr :: acc)
            in
            let _, acc = List.fold_right f lst (false, []) in
            List.rev acc
        in
        input |> trimming |> tr


    let src_to_doc (input: string) : Doc.doc =
        input |> Utf8.str_to_utf8 |> Doc.utf8_to_doc


    let str_of_doc (input: Doc.doc) : string =
        input |> Doc.utf8_of_doc |> Utf8.str_of_utf8

end

module BlockElement = struct
    type lines = U.Doc.line list
    type t =
        | AtxHeader
        | SetexHeader
        | CodeBlock
        | BlockQuote
        | HorizontalRule
        | UnorderedList
        | OrderList
        | Paragraph
        | ReferenceResolutionBlock
        | NullBlock

end

module SpanElement = struct
    type t =
        | Span
        | Link
        | Emphasis
        | CodeSpan
        | Image
        | AutomaticLink
        | HTML

end

let parse (md: string) : string =
    let doc = U.src_to_doc md in
    dump doc |> print_endline ;
    U.str_of_doc doc

