let is_whitespace = function
    | ' ' | '\012' | '\n' | '\r' | '\t' -> true
    | _ -> false

let is_blank_line (line:string) : bool = String.trim line = ""

let expand_tab (line:string) : string =
    let len = String.length line in
    let buf = Buffer.create len in
    let rec add_space = function
        | 0 -> ()
        | x -> Buffer.add_char buf ' '; add_space (x - 1)
    in
    let rec aux bpos lpos =
        if lpos < len
        then
            let bpos_new = (match line.[lpos] with
                | '\t' ->
                    let n = 4 - (bpos mod 4) in
                    add_space n;
                    bpos + n
                | c -> Buffer.add_char buf c; bpos + 1
            ) in
            aux bpos_new (lpos + 1)
    in
    aux 0 0;
    Buffer.contents buf

let simplifying s =
    s
    |> Str.split (Str.regexp "[ \n\r\t\012]+")
    |> String.concat " "

let str_to_line (s:string) : string list = s |> Str.split (Str.regexp "\n")
let str_to_char (s:string) : char list =
    let rec exp i l =
        if i < 0
        then l
        else exp (i - 1) (s.[i] :: l) in
    exp (String.length s - 1) []









let rec parse_span (line:string) =
    let chars = str_to_char line in
    let consumed = 0 in
    (match chars with
        | [] -> []
        | '['::_ | ']'::_ -> parse_link chars
        | '!'::'['::_ -> parse_image chars
        | '`'::_ -> parse_code_span chars
        | '*'::'*'::_ -> parse_emphasis chars
        | '_'::'_'::_ -> parse_emphasis chars
        | '~'::'~'::_ -> parse_strikethrough chars
        | '<'::_ when consumed <> 0 -> parse_HTML chars
        | _ when consumed <> 0 -> parse_autolink chars
        | _ -> []
    )











(**
 * unordered list
 * ordered list
 * horizontal
*)
let is_unordered_list (line:string) : bool = true
let is_ordered_list (line:string) : bool = true
let is_horizontal (line:string) : bool = true


let parse (md:string) : string = ""
