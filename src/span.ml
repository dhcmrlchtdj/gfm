open Batteries

type span =
    | Opening of string
    | Closing of string
    | SelfContained of string
    | Text of string

type x = Asterisk | Underscore | Link | RawHTML

let parse_link (x:char Enum.t) =


let parse_root (x: char Enum.t) =
    let fst_ch = Enum.peek x in
    match fst_ch with
        | None -> ""
        | Some '[' | Some ']' -> parse_link x
        | Some '*' -> parse_emphasis x
        | Some '`' -> parse_code_span x
        | Some '!' -> parse_image x
        | Some '<' -> parse_HTML x
        | Some _ -> ""


let parse (input: UTF8.t) : UTF8.t =
    let len = String.length input in
    let remain = String.enum input in
    parse_root remain

