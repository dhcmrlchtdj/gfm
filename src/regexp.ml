open Batteries

let compile (pattern: string) : Re.re =
    pattern |> Re.Perl.re ~opts:[] |> Re.compile

let test (re: Re.re) (str: string) : bool = Re.execp re str

let exec (re: Re.re) (str: string) : string array option =
    Re.exec_opt re str |> Option.map Re.Group.all

type split_text = SplitText of string | SplitDelim of string

let token_to_str : Re.split_token list -> split_text list =
    List.map (function
        | `Text s -> SplitText s
        | `Delim d ->
            try
                let s = Re.Group.get d 0 in
                SplitDelim s
            with Not_found -> SplitText "" )

let split_full (re: Re.re) (str: string) : split_text list =
    Re.split_full re str |> token_to_str

module Infix = struct
    let ( =~ ) = test
end
