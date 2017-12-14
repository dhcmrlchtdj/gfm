open Batteries

let regexp (pattern: string) : Re.re =
    pattern |> Re_perl.re ~opts:[] |> Re.compile


let test (re: Re.re) (str: string) : bool = Re.execp re str

let exec (re: Re.re) (str: string) : string list option =
    Re.exec_opt re str |> Option.map Re.Group.all |> Option.map Array.to_list


module Infix = struct
    let ( =~ ) = test
end
