open Batteries

let compile (pattern: string) : Re.re =
    pattern |> Re.Perl.re ~opts:[] |> Re.compile

let test (re: Re.re) (str: string) : bool = Re.execp re str

let exec (re: Re.re) (str: string) : string array option =
    Re.exec_opt re str |> Option.map Re.Group.all

module Infix = struct
    let ( =~ ) = test
end
