open Batteries

let regexp (str: string) : Re.re = str |> Re_perl.re ~opts:[] |> Re.compile

let test (re: Re.re) (str: string) = Re.execp re str

let exec (re: Re.re) (str: string) : string list option =
    Re.exec_opt re str |> Option.map Re.Group.all |> Option.map Array.to_list


module Infix = struct
    let ( =~ ) = test
end
