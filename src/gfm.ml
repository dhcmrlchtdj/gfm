type peg = Peg.peg
type src = Peg.src
type res = Peg.res
type mf = Peg.mf
let print = Peg.print

exception Err of string

let rec start_list () (s:string) : res =
    if String.length s = 0
    then Ok (s, `EOF)
    else Error ("not match EOF")

let rec block () (s:string) : res =
    if String.length s = 0
    then Ok (s, `EOF)
    else Error ("not match EOF")

let rec doc : mf =
    Peg.sequence [
        start_list ();
        Peg.star (block ())
    ]

let parse (s:string) : peg =
    match doc s with
        | Ok (_, p) -> p
        | Error err -> raise (Err err)

;;

let input = "**bold**" in (parse input) |> print
