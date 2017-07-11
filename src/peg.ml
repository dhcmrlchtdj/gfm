type peg =
    | EOF
    | Epsilon
    | Sequence of peg list
    | Choice of peg
    | Question of peg
    | Plus of peg list
    | Star of peg list
    | Dot of string
    | Literal of string
    | Range of string

type src = (string * peg) (*remain, output*)
type res = (src, string) result
type mf = string -> res

let rec print : peg -> unit = function
    | EOF -> print_endline "EOF"
    | Epsilon -> print_endline "()"
    | _ -> print_endline "un"

let rec eof () (s:string) : res =
    if String.length s = 0
    then Ok (s, EOF)
    else Error ("not match EOF")

let rec dot () (s:string) : res =
    let len = String.length s in
    if len <= 0
    then Error "not match DOT"
    else (
        let ss = Char.escaped s.[0] in
        let r = String.sub s 1 (len - 1) in
        Ok (r, Dot ss)
    )

let rec epsilon () (s:string) : res =
    Ok (s, Epsilon)

let rec sequence (ml:mf list) (s:string) : res =
    let rec aux ml ss sl =
        (match sl with
            | Sequence sl ->
                (match ml with
                    | [] -> Ok (ss, Sequence sl)
                    | hd :: tl ->
                        (match hd ss with
                            | Ok (rr, oo) -> aux tl rr (Sequence (sl @ [oo]))
                            | Error _ as err -> err))
            | _ -> Error "unexpected")
    in
    aux ml s (Sequence [])

let rec choice (ml:mf list) (s:string) : res =
    (match ml with
        | [] -> Error "expect Choice, not match"
        | hd :: tl ->
            (match hd s with
                | Ok (r, o) -> Ok (r, Choice o)
                | Error _ -> choice tl s))

let rec question (m:mf) (s:string) : res =
    (match m s with
        | Ok (r, o) -> Ok (r, Question o)
        | Error _ -> Ok (s, Question Epsilon))

let rec star (m:mf) (s:string) : res =
    let rec aux mm ss sl =
        (match sl with
            | Star sl ->
                (match mm ss with
                    | Ok (rr, oo) -> aux mm rr (Star (sl @ [oo]))
                    | Error _ as err -> err)
            | _ -> Error "unexpected")
    in
    aux m s (Star [])

let rec plus (m:mf) (s:string) : res =
    let ml = [m; (star m)] in
    (match sequence ml s with
        | Ok (r, Sequence (o :: (Star oo) :: [])) -> Ok (r, Plus (o :: oo))
        | Error _ as err -> err
        | _ -> Error "unexpected")

let rec literal (p:string) (s:string) : res =
    let plen = String.length p in
    let slen = String.length s in
    if plen > slen
    then Error "literal not match"
    else (
        let ss = String.sub s 0 plen in
        if p != ss
        then Error "literal not match"
        else (
            let rr = String.sub s plen (slen - plen) in
            Ok (rr, Literal ss)
        )
    )

let rec range (c1:char) (c2:char) (s:string) : res =
    let c = s.[0] in
    if not (c >= c1 && c <= c2)
    then Error "not match range"
    else (
        let ss = Char.escaped c in
        let len = String.length s in
        let r = String.sub s 1 (len - 1) in
        Ok (r, Range ss)
    )

let rec next_not (m:mf) (s:string) : bool =
    (match m s with
        | Ok _ -> true
        | _ -> false)

let rec next_is (m:mf) (s:string) : bool =
    not (next_is m s)
