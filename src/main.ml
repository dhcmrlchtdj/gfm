let () =
    let parse = fun (id,sec,md,html) ->
        let r = Parser.parse md in
        (id,sec,md,html,r,r=html)
    in
    let print = fun (id,sec,md,html,r,_) ->
        Printf.printf "%40s \t %d \t %B \t %S %S \n" sec id (r = html) html r
    in
    Spec.spec
    |> List.map parse
    |> List.filter (fun (_,_,_,_,_,pass) -> not pass)
    |> List.iter print
