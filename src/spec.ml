(* id * section * markdown * html *)
let spec : (int * string * string * string) list =
    let module U = Yojson.Basic.Util in
    Yojson.Basic.from_file "./src/spec.json"
    |> U.to_list
    |> List.map (fun json ->
        let member field = json |> U.member field |> U.to_string in
        let id = json |> U.member "example" |> U.to_int
        and section = member "section"
        and markdown = member "markdown"
        and html = member "html"
        in (id, section, markdown, html)
    )
