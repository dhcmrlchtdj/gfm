open Batteries

let () =
    Cli.run (fun input ->
        input
        |> Parser.parse
        |> Renderer.gfm_render
        |> Printf.sprintf "%s\n"
        |> print_string )
