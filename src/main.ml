open Batteries

let () =
    Cli.run (fun input ->
        input
        |> Parser.parse
        |> Renderer.html_render
        |> Renderer.with_style
        |> Printf.sprintf "%s\n"
        |> print_string )
