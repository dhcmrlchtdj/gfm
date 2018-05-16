open Batteries

let () =
    Cli.run (fun input ->
      input |> Parser.parse |> Renderer.html_render |> print_string )
