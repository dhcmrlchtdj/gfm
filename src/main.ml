open Batteries

let () =
    Cli.run (fun input ->
        let ast = Parser.parse input in
        let html = Renderer.html_render ast in
        print_string html )
