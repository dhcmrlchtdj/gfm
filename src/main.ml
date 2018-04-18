open Batteries

let render (input: string) : unit =
    let ast = Parser.parse input in
    let html = Renderer.html_render ast in
    print_string html


let () = Cli.run render
