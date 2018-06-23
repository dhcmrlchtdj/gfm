open Batteries

let run callback =
    let usage () = Printf.printf "Usage: %s [file | -]\n" exe in
    let argv = Sys.argv |> Array.to_list |> List.tl in
    let aux = function
        | ["-h"] | ["--help"] -> usage ()
        | ["-"] -> IO.read_all stdin |> callback
        | [file] -> File.with_file_in file IO.read_all |> callback
        | _ -> usage ()
    in
    aux argv

let main input =
    input
    |> Parser.parse
    |> Renderer.html_render
    |> Renderer.with_style
    |> print_endline

let () = run main
