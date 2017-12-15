open Batteries

let render (src: string) : unit =
    let s = src |> String.rchop ~n:1 in
    let html = Parser.parse s in
    print_string html ; ()


let () =
    let usage () = Printf.printf "Usage: %s [file | -]\n" exe in
    let argv = Sys.argv |> Array.to_list |> List.tl in
    let aux = function
        | ["-h"] | ["--help"] -> usage ()
        | ["-"] -> IO.read_all stdin |> render
        | [file] -> File.with_file_in file IO.read_all |> render
        | _ -> usage ()
    in
    aux argv

