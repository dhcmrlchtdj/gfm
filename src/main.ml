open Batteries

let render (src: string) : unit =
    let s = src |> String.trim in
    let md = Parser.parse s in
    print_endline md ; ()


let () =
    let prog = Sys.argv.(0) in
    let usage () = Printf.printf "Usage: %s [file | -]\n" prog in
    let argv = Sys.argv |> Array.to_list |> List.tl in
    let aux = function
        | ["-h"] | ["--help"] -> usage ()
        | ["-"] -> IO.read_all IO.stdin |> render
        | [file] -> File.with_file_in file IO.read_all |> render
        | [] | _ -> usage ()
    in
    aux argv

