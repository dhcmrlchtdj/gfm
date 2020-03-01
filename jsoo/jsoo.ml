open! Containers
open Js_of_ocaml
open Mine

let of_jsString = Js.to_string

let to_jsString = Js.string

let () =
  let obj =
    object%js
      method render s =
        s |> of_jsString |> Parser.parse |> Renderer.html_render |> to_jsString

      method renderWithStyle s =
        s
        |> of_jsString
        |> Parser.parse
        |> Renderer.html_render
        |> Renderer.with_style
        |> to_jsString
    end
  in
  Js.export_all obj
