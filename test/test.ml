open Mine

let () =
  let cases =
    [
      "`hi`lo`";
      "`foo`";
      "`a  b`";
      "*foo`*`";
      "[not a `link](/foo`)";
      "`<a href=\"`\">`";
      "`<http://foo.bar.`baz>`";
      "*foo bar*";
      "foo*bar*";
      "_foo*";
      "~~foo~~";
      "**foo**";
      "*foo*";
      "<http://foo.bar.baz>";
      "<MAILTO:FOO@BAR.BAZ>";
      "http://commonmark.org";
      "Visit https://encrypted.google.com/search?q=Markup+(business)";
      "p1\n\np2";
    ]
  in
  let f case = case |> Parser.parse |> Renderer.html_render |> print_endline in
  List.iter f cases
