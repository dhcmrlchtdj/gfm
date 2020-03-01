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
      "p1\n---\np2";
      (* https://guides.github.com/features/mastering-markdown/ *)
      "# This is an <h1> tag";
      "###### This is an <h6> tag";
      "*This text will be italic*";
      "_This will also be italic_";
      "**This text will be bold**";
      "__This will also be bold__";
      "_You **can** combine them_";
      "- Item 1\n- Item 2\n    - Item 2a\n\t- Item 2b";
      "![GitHub Logo](/images/logo.png)";
      "Format: ![Alt Text](url)";
      "http://github.com - automatic!";
      "[GitHub](http://github.com)";
      "As Kanye West said:\n\n> We're living the future so\n> the present is our past.";
      "I think you should use an\n`<addr>` element here instead.";
      {|```javascript
function fancyAlert(arg) {
  if(arg) {
    $.facebox({div:'#foo'})
  }
}
```|};
    ]
  in
  let f case = case |> Parser.parse |> Renderer.html_render |> print_endline in
  List.iter f cases
