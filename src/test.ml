open Batteries

let cases =
    [ ("`hi`lo`", "<p><code>hi</code>lo`</p>")
    ; ("`foo`", "<p><code>foo</code></p>")
    ; ("`a  b`", "<p><code>a  b</code></p>")
    ; ("*foo`*`", "<p>*foo<code>*</code></p>")
    ; ("[not a `link](/foo`)", "<p>[not a <code>link](/foo</code>)</p>")
    ; ("`<a href=\"`\">`", "<p><code>&lt;a href=&quot;</code>&quot;&gt;`</p>")
    ; ( "`<http://foo.bar.`baz>`"
      , "<p><code>&lt;http://foo.bar.</code>baz&gt;`</p>" )
    ; ("*foo bar*", "<p><em>foo bar</em></p>")
    ; ("foo*bar*", "<p>foo<em>bar</em></p>")
    ; ("_foo*", "<p>_foo*</p>")
    ; ( "<http://foo.bar.baz>"
      , "<p><a href=\"http://foo.bar.baz\">http://foo.bar.baz</a></p>" )
    ; ( "<MAILTO:FOO@BAR.BAZ>"
      , "<p><a href=\"MAILTO:FOO@BAR.BAZ\">MAILTO:FOO@BAR.BAZ</a></p>" )
    ; ( "http://commonmark.org"
      , "<p><a href=\"http://commonmark.org\">http://commonmark.org</a></p>" )
    ; ( "Visit https://encrypted.google.com/search?q=Markup+(business)"
      , "<p>Visit <a href=\"https://encrypted.google.com/search?q=Markup+(business)\">https://encrypted.google.com/search?q=Markup+(business)</a></p>"
    ) ]

let build (input, output) =
    ( "test"
    , `Quick
    , fun () ->
        Alcotest.check Alcotest.string "equal" output
            (input |> Parser.parse |> Renderer.html_render) )

let () = Alcotest.run "markdown" [("test_set", List.map build cases)]
