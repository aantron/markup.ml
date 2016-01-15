(* This file is part of Markup.ml, released under the BSD 2-clause license. See
   doc/LICENSE for details, or visit https://github.com/aantron/markup.ml. *)

open OUnit2
open Test_support
open Common

let doctype =
  `Doctype
    {doctype_name      = Some "html";
     public_identifier = None;
     system_identifier = None;
     raw_text          = None;
     force_quirks      = false}

let start_element name =
  `Start_element ((html_ns, name), [])

let expect ?prefix ?(context = Some `Document) text signals =
  let report, iterate, ended =
    expect_signals ?prefix signal_to_string text signals in

  text
  |> Stream_io.string
  |> Encoding.utf_8
  |> Input.preprocess is_valid_html_char Error.ignore_errors
  |> Html_tokenizer.tokenize Error.ignore_errors
  |> Html_parser.parse context report
  |> iter iterate;

  ended ()

let tests = [
  ("html.parser.basic" >:: fun _ ->
    expect "<!DOCTYPE html><html><head></head><body></body></html>"
      [ 1,  1, S  doctype;
        1, 16, S (start_element "html");
        1, 22, S (start_element "head");
        1, 28, S  `End_element;
        1, 35, S (start_element "body");
        1, 41, S  `End_element;
        1, 48, S  `End_element];

    expect ~prefix:true " <!--foo--> <!DOCTYPE html>"
      [ 1,  2, S (`Comment "foo");
        1, 13, S  doctype];

    expect ~prefix:true "<!DOCTYPE html> <!--foo--> <html></html>"
      [ 1,  1, S  doctype;
        1, 17, S (`Comment "foo");
        1, 28, S (start_element "html")];

    expect ~prefix:true "<html> <!--foo--> <head></head></html>"
      [ 1,  1, S (start_element "html");
        1,  8, S (`Comment "foo");
        1, 19, S (start_element "head")]);

  ("html.parser.implicit-top-level" >:: fun _ ->
    expect "<!DOCTYPE html>"
      [ 1,  1, S  doctype;
        1, 16, S (start_element "html");
        1, 16, S (start_element "head");
        1, 16, S  `End_element;
        1, 16, S (start_element "body");
        1, 16, S  `End_element;
        1, 16, S  `End_element];

    expect "<!DOCTYPE html><html></html>"
      [ 1,  1, S  doctype;
        1, 16, S (start_element "html");
        1, 22, S (start_element "head");
        1, 22, S  `End_element;
        1, 22, S (start_element "body");
        1, 22, S  `End_element;
        1, 22, S  `End_element];

    expect "<!DOCTYPE html><head></head>"
      [ 1,  1, S  doctype;
        1, 16, S (start_element "html");
        1, 16, S (start_element "head");
        1, 22, S  `End_element;
        1, 29, S (start_element "body");
        1, 29, S  `End_element;
        1, 29, S  `End_element];

    expect "<!DOCTYPE html><body></body>"
      [ 1,  1, S  doctype;
        1, 16, S (start_element "html");
        1, 16, S (start_element "head");
        1, 16, S  `End_element;
        1, 16, S (start_element "body");
        1, 22, S  `End_element;
        1, 29, S  `End_element];

    expect "<!DOCTYPE html><p></p>"
      [ 1,  1, S  doctype;
        1, 16, S (start_element "html");
        1, 16, S (start_element "head");
        1, 16, S  `End_element;
        1, 16, S (start_element "body");
        1, 16, S (start_element "p");
        1, 19, S  `End_element;
        1, 23, S  `End_element;
        1, 23, S  `End_element];

    expect "<!DOCTYPE html><title></title>"
      [ 1,  1, S  doctype;
        1, 16, S (start_element "html");
        1, 16, S (start_element "head");
        1, 16, S (start_element "title");
        1, 23, S  `End_element;
        1, 31, S  `End_element;
        1, 31, S (start_element "body");
        1, 31, S  `End_element;
        1, 31, S  `End_element]);

  ("html.parser.no-doctype" >:: fun _ ->
    expect ~prefix:true "<title>foo</title>"
      [ 1,  1, S (start_element "html");
        1,  1, S (start_element "head");
        1,  1, S (start_element "title");
        1,  8, S (`Text ["foo"])]);

  ("html.parser.double-doctype" >:: fun _ ->
    expect ~prefix:true "<!DOCTYPE html><!DOCTYPE html><html></html>"
      [ 1,  1, S  doctype;
        1, 16, E (`Bad_document "doctype should be first");
        1, 31, S (start_element "html")]);

  ("html.parser.end-before-html" >:: fun _ ->
    expect ~prefix:true "</p><html></html>"
      [ 1,  1, E (`Unmatched_end_tag "p");
        1,  5, S (start_element "html")]);

  ("html.parser.junk-before-head" >:: fun _ ->
    expect ~prefix:true "<html><!DOCTYPE html><html></p><head></head></html>"
      [ 1,  1, S (start_element "html");
        1,  7, E (`Bad_document "doctype should be first");
        1, 22, E (`Misnested_tag ("html", "html"));
        1, 28, E (`Unmatched_end_tag "p");
        1, 32, S (start_element "head")]);

  ("html.parser.head" >:: fun _ ->
    expect ~prefix:true "<head> <!--foo--><link><link/><meta><meta/></head>"
      [ 1,  1, S (start_element "html");
        1,  1, S (start_element "head");
        1,  7, S (`Text [" "]);
        1,  8, S (`Comment "foo");
        1, 18, S (start_element "link");
        1, 18, S  `End_element;
        1, 24, S (start_element "link");
        1, 24, S  `End_element;
        1, 31, S (start_element "meta");
        1, 31, S  `End_element;
        1, 37, S (start_element "meta");
        1, 37, S  `End_element;
        1, 44, S  `End_element;
        1, 51, S (start_element "body")]);

  ("html.parser.style" >:: fun _ ->
    expect ~prefix:true "<head><style>foo</head>&lt;</style></head>"
      [ 1,  1, S (start_element "html");
        1,  1, S (start_element "head");
        1,  7, S (start_element "style");
        1, 14, S (`Text ["foo</head>&lt;"]);
        1, 28, S  `End_element;
        1, 36, S  `End_element;
        1, 43, S (start_element "body")]);

  ("html.parser.title" >:: fun _ ->
    expect ~prefix:true "<head><title>foo</head>&lt;</title></head>"
      [ 1,  1, S (start_element "html");
        1,  1, S (start_element "head");
        1,  7, S (start_element "title");
        1, 14, S (`Text ["foo</head><"]);
        1, 28, S  `End_element;
        1, 36, S  `End_element;
        1, 43, S (start_element "body")]);

  ("html.parser.script" >:: fun _ ->
    expect ~prefix:true "<head><script><!--foo</head>&lt;bar</script></head>"
      [ 1,  1, S (start_element "html");
        1,  1, S (start_element "head");
        1,  7, S (start_element "script");
        1, 15, S (`Text ["<!--foo</head>&lt;bar"]);
        1, 36, S  `End_element;
        1, 45, S  `End_element;
        1, 52, S (start_element "body")]);

  ("html.parser.junk-in-head" >:: fun _ ->
    expect ~prefix:true "<head><!DOCTYPE html><html><head></p></head>"
      [ 1,  1, S (start_element "html");
        1,  1, S (start_element "head");
        1,  7, E (`Bad_document "doctype should be first");
        1, 22, E (`Misnested_tag ("html", "head"));
        1, 28, E (`Misnested_tag ("head", "head"));
        1, 34, E (`Unmatched_end_tag "p");
        1, 38, S  `End_element;
        1, 45, S (start_element "body")]);

  ("html.parser.junk-after-head" >:: fun _ ->
    expect ~prefix:true
      "<head></head> <!--foo--><!DOCTYPE html><html><meta><head></p><body>"
      [ 1,  1, S (start_element "html");
        1,  1, S (start_element "head");
        1,  7, S  `End_element;
        1, 14, S (`Text [" "]);
        1, 15, S (`Comment "foo");
        1, 25, E (`Bad_document "doctype should be first");
        1, 40, E (`Misnested_tag ("html", "html"));
        1, 46, E (`Misnested_tag ("meta", "html"));
        1, 46, S (start_element "meta");
        1, 46, S  `End_element;
        1, 52, E (`Bad_document "duplicate head element");
        1, 58, E (`Unmatched_end_tag "p");
        1, 62, S (start_element "body")]);

  ("html.parser.body-content" >:: fun _ ->
    expect "<body><!--foo--> bar</body>"
      [ 1,  1, S (start_element "html");
        1,  1, S (start_element "head");
        1,  1, S  `End_element;
        1,  1, S (start_element "body");
        1,  7, S (`Comment "foo");
        1, 17, S (`Text [" bar"]);
        1, 21, S  `End_element;
        1, 28, S  `End_element]);

  ("html.parser.paragraphs" >:: fun _ ->
    expect "<p>foo</p>"
      [ 1,  1, S (start_element "html");
        1,  1, S (start_element "head");
        1,  1, S  `End_element;
        1,  1, S (start_element "body");
        1,  1, S (start_element "p");
        1,  4, S (`Text ["foo"]);
        1,  7, S  `End_element;
        1, 11, S  `End_element;
        1, 11, S  `End_element];

    expect "<p>foo<p>bar<div>baz</div>"
      [ 1,  1, S (start_element "html");
        1,  1, S (start_element "head");
        1,  1, S  `End_element;
        1,  1, S (start_element "body");
        1,  1, S (start_element "p");
        1,  4, S (`Text ["foo"]);
        1,  7, S  `End_element;
        1,  7, S (start_element "p");
        1, 10, S (`Text ["bar"]);
        1, 13, S  `End_element;
        1, 13, S (start_element "div");
        1, 18, S (`Text ["baz"]);
        1, 21, S  `End_element;
        1, 27, S  `End_element;
        1, 27, S  `End_element]);

  ("html.parser.headings" >:: fun _ ->
    expect "<p><h1><h2>foo</h2>"
      [ 1,  1, S (start_element "html");
        1,  1, S (start_element "head");
        1,  1, S  `End_element;
        1,  1, S (start_element "body");
        1,  1, S (start_element "p");
        1,  4, S  `End_element;
        1,  4, S (start_element "h1");
        1,  8, E (`Misnested_tag ("h2", "h1"));
        1,  8, S  `End_element;
        1,  8, S (start_element "h2");
        1, 12, S (`Text ["foo"]);
        1, 15, S  `End_element;
        1, 20, S  `End_element;
        1, 20, S  `End_element]);

  ("html.parser.pre" >:: fun _ ->
    expect "<p><pre>foo</pre>"
      [ 1,  1, S (start_element "html");
        1,  1, S (start_element "head");
        1,  1, S  `End_element;
        1,  1, S (start_element "body");
        1,  1, S (start_element "p");
        1,  4, S  `End_element;
        1,  4, S (start_element "pre");
        1,  9, S (`Text ["foo"]);
        1, 12, S  `End_element;
        1, 18, S  `End_element;
        1, 18, S  `End_element];

    expect "<p><pre>\n\nfoo</pre>"
      [ 1,  1, S (start_element "html");
        1,  1, S (start_element "head");
        1,  1, S  `End_element;
        1,  1, S (start_element "body");
        1,  1, S (start_element "p");
        1,  4, S  `End_element;
        1,  4, S (start_element "pre");
        2,  1, S (`Text ["\nfoo"]);
        3,  4, S  `End_element;
        3, 10, S  `End_element;
        3, 10, S  `End_element]);

  ("html.parser.textarea" >:: fun _ ->
    expect "<textarea>foo</p></textarea>"
      [ 1,  1, S (start_element "html");
        1,  1, S (start_element "head");
        1,  1, S  `End_element;
        1,  1, S (start_element "body");
        1,  1, S (start_element "textarea");
        1, 11, S (`Text ["foo</p>"]);
        1, 18, S  `End_element;
        1, 29, S  `End_element;
        1, 29, S  `End_element];

    expect "<textarea>\n\nfoo</p></textarea>"
      [ 1,  1, S (start_element "html");
        1,  1, S (start_element "head");
        1,  1, S  `End_element;
        1,  1, S (start_element "body");
        1,  1, S (start_element "textarea");
        2,  1, S (`Text ["\nfoo</p>"]);
        3,  8, S  `End_element;
        3, 19, S  `End_element;
        3, 19, S  `End_element]);

  ("html.parser.list" >:: fun _ ->
    expect "<ul><li>foo<li>bar</ul>"
      [ 1,  1, S (start_element "html");
        1,  1, S (start_element "head");
        1,  1, S  `End_element;
        1,  1, S (start_element "body");
        1,  1, S (start_element "ul");
        1,  5, S (start_element "li");
        1,  9, S (`Text ["foo"]);
        1, 12, S  `End_element;
        1, 12, S (start_element "li");
        1, 16, S (`Text ["bar"]);
        1, 19, S  `End_element;
        1, 19, S  `End_element;
        1, 24, S  `End_element;
        1, 24, S  `End_element]);

  ("html.parser.definition" >:: fun _ ->
    expect "<p><dt>foo<dd>bar"
      [ 1,  1, S (start_element "html");
        1,  1, S (start_element "head");
        1,  1, S  `End_element;
        1,  1, S (start_element "body");
        1,  1, S (start_element "p");
        1,  4, S  `End_element;
        1,  4, S (start_element "dt");
        1,  8, S (`Text ["foo"]);
        1, 11, S  `End_element;
        1, 11, S (start_element "dd");
        1, 15, S (`Text ["bar"]);
        1, 18, S  `End_element;
        1, 18, S  `End_element;
        1, 18, S  `End_element]);

  ("html.parser.plaintext" >:: fun _ ->
    expect "<p><plaintext>foo</plaintext></p>"
      [ 1,  1, S (start_element "html");
        1,  1, S (start_element "head");
        1,  1, S  `End_element;
        1,  1, S (start_element "body");
        1,  1, S (start_element "p");
        1,  4, S  `End_element;
        1,  4, S (start_element "plaintext");
        1,  4, E (`Unmatched_start_tag "plaintext");
        1, 15, S (`Text ["foo</plaintext></p>"]);
        1, 34, S  `End_element;
        1, 34, S  `End_element;
        1, 34, S  `End_element]);

  ("html.parser.table" >:: fun _ ->
    expect "<p><table><tr><td>foo</td><td>bar</td></tr></table>"
      [ 1,  1, S (start_element "html");
        1,  1, S (start_element "head");
        1,  1, S  `End_element;
        1,  1, S (start_element "body");
        1,  1, S (start_element "p");
        1,  4, S  `End_element;
        1,  4, S (start_element "table");
        1, 11, S (start_element "tbody");
        1, 11, S (start_element "tr");
        1, 15, S (start_element "td");
        1, 19, S (`Text ["foo"]);
        1, 22, S  `End_element;
        1, 27, S (start_element "td");
        1, 31, S (`Text ["bar"]);
        1, 34, S  `End_element;
        1, 39, S  `End_element;
        1, 44, S  `End_element;
        1, 44, S  `End_element;
        1, 52, S  `End_element;
        1, 52, S  `End_element]);

  ("html.parser.select" >:: fun _ ->
    expect "<select><option>foo<option>bar</select>"
      [ 1,  1, S (start_element "html");
        1,  1, S (start_element "head");
        1,  1, S  `End_element;
        1,  1, S (start_element "body");
        1,  1, S (start_element "select");
        1,  9, S (start_element "option");
        1, 17, S (`Text ["foo"]);
        1, 20, S  `End_element;
        1, 20, S (start_element "option");
        1, 28, S (`Text ["bar"]);
        1, 31, S  `End_element;
        1, 31, S  `End_element;
        1, 40, S  `End_element;
        1, 40, S  `End_element]);

  ("html.parser.truncated-body" >:: fun _ ->
    expect "<body>"
      [ 1,  1, S (start_element "html");
        1,  1, S (start_element "head");
        1,  1, S  `End_element;
        1,  1, S (start_element "body");
        1,  7, S  `End_element;
        1,  7, S  `End_element];

    expect "<body></html>"
      [ 1,  1, S (start_element "html");
        1,  1, S (start_element "head");
        1,  1, S  `End_element;
        1,  1, S (start_element "body");
        1,  7, S  `End_element;
        1,  7, S  `End_element]);

  ("html.parser.junk-in-body" >:: fun _ ->
    expect "<body>\x00<!DOCTYPE html><html><meta><body></body>"
      [ 1,  1, S (start_element "html");
        1,  1, S (start_element "head");
        1,  1, S  `End_element;
        1,  1, S (start_element "body");
        1,  7, E (`Bad_token ("U+0000", "body", "null"));
        1,  8, E (`Bad_document "doctype should be first");
        1, 23, E (`Misnested_tag ("html", "body"));
        1, 29, S (start_element "meta");
        1, 29, S  `End_element;
        1, 35, E (`Misnested_tag ("body", "body"));
        1, 41, S  `End_element;
        1, 48, S  `End_element]);

  ("html.parser.foreign" >:: fun _ ->
    expect "<body><svg><g/></svg></body>"
      [ 1,  1, S (start_element "html");
        1,  1, S (start_element "head");
        1,  1, S  `End_element;
        1,  1, S (start_element "body");
        1,  7, S (`Start_element ((svg_ns, "svg"), []));
        1, 12, S (`Start_element ((svg_ns, "g"), []));
        1, 12, S  `End_element;
        1, 16, S  `End_element;
        1, 22, S  `End_element;
        1, 29, S  `End_element]);

  ("html.parser.reconstruct-active-formatting-elements" >:: fun _ ->
    expect "<p><em><strong>foo<p>bar"
      [ 1,  1, S (start_element "html");
        1,  1, S (start_element "head");
        1,  1, S  `End_element;
        1,  1, S (start_element "body");
        1,  1, S (start_element "p");
        1,  4, S (start_element "em");
        1,  8, S (start_element "strong");
        1,  8, E (`Unmatched_start_tag "strong");
        1, 16, S (`Text ["foo"]);
        1, 19, S  `End_element;
        1, 19, S  `End_element;
        1, 19, S  `End_element;
        1, 19, S (start_element "p");
        1,  4, S (start_element "em");
        1,  8, S (start_element "strong");
        1,  8, E (`Unmatched_start_tag "strong");
        1,  4, E (`Unmatched_start_tag "em");
        1, 22, S (`Text ["bar"]);
        1, 25, S  `End_element;
        1, 25, S  `End_element;
        1, 25, S  `End_element;
        1, 25, S  `End_element;
        1, 25, S  `End_element]);

  ("html.parser.fragment" >:: fun _ ->
    expect ~context:(Some (`Fragment "title")) "</p>"
      [ 1,  1, S (`Text ["</p>"])];

    expect ~context:(Some (`Fragment "body")) "</p>"
      [ 1,  1, E (`Unmatched_end_tag "p");
        1,  1, S (start_element "p");
        1,  1, S  `End_element];

    expect ~context:(Some (`Fragment "body")) "<!DOCTYPE html>"
      [ 1,  1, E (`Bad_document "doctype should be first")]);

  ("html.parser.context-detection" >:: fun _ ->
    expect ~context:None "<p>foo</p>"
      [ 1,  1, S (start_element "p");
        1,  4, S (`Text ["foo"]);
        1,  7, S  `End_element]);

  ("html.parser.foreign-context" >:: fun _ ->
    expect ~context:None "<g/>"
      [ 1,  1, S (`Start_element ((svg_ns, "g"), []));
        1,  1, S  `End_element]);

  ("html.parser.bad-self-closing-tag" >:: fun _ ->
    expect "<p/>"
      [ 1,  1, S (start_element "html");
        1,  1, S (start_element "head");
        1,  1, S  `End_element;
        1,  1, S (start_element "body");
        1,  1, E (`Bad_token ("/>", "tag", "should not be self-closing"));
        1,  1, S (start_element "p");
        1,  5, S  `End_element;
        1,  5, S  `End_element;
        1,  5, S  `End_element]);

  ("html.parser.image-tag" >:: fun _ ->
    expect ~context:(Some (`Fragment "body")) "<image/>"
      [ 1,  1, E (`Bad_token ("image", "tag", "should be 'img'"));
        1,  1, S (start_element "img");
        1,  1, S  `End_element]);

  ("html.parser.nulls" >:: fun _ ->
    expect ~context:(Some (`Fragment "svg")) "\x00foo"
      [ 1,  1, E (`Bad_token ("U+0000", "foreign content", "null"));
        1,  1, S (`Text ["\xef\xbf\xbdfoo"])];

    expect ~context:(Some (`Fragment "body")) "<table>\x00foo</table>"
      [ 1,  1, S (start_element "table");
        1,  8, E (`Bad_token ("U+0000", "table", "null"));
        1,  9, E (`Bad_content "table");
        1, 10, E (`Bad_content "table");
        1, 11, E (`Bad_content "table");
        1,  9, S (`Text ["foo"]);
        1, 12, S  `End_element];

    expect ~context:(Some (`Fragment "select")) "\x00foo"
      [ 1,  1, E (`Bad_token ("U+0000", "select", "null"));
        1,  2, S (`Text ["foo"])]);

  ("html.parser.foreign.cdata" >:: fun _ ->
    expect ~context:None "<svg><![CDATA[foo]]></svg>"
      [ 1,  1, S (`Start_element ((svg_ns, "svg"), []));
        1, 15, S (`Text ["foo"]);
        1, 21, S  `End_element]);

  ("html.parser.large-text" >:: fun _ ->
    with_text_limit 8 begin fun () ->
      expect ~context:None "foobar" [ 1,  1, S (`Text ["foobar"])];
      expect ~context:None "foobarbaz" [ 1,  1, S (`Text ["foobarba"; "z"])]
    end)
]
