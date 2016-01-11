# Markup.ml &nbsp; [![version pre0.5][version]][releases] [![(BSD license)][license-img]][license]

[version]:       https://img.shields.io/badge/version-pre0.5-blue.svg
[license-img]:   https://img.shields.io/badge/license-BSD-blue.svg

Markup.ml is a pair of streaming, error-recovering parsers, one for HTML and one
for XML, with a simple interface: each parser is a function that transforms
streams.

Here is an example of pretty-printing and correcting an HTML fragment. The code
is in the left column.

```ocaml
open Markup;;

string s                "<p><em>Markup.ml<p>rocks!"    (* malformed HTML *)

|> parse_html           `Start_element "p"
                        `Start_element "em"
                        `Text "Markup.ml"
                        ~report (1, 4)  (* can use ~report to abort parsing; *)
                          (`Unmatched_start_tag "em")  (* ignored by default *)
                        `End_element                   (* /em: recovery *)
                        `End_element                   (* /p *)
                        `Start_element "p"
                        `Start_element "em"            (* recovery *)
                        `Text "rocks!"
                        `End_element                   (* /em *)
                        `End_element                   (* /p *)
|> drop_locations
|> pretty_print         (* adjusts the `Text signals *)

|> write_html
|> to_string;;          "<p>
                           <em>Markup.ml</em>
                         </p>
                         <p>
                           <em>rocks!</em>
                         </p>"                         (* valid HTML *)
```

Some features:

- Supports both strict and error-correcting parsing.
- Based on the [HTML5][HTML5] and [XML][XML] specifications. This concerns HTML
  error recovery especially.
- Character encodings detected automatically; emits UTF-8.
- Can be used in simple synchronous style or with [Lwt][lwt].
- Streaming and lazy – partial input is processed as it is received, but only if
  needed.
- Parses input in one pass and does not build up a document representation in
  memory.

The interface is centered around four transformations between byte streams and
signal streams: [`parse_html`][parse_html], [`write_html`][write_html],
[`parse_xml`][parse_xml], and [`write_xml`][write_xml]. These have several
optional arguments for fine-tuning their behavior. The rest of the functions
either input or output byte streams, or transform signal streams in some
interesting way.

Here are some more usage examples:

```ocaml
(* Show up to 10 XML well-formedness errors to the user. Stop after
   the 10th, without reading more input. *)
let report =
  let count = ref 0 in
  fun location error ->
    error |> Error.to_string ~location |> prerr_endline;
    count := !count + 1;
    if !count >= 10 then raise_notrace Exit

string "some xml" |> parse_xml ~report |> drain

(* Load HTML into a custom document tree data type. *)
type html = Text of string | Element of string * html list

file "some_file"
|> parse_html
|> tree
  ~text:(fun s -> Text s)
  ~element:(fun (_, name) _ children -> Element (name, children))
```

The library is subjected to fairly thorough [testing][tests], with more tests on
the way before 1.0 release.

## Installing

Until Markup.ml is added to OPAM, the easiest way to install it is by cloning
this repository, then running

```sh
make install
```

in the cloned directory. This will use OPAM to pin Markup.ml, install the
dependency Uutf, then build and install Markup.ml. If you want to use the module
`Markup_lwt`, check that Lwt is installed before installing Markup.ml.

To remove the pin later, run `make uninstall`.

## Documentation

The interface of Markup.ml is three modules [`Markup`][Markup],
[`Markup_lwt`][Markup_lwt], and [`Markup_lwt_unix`][Markup_lwt_unix].

## Help wanted

Parsing markup has more applications than one person can easily think of, which
makes it difficult to do exhaustive testing. I would greatly appreciate any bug
reports.

While the parsers are in an "advanced" state of completion, there is still
considerable work to be done on standard conformance and speed. Again, any help
would be appreciated.

I have much more experience with Lwt than Async, so if you would like to create
an Async interface, it would be very welcome.

Please see the [CONTRIBUTING][contributing] file.

Feel free to open any issues on GitHub, or send me an email at
[antonbachin@yahoo.com][email].

[![Travis status][travis-img]][travis] [![Coverage][coveralls-img]][coveralls]

[travis]:        https://travis-ci.org/aantron/markup.ml/branches
[travis-img]:    https://img.shields.io/travis/aantron/markup.ml/master.svg
[coveralls]:     google.com
[coveralls-img]: https://img.shields.io/coveralls/aantron/markup.ml/master.svg

## License

Markup.ml is distributed under the BSD license. See [LICENSE][license].

The Markup.ml source distribution includes a copy of the HTML5 entity list,
which is distributed under the W3C document license. The copyright notices and
text of this license are also found in [LICENSE][license].

## Interesting

As it turns out, there is no simple way to read an entire text file into a
string using the standard library of OCaml. If you have Markup.ml installed,
however, you can do

```ocaml
file "foo.txt" |> to_string
```

This only supports text mode.

Markup.ml also makes a decent half of a character encodings library – you can
use it to convert byte sources into Unicode scalar values. For example, suppose
you have a file in UTF-16. Then, you can do

```ocaml
open Encoding
file "encoded.txt" |> decode utf_16 |> iter (*...do something with the ints...*)
```

[releases]:        https://github.com/aantron/markup.ml/releases
[parse_html]:      http://aantron.github.io/markup.ml/#VALparse_html
[write_html]:      http://aantron.github.io/markup.ml/#VALwrite_html
[parse_xml]:       http://aantron.github.io/markup.ml/#VALparse_xml
[write_xml]:       http://aantron.github.io/markup.ml/#VALwrite_xml
[HTML5]:           https://www.w3.org/TR/html5/
[XML]:             https://www.w3.org/TR/xml/
[tests]:           https://github.com/aantron/markup.ml/tree/master/test
[signal]:          http://aantron.github.io/markup.ml/#TYPEsignal
[lwt]:             http://ocsigen.org/lwt/
[license]:         https://github.com/aantron/markup.ml/blob/master/doc/LICENSE
[contributing]:    https://github.com/aantron/markup.ml/blob/master/doc/CONTRIBUTING.md
[email]:           mailto:antonbachin@yahoo.com
[Markup]:          http://aantron.github.io/markup.ml
[Markup_lwt]:      http://aantron.github.io/markup.ml/Markup_lwt.html
[Markup_lwt_unix]: http://aantron.github.io/markup.ml/Markup_lwt_unix.html
