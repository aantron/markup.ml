# Markup.ml &nbsp; [![version 0.7.1][version]][releases] [![Documentation][docs-img]][Markup] [![BSD license][license-img]][license] [![Travis status][travis-img]][travis] [![Coverage][coveralls-img]][coveralls]

[version]:       https://img.shields.io/badge/version-0.7.1-blue.svg
[docs-img]:      https://img.shields.io/badge/docs-online-blue.svg
[license-img]:   https://img.shields.io/badge/license-BSD-blue.svg
[travis]:        https://travis-ci.org/aantron/markup.ml/branches
[travis-img]:    https://img.shields.io/travis/aantron/markup.ml/master.svg
[coveralls]:     https://coveralls.io/github/aantron/markup.ml?branch=master
[coveralls-img]: https://img.shields.io/coveralls/aantron/markup.ml/master.svg

Markup.ml is a pair of best-effort parsers implementing the HTML5 and XML
specifications. Usage is simple, because each parser is just a function from
byte streams to parsing signal streams:

![Usage example][sample]

[sample]: https://github.com/aantron/markup.ml/blob/master/doc/sample.png

In addition to being error-correcting, the parsers are:

- **streaming**: capable of parsing partial input and emitting signals while
  more input is still being received;
- **lazy**: not parsing input unless you have requested the next parsing signal,
  so you can easily stop parsing partway through a document;
- **non-blocking**: they can be used with [Lwt][lwt], but still provide a
  straightforward synchronous interface for simple usage; and
- **one-pass**: memory consumption is limited since the parsers don't build up a
  document representation, nor buffer input beyond a small amount of lookahead.

The parsers detect character encodings automatically, and emit everything in
UTF-8.

Here is a breakdown showing the signal stream and errors emitted during the
parsing and pretty-printing of `bad_html`:

```ocaml
string bad_html         "<body><p><em>Markup.ml<p>rocks!"

|> parse_html           `Start_element "body"
|> signals              `Start_element "p"
                        `Start_element "em"
                        `Text ["Markup.ml"]
                        ~report (1, 10) (`Unmatched_start_tag "em")
                        `End_element                   (* /em: recovery *)
                        `End_element                   (* /p: not an error *)
                        `Start_element "p"
                        `Start_element "em"            (* recovery *)
                        `Text ["rocks!"]
                        `End_element                   (* /em *)
                        `End_element                   (* /p *)
                        `End_element                   (* /body *)

|> pretty_print         (* adjusts the `Text signals *)

|> write_html
|> to_channel stdout;;  "...shown above..."            (* valid HTML *)
```

The parsers are [tested][tests] thoroughly.

For a higher-level parser, see [Lambda Soup][lambdasoup], which is based on
Markup.ml, but can search documents using CSS selectors, and perform various
manipulations.

## Overview and basic usage

The interface is centered around four functions between byte streams and signal
streams: [`parse_html`][parse_html], [`write_html`][write_html],
[`parse_xml`][parse_xml], and [`write_xml`][write_xml]. These have several
optional arguments for fine-tuning their behavior. The rest of the functions
either [input][input] or [output][output] byte streams, or
[transform][transform] signal streams in some interesting way.

Here is an example with an optional argument:

```ocaml
(* Show up to 10 XML well-formedness errors to the user. Stop after
   the 10th, without reading more input. *)
let report =
  let count = ref 0 in
  fun location error ->
    error |> Error.to_string ~location |> prerr_endline;
    count := !count + 1;
    if !count >= 10 then raise_notrace Exit

file "some.xml" |> parse_xml ~report |> signals |> drain
```

[input]: http://aantron.github.io/markup.ml/#2_Inputsources
[output]: http://aantron.github.io/markup.ml/#2_Outputdestinations
[transform]: http://aantron.github.io/markup.ml/#2_Utility

## Advanced: [Cohttp][cohttp] + Markup.ml + [Lambda Soup][lambdasoup] + [Lwt][lwt]

The code below is a complete program that requests a Google search, then
performs a streaming scrape of result titles. The first GitHub link is printed,
then the program exits without waiting for the rest of input. Perhaps early exit
is not so important for a Google results page, but it may be needed for large
documents. Memory consumption is low because only the `h3` elements are
converted into DOM-like trees.

```ocaml
open Lwt.Infix

let () =
  Markup_lwt.ensure_tail_calls ();    (* Workaround for current Lwt :( *)

  Lwt_main.run begin
    Uri.of_string "https://www.google.com/search?q=markup.ml"
    |> Cohttp_lwt_unix.Client.get
    >|= snd                           (* Assume success and get body. *)
    >|= Cohttp_lwt_body.to_stream     (* Now an Lwt_stream.t. *)
    >|= Markup_lwt.lwt_stream         (* Now a Markup.stream. *)
    >|= Markup.strings_to_bytes
    >|= Markup.parse_html
    >|= Markup.signals
    >|= Markup.elements (fun name _ -> snd name = "h3")
    >>= Markup_lwt.iter begin fun h3_subtree ->
      h3_subtree
      |> Markup_lwt.to_list
      >|= Markup.of_list
      >|= Soup.from_signals
      >|= fun soup ->
        let open Soup in
        match soup $? "a[href*=github]" with
        | None -> ()
        | Some a ->
          a |> texts |> List.iter print_string;
          print_newline ();
          exit 0
    end
  end
```

This prints `aantron/markup.ml · GitHub`. To run it, do:

```sh
ocamlfind opt -linkpkg -package lwt.unix -package cohttp.lwt \
    -package markup.lwt -package lambdasoup scrape.ml && ./a.out
```

You can get all the necessary packages by

```sh
opam install lwt ssl cohttp lambdasoup markup
```

## Installing

```sh
opam install markup
```

## Documentation

The interface of Markup.ml is three modules: [`Markup`][Markup],
[`Markup_lwt`][Markup_lwt], and [`Markup_lwt_unix`][Markup_lwt_unix]. The last
two are available only if you have [Lwt][lwt] installed.

The documentation includes a summary of the [conformance status][conformance] of
Markup.ml.

## Help wanted

Parsing markup has more applications than one person can easily think of, which
makes it difficult to do exhaustive testing. I would greatly appreciate any bug
reports.

Although the parsers are in an "advanced" state of completion, there is still
considerable work to be done on standard conformance and speed. Again, any help
would be appreciated.

I have much more experience with Lwt than Async, so if you would like to create
an Async interface, it would be very welcome.

Please see the [`CONTRIBUTING`][contributing] file. Feel free to open issues on
GitHub, or send me an email at [antonbachin@yahoo.com][email].

## License

Markup.ml is distributed under the [BSD license][license]. The Markup.ml source
distribution includes a copy of the HTML5 entity list, which is distributed
under the W3C document license. The copyright notices and text of this license
are found in [`LICENSE`][license].

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
[lambdasoup]:      https://github.com/aantron/lambda-soup
[cohttp]:          https://github.com/mirage/ocaml-cohttp
[license]:         https://github.com/aantron/markup.ml/blob/master/doc/LICENSE
[contributing]:    https://github.com/aantron/markup.ml/blob/master/doc/CONTRIBUTING.md
[email]:           mailto:antonbachin@yahoo.com
[Markup]:          http://aantron.github.io/markup.ml
[Markup_lwt]:      http://aantron.github.io/markup.ml/Markup_lwt.html
[Markup_lwt_unix]: http://aantron.github.io/markup.ml/Markup_lwt_unix.html
[conformance]:     http://aantron.github.io/markup.ml/#2_Conformancestatus
