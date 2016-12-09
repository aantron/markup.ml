# Contributing to Markup.ml

Markup.ml is developed on [GitHub][repo]. Feel free to open an issue, or send me
and email at [antonbachin@yahoo.com][email].

[repo]:    https://github.com/aantron/markup.ml
[email]:   mailto:antonbachin@yahoo.com

## Getting started

A development version of Markup.ml can be installed in two ways. The easiest is

```
opam source --dev-repo --pin markup
```

The other way is to clone this repo (perhaps after forking it), then execute
`make install` in it. If you go this route, execute `make uninstall` later to
remove the pin.

## Code overview

The library is written in continuation-passing style (CPS). Synchronous (module
`Markup`) and asynchronous (module `Markup_lwt`) interfaces are thin layers on
top of that. The typical CPS function "returning" `'a` has signature

```ocaml
arguments -> (exn -> unit) -> ('a -> unit) -> unit
```

Due to pervasive use of CPS, there are two useful aliases defined in
[`Markup_common`][common]:

```ocaml
type 'a cont = 'a -> unit
type 'a cps = (exn -> unit) -> ('a -> unit) -> unit
```

Pieces of each parser are connected to each other by CPS streams, or *kstreams*,
where *k* is for kontinuation ([markup_kstream.mli][kstream]). The fundamental
function on these streams is:

```ocaml
next : 'a t -> (exn -> unit) -> (unit -> unit) -> ('a -> unit) -> unit
```

such that `next stream on_exn on_empty k` results in:

- a call to `on_exn exn` if retrieving the next element results in exception
  `exn`,
- a call to `on_empty ()` if the end of `stream` is reached, and
- a call to `k v` otherwise, where `v` is the next value in `stream`.

The HTML specification strongly suggests a logical structure for the parser in
the section [*8.2.1 Overview of the parsing model*][model], from where the
following image is brutally ripped:

![HTML parsing model][model-img]

[model]: https://www.w3.org/TR/html5/syntax.html#overview-of-the-parsing-model
[model-img]: https://www.w3.org/TR/html5/images/parsing-model-overview.svg

The XML parser follows the same structure, even though it is not explicitly
suggested.

With the structure and kstreams in mind, the modules can be arranged in the
following logical order. Everything

1.  [`Markup_common`][common] – shared definitions, compiler compatibility,
    etc.,
2.  [`Markup_error`][error] – parsing and serialization error type,
3.  [`Markup.namespace`][namespace] – namespace URI to prefix conversion and
    back,
4.  [`Markup_entities`][entities], [`Markup_trie`][trie] – HTML entities and a
    trie for searching them,
5.  [`Markup_kstream`][kstream] – CPS-friendly streams,
6.  [`Markup_stream_io`][stream_io] – make byte streams from strings, etc.,
    write byte streams to strings, etc. – the first stage of parsing and the
    last stage of serialization ("*Network*"),
7.  [`Markup_encoding`][encoding] – byte streams to Unicode streams ("*Byte
    Stream Decoder*"),
8.  [`Markup_input`][input] – Unicode streams to preprocessed Unicode streams –
    gets rid of things like CR-LF ("*Input Stream Preprocessor*"),
9.  [`Markup_html_tokenizer`][html_tokenizer] – preprocessed Unicode streams to
    HTML lexeme streams ("*Tokenizer*"),
10. [`Markup_html_parser`][html_parser] – HTML lexeme streams to HTML signal
    streams ("*Tree Construction*", except Markup.ml emits SAX signals instead),
11. No DOM is built – that is up to the user of Markup.ml,
12. [`Markup_html_writer`][html_writer] – HTML signal streams back to
    UTF-8-encoded byte streams,
13. [`Markup_xml_tokenizer`][xml_tokenizer], [`Markup_xml_parser`][xml_parser],
    [`Markup_xml_writer`][xml_writer] – as for HTML,
14. [`Markup_text`][text] – some utilities for both tokenizers,
15. [`Markup_detect`][detect] – prescans byte streams to detect encodings,
16. [`Markup_utility`][utility] – convenience functions on signal streams for
    the user, and finally...
17. [`Markup`][main], [`Markup_lwt`][lwt], [`Markup_lwt_unix`][lwt_unix] – the
    public interface for operating all of the above machinery without going
    insane. This is where the CPS implementation is hidden.

Almost everything is based directly on specifications. Most functions are
commented with the HTML or XML specification section number they are
implementing. It may also be useful to see the [conformance status][conformance]
– these are all the known deviations by Markup.ml from the specifications.

[common]: https://github.com/aantron/markup.ml/blob/master/src/markup_common.ml
[error]: https://github.com/aantron/markup.ml/blob/master/src/markup_error.ml
[namespace]: https://github.com/aantron/markup.ml/blob/master/src/markup_namespace.mli
[entities]: https://github.com/aantron/markup.ml/blob/master/src/markup_entities.ml
[trie]: https://github.com/aantron/markup.ml/blob/master/src/markup_trie.ml
[kstream]: https://github.com/aantron/markup.ml/blob/master/src/markup_kstream.mli
[stream_io]: https://github.com/aantron/markup.ml/blob/master/src/markup_stream_io.ml
[encoding]: https://github.com/aantron/markup.ml/blob/master/src/markup_encoding.ml
[input]: https://github.com/aantron/markup.ml/blob/master/src/markup_input.mli
[html_tokenizer]: https://github.com/aantron/markup.ml/blob/master/src/markup_html_tokenizer.mli
[html_parser]: https://github.com/aantron/markup.ml/blob/master/src/markup_html_parser.mli
[html_writer]: https://github.com/aantron/markup.ml/blob/master/src/markup_html_writer.mli
[xml_tokenizer]: https://github.com/aantron/markup.ml/blob/master/src/markup_xml_tokenizer.mli
[xml_parser]: https://github.com/aantron/markup.ml/blob/master/src/markup__xml_parser.mli
[xml_writer]: https://github.com/aantron/markup.ml/blob/master/src/markup_xml_writer.mli
[text]: https://github.com/aantron/markup.ml/blob/master/src/markup_text.ml
[detect]: https://github.com/aantron/markup.ml/blob/master/src/markup_detect.mli
[utility]: https://github.com/aantron/markup.ml/blob/master/src/markup_utility.ml
[main]: https://github.com/aantron/markup.ml/blob/master/src/markup.mli
[lwt]: https://github.com/aantron/markup.ml/blob/master/src/markup_lwt.mli
[lwt_unix]: https://github.com/aantron/markup.ml/blob/master/src/markup_lwt_unix.mli
[status]: http://aantron.github.io/markup.ml/#2_Conformancestatus

## Testing

To test the code, you need package `ounit`. Then, simply run `make test` for
unit tests. If you also have `bisect_ppx` installed, a coverage report will be
generated. There are several other kinds of testing:

- `make performance-test` measures time for Markup.ml to parse some XML and HTML
  files. If you have Ocamlnet and/or Xmlm installed, those libraries will also
  be measured, for comparison.
- `make js-test` checks that `Markup_lwt` can be linked into a `js_of_ocaml`
  program, i.e. that it is not accidentally pulling in any Unix dependencies.
- `make dependency-test` pins and installs Markup.ml using OPAM, then builds
  some small programs that depend on Markup.ml. This tests correct installation
  and that no dependencies are missing. To get the most out of this test, it
  should be done on an OPAM switch that has Lwt but does not have Uutf.

All tests can be run with `make all-tests`. This also generates the
documentation, to make sure that it is not broken.

## Suggestions

- Create `Markup_async`. This would consist of applying the functor
  `Markup.Asynchronous`, and then extending that interface with whatever
  Async-specific functions are useful. You would probably want the `'a io` type
  to be `('a, exn) result Deferred.t`.
