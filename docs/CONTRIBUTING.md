# Contributing to Markup.ml

Markup.ml is developed on [GitHub][repo]. Feel free to open an issue, or send me
an email at [antonbachin@yahoo.com][email]. If you have a strong interest and
would like to join the project as a co-maintainer, it is welcome!

[repo]:    https://github.com/aantron/markup.ml
[email]:   mailto:antonbachin@yahoo.com

<br/>

#### Table of contents

- [Getting started](#getting-started)
- [Building and testing](#building)
- [Code overview](#code-overview)
  - [Common concepts](#common-concepts)
  - [Structure](#structure)



<br/>

<a id="getting-started"></a>
## Getting started

A development version of Markup.ml can be installed in two ways. The easiest is

```
opam source --dev-repo --pin markup
```

The other way is to clone this repo (perhaps after forking it), then execute
`make install` in it. If you go this route, execute `make uninstall` later to
remove the pin.



<br/>

<a id="building"></a>
## Building and testing

To test the code, you need to have packages `ounit` and `lwt` installed. Then,
simply run `make test` to build the library and run unit tests. To generate a
coverage report, install package `bisect_ppx`, and run `make coverage`. There
are several other kinds of testing:

- `make performance-test` measures time for Markup.ml to parse some XML and HTML
  files. You should have `ocamlnet` and `xmlm` installed. Those libraries will
  also be measured, for comparison.
- `make js-test` checks that `Markup_lwt` can be linked into a `js_of_ocaml`
  program, i.e. that it is not accidentally pulling in any Unix dependencies.
- `make dependency-test` pins and installs Markup.ml using OPAM, then builds
  some small programs that depend on Markup.ml. This tests correct installation
  and that no dependencies are missing.



<br/>

<a id="code-overview"></a>
## Code overview

<a id="common-concepts"></a>
### Common concepts

The library is internally written entirely in continuation-passing style (CPS),
more popularly known simply as *using callbacks*. Except for really trivial
helpers, most internal functions in Markup.ml take two continuations
(callbacks): one to call if the function succeeds, and one to call if it fails
with an exception. So, for a function we would think of as taking as one `int`
argument, and returning a `string`, the type signature would look like this:

```ocaml
val do_something : int -> (exn -> unit) -> (string -> unit) -> unit
```

The code will call it on `1337` as `do_something 1337 throw k`. If
`do_something` succeeds, say with result `"foo"`, it will call `k "foo"`. If it
fails, say with `Exit`, it will call `throw Exit`. Popular names in other
languages for `throw` and `k` are `on_failure` and `on_success`, but this code
was originally written without popular languages in mind :)

The point of all this is that `do_something` doesn't have to return right away:
it can, perhaps transitively, trigger some I/O, and call `throw` or `k` only
later, when the I/O completes.

Due to pervasive use of CPS, there are two useful type aliases defined in
[`Markup.Common`][common]:

```ocaml
type 'a cont = 'a -> unit
type 'a cps = exn cont -> 'a cont -> unit
```

With these aliases, the signature of `do_something` can be abbreviated as:

```ocaml
val do_something : int -> string cps
```

which should be much more legible. It might be a [nice easy PR][rename-cps] to
rename alias `'a cont` to `'a callback`.

[rename-cps]: https://github.com/aantron/markup.ml/issues/23

The other important internal type in Markup.ml is the continuation-passing style
stream, or *kstream* (`k` being the traditional meta-variable for a
continuation). The fundamental operation on a stream is getting the next
element, and for kstreams this looks like:

```ocaml
Kstream.next : 'a Kstream.t -> exn cont -> unit cont -> 'a cont -> unit
```

The way this works is that when you call `next stream on_exn on_empty k`, `next`
eventually calls:

- `on_exn exn` if trying to retrieve the next element resulted in exception
  `exn`,
- `on_empty ()` if the stream ended, or
- `k v` in the remaining case, when the stream has a next value, where we are
  calling that value `v`.

Most of Markup.ml is composed as a big chain of stream processors, tied together
by these kstreams. For example, each parser

- takes a stream of bytes,
- transforms it into a stream of Unicode characters paired with locations,
- transforms that into a stream of language tokens, like an "open tag,"
- and transforms that into a stream of parsing signals, like "the start of an
  element."

<br/>

The synchronous default API of Markup.ml, seen in the [`README`][readme], is a
thin wrapper over this internal implementation, which simply expects each call
to a CPS function to call its continuations (callbacks) *before* the function
returns.

Likewise, the Lwt API is another thin wrapper, which translates between CPS and
Lwt promises.

[readme]: https://github.com/aantron/markup.ml#readme

<br/>

<a id="structure"></a>
### Structure

As for how the stream processors are chained together, The HTML specification
strongly suggests a structure for the parser in the section
[*8.2.1 Overview of the parsing model*][model], from where the following diagram
is ripped without any shame whatsoever:

<p align="center">
<img src="https://www.w3.org/TR/html5/images/parsing-model-overview.svg" />
</p>

[model]: https://www.w3.org/TR/html5/syntax.html#overview-of-the-parsing-model

The XML parser follows the same structure, even though it is not explicitly
suggested by the XML specification.

The modules can be arranged in the following categories. Where a module directly
implements a box from the diagram, it is indicated in boldface.

Until the modules dealing with Lwt, only `Markup.Stream_io` does I/O. The rest
of the modules are pure with respect to I/O.

Almost everything is based directly on specifications. Most functions are
commented with the HTML or XML specification section number they are
implementing. It may also be useful to see the [conformance status][conformance]
– these are all the known deviations by Markup.ml from the specifications.

#### Helpers

- [`Markup.Common`][common] – shared definitions, compiler compatibility, etc.
- [`Markup.Error`][error] – parsing and serialization error type. Markup.ml does
  not throw exceptions, because all errors are recoverable.
- [`Markup.Namespace`][namespace] – namespace URI to prefix conversion and back.
- [`Markup.Entities`][entities] – checked-in auto-generated HTML5 entity list.
  The source for this file is `src/entities.json`, and the generator is
  `src/translate_entities.ml`. Neither of these latter two files is part of the
  built Markup.ml, nor of the build process.
- [`Markup.Trie`][trie] – trie for incrementally searching the entity list.
- [`Markup.Kstream`][kstream] – above-mentioned CPS streams.
- [`Markup.Text`][text] – some utilities for `Markup.Html_tokenizer` and
  `Markup.Xml_tokenizer`; see below.

#### I/O

- [`Markup.Stream_io`][stream_io] – make byte streams from files, strings, etc.,
  write byte streams to strings, etc. – the first stage of parsing and the last
  stage of serialization (**Network** in the diagram). This uses the I/O
  functions in `Pervasives`.

#### Encodings

- [`Markup.Encoding`][encoding] – byte streams to Unicode character streams
  (**Byte Stream Decoder** in the diagram). For UTF-8, this is a wrapper around
  `uutf`.
- [`Markup.Detect`][detect] – prescans byte streams to detect encodings.
- [`Markup.Input`][input] – Unicode streams to "preprocessed" Unicode streams –
  in HTML5 parlance, this just means normalizing CR-LF to CR, and attaching
  locations (**Input Stream Preprocessor** in the diagram).

#### HTML parsing

- [`Markup.Html_tokenizer`][html_tokenizer] – preprocessed Unicode streams to
  HTML lexeme streams (**Tokenizer** in the diagram). HTML lexemes are things
  like start tags, end tags, and runs of text.
- [`Markup.Html_parser`][html_parser] – HTML lexeme streams to HTML signal
  streams (**Tree Construction** in the diagram). Signal streams are things like
  "start an element," "start another element as its child," "now end the child,"
  "now end the root element." They are basically a left-to-right traversal of a
  DOM tree, without the DOM tree actually being in memory.

#### XML parsing

- [`Markup.Xml_tokenizer`][xml_tokenizer] – as for HTML above, but for XML.
- [`Markup.Xml_parser`][xml_parser] - as for HTML above, but for XML.

#### HTML writing

- [`Markup.Html_writer`][html_writer] – HTML signal streams back to
  UTF-8-encoded byte streams.

#### XML writing

- [`Markup.Xml_writer`][xml_writer] - as for HTML above, but for XML.

#### User-friendly APIs

- [`Markup.Utility`][utility] – convenience functions on signal streams for the
  user.
- [`Markup`][main], [`Markup_lwt`][lwt], [`Markup_lwt_unix`][lwt_unix] – the
  public interface for operating all of the above machinery without having to
  touch CPS.

[common]: https://github.com/aantron/markup.ml/blob/master/src/common.ml
[error]: https://github.com/aantron/markup.ml/blob/master/src/error.ml
[namespace]: https://github.com/aantron/markup.ml/blob/master/src/namespace.mli
[entities]: https://github.com/aantron/markup.ml/blob/master/src/entities.ml
[trie]: https://github.com/aantron/markup.ml/blob/master/src/trie.ml
[kstream]: https://github.com/aantron/markup.ml/blob/master/src/kstream.mli
[stream_io]: https://github.com/aantron/markup.ml/blob/master/src/stream_io.ml
[encoding]: https://github.com/aantron/markup.ml/blob/master/src/encoding.ml
[input]: https://github.com/aantron/markup.ml/blob/master/src/input.mli
[html_tokenizer]: https://github.com/aantron/markup.ml/blob/master/src/html_tokenizer.mli
[html_parser]: https://github.com/aantron/markup.ml/blob/master/src/html_parser.mli
[html_writer]: https://github.com/aantron/markup.ml/blob/master/src/html_writer.mli
[xml_tokenizer]: https://github.com/aantron/markup.ml/blob/master/src/xml_tokenizer.mli
[xml_parser]: https://github.com/aantron/markup.ml/blob/master/src/xml_parser.mli
[xml_writer]: https://github.com/aantron/markup.ml/blob/master/src/xml_writer.mli
[text]: https://github.com/aantron/markup.ml/blob/master/src/text.ml
[detect]: https://github.com/aantron/markup.ml/blob/master/src/detect.mli
[utility]: https://github.com/aantron/markup.ml/blob/master/src/utility.ml
[main]: https://github.com/aantron/markup.ml/blob/master/src/markup.mli
[lwt]: https://github.com/aantron/markup.ml/blob/master/src/markup_lwt.mli
[lwt_unix]: https://github.com/aantron/markup.ml/blob/master/src/markup_lwt_unix.mli
[conformance]: http://aantron.github.io/markup.ml/#2_Conformancestatus
