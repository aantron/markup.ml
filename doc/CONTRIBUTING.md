# Contributing to Markup.ml

Markup.ml is developed on [GitHub][repo]. Feel free to open an issue, or send me
and email at [antonbachin@yahoo.com][email].

## Getting started

A development version of Markup.ml can be installed in two ways. The easiest is

```
opam source --dev-repo --pin markup
```

The other way is to clone this repo (perhaps after forking it), then execute
`make install` in it. If you go this route, execute `make uninstall` later to
remove the pin.

## Code overview

The library is written in continuation-passing style. Synchronous (module
`Markup`) and asynchronous (module `Markup_lwt`) interfaces are thin layers on
top of that. The typical CPS function "returning" `'a` has signature

```ocaml
arguments -> (exn -> unit) -> ('a -> unit) -> unit
```

The modules depend on each other in roughly the order they are tested in
[`test/test.ml`][test.ml].

## Testing

To test the code, you need package [ounit]. Then, simply run [make test] for
unit tests. If you also have [bisect_ppx] installed, a coverage report will be
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

All tests can be run with [make all-tests]. This also generates the
documentation, to make sure that it is not broken.

[repo]:    https://github.com/aantron/markup.ml
[email]:   mailto:antonbachin@yahoo.com
[test.ml]: https://github.com/aantron/markup.ml/blob/master/test/test.ml
