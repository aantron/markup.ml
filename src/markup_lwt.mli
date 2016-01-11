(* This file is part of Markup.ml, released under the BSD 2-clause license. See
   doc/LICENSE for details, or visit https://github.com/aantron/markup.ml. *)

(** Lwt interface to Markup.ml.

    The majority of the functions in this interface are listed in the signature
    {!Markup.ASYNCHRONOUS}, and are not directly included on this page. There
    are also additional Lwt functions in module {!Markup_lwt_unix}. Those are
    based on [Lwt_io], and have been separated to make this module [Markup_lwt]
    usable on [js_of_ocaml], which does not support [Lwt_io].

    This module is available if Markup.ml is installed when Lwt is installed,
    i.e.

{[
opam install lwt markup
]}

    To link with this module, depend on the findlib package [markup.lwt] instead
    of package [markup]. *)

open Markup

val ensure_tail_calls : ?hook:((exn -> unit) ref) -> unit -> unit
(** Call [ensure_tail_calls ()] before using [Markup_lwt] to avoid stack
    overflows.

    Current versions of Lwt don't interface well with continuation-passing
    style, in which Markup.ml is written. [ensure_tail_calls] installs a
    workaround that abuses [Lwt.async_exception_hook] to avoid this problem.

    If your application needs to use [Lwt.async_exception_hook] for its own
    purposes, there are two options:

    - If you don't need to modify [Lwt.async_exception_hook] after installing
      your hook, simply install it, then call [ensure_tail_calls ()]. When
      [Markup_lwt] receives any exception besides its internal workaround
      exception, it will delegate handling to your hook.
    - If you need to modify the hook after initialization, you can use the
      [~hook] parameter to provide a reference, and modify that reference
      instead of [Lwt.async_exception_hook]. [Markup_lwt] will delegate to the
      handler stored in the reference.

    See {{:https://github.com/ocsigen/lwt/pull/206} Lwt pull request #206}. When
    Lwt makes a release that is compatible with CPS, this function will be
    deprecated, and the implementation will be changed to a no-op.
 *)

include Markup.ASYNCHRONOUS with type 'a io := 'a Lwt.t

val lwt_stream : 'a Lwt_stream.t -> ('a, async) stream
(** Adapts an Lwt stream to a Markup.ml stream. *)

val to_lwt_stream : ('a, _) stream -> 'a Lwt_stream.t
(** Adapts a Markup.ml stream to an Lwt stream. *)

(**/**)

val to_cps : (unit -> 'a Lwt.t) -> (exn -> unit) -> ('a -> unit) -> unit

(**/**)
