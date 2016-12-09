(* This file is part of Markup.ml, released under the BSD 2-clause license. See
   doc/LICENSE for details, or visit https://github.com/aantron/markup.ml. *)

open Markup_common

val parse :
  [< `Document | `Fragment of string ] option ->
  Markup_error.parse_handler ->
  (location * Markup_html_tokenizer.token) Markup_kstream.t *
  (Markup_html_tokenizer.state -> unit) *
  ((unit -> bool) -> unit) ->
    (location * signal) Markup_kstream.t
