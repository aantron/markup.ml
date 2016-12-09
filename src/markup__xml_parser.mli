(* This file is part of Markup.ml, released under the BSD 2-clause license. See
   doc/LICENSE for details, or visit https://github.com/aantron/markup.ml. *)

open Markup_common

val parse :
  [< `Document | `Fragment ] option ->
  (string -> string option) ->
  Markup_error.parse_handler ->
  (location * Markup_xml_tokenizer.token) Markup_kstream.t ->
    (location * signal) Markup_kstream.t
