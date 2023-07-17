(* This file is part of Markup.ml, released under the MIT license. See
   LICENSE.md for details, or visit https://github.com/aantron/markup.ml. *)

open Common

val parse :
  ?get_opens:((unit -> open_elements) option ref) ->
  [< `Document | `Fragment of string ] option ->
  (open_elements -> Error.parse_handler) ->
  (location * Html_tokenizer.token) Kstream.t *
  (Html_tokenizer.state -> unit) *
  ((unit -> bool) -> unit) ->
    (location * signal) Kstream.t
