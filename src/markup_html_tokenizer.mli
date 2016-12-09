(* This file is part of Markup.ml, released under the BSD 2-clause license. See
   doc/LICENSE for details, or visit https://github.com/aantron/markup.ml. *)

open Markup_common

type token =
  [ `Doctype of doctype
  | `Start of Token_tag.t
  | `End of Token_tag.t
  | `Char of int
  | `Comment of string
  | `EOF ]

type state = [ `Data | `RCDATA | `RAWTEXT | `Script_data | `PLAINTEXT ]

val tokenize :
  Markup_error.parse_handler ->
  (location * int) Markup_kstream.t * (unit -> location) ->
    (location * token) Markup_kstream.t *
    (state -> unit) *
    ((unit -> bool) -> unit)
