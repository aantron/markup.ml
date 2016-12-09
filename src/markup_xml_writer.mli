(* This file is part of Markup.ml, released under the BSD 2-clause license. See
   doc/LICENSE for details, or visit https://github.com/aantron/markup.ml. *)

open Markup_common

val write :
  Markup_error.write_handler ->
  (string -> string option) ->
  [< signal ] Markup_kstream.t ->
    string Markup_kstream.t
