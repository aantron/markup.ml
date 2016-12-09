(* This file is part of Markup.ml, released under the BSD 2-clause license. See
   doc/LICENSE for details, or visit https://github.com/aantron/markup.ml. *)

open Markup_common

val preprocess :
  (int -> bool) -> Markup_error.parse_handler -> int Markup_kstream.t ->
    (location * int) Markup_kstream.t * (unit -> location)
