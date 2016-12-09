(* This file is part of Markup.ml, released under the BSD 2-clause license. See
   doc/LICENSE for details, or visit https://github.com/aantron/markup.ml. *)

open Markup_common

val select_html : ?limit:int -> char Markup_kstream.t -> Markup_encoding.t cps
val select_xml : char Markup_kstream.t -> Markup_encoding.t cps

(* The following values are exposed for testing. They are not used outside the
   module. *)

val normalize_name : bool -> string -> string
val guess_from_bom_html : char Markup_kstream.t -> string option cps
val guess_from_bom_xml : char Markup_kstream.t -> string option cps
val guess_family_xml : char Markup_kstream.t -> string option cps
val meta_tag_prescan :
  ?supported:(string -> bool cont -> unit) ->
  ?limit:int ->
  char Markup_kstream.t -> string option cps
val read_xml_encoding_declaration :
  char Markup_kstream.t -> Markup_encoding.t -> string option cps
