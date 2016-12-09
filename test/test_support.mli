(* This file is part of Markup.ml, released under the BSD 2-clause license. See
   doc/LICENSE for details, or visit https://github.com/aantron/markup.ml. *)

open Markup_common

val wrong_k : string -> _ cont

val with_text_limit : int -> (unit -> unit) -> unit

val expect_error :
  ?allow_recovery:int ->
  location ->
  Markup_error.t ->
  (Markup_error.parse_handler -> unit) ->
    unit

type 'a general_signal = S of 'a | E of Markup_error.t

val expect_signals :
  ?prefix:bool ->
  ('a -> string) ->
  string ->
  (int * int * 'a general_signal) list ->
    Markup_error.parse_handler * ((location * 'a) -> unit cps) * (unit -> unit)

val expect_strings :
  string -> string general_signal list ->
    Markup_error.write_handler * (string -> unit cps) * (unit -> unit)

val iter : ('a -> unit cps) -> 'a Markup_kstream.t -> unit
