(* This file is part of Markup.ml, released under the BSD 2-clause license. See
   doc/LICENSE for details, or visit https://github.com/aantron/markup.ml. *)

open Common

val wrong_k : string -> _ cont

val expect_error :
  ?allow_recovery:int -> location -> Error.t -> (Error.parse_handler -> unit) ->
    unit

type 'a general_signal = S of 'a | E of Error.t

val expect_signals :
  ?prefix:bool ->
  ('a -> string) ->
  string ->
  (int * int * 'a general_signal) list ->
    Error.parse_handler * ((location * 'a) -> unit cps) * (unit -> unit)

val expect_strings :
  string -> string general_signal list ->
    Error.write_handler * (string -> unit cps) * (unit -> unit)

val iter : ('a -> unit cps) -> 'a Kstream.t -> unit
