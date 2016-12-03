(* This file is part of Markup.ml, released under the BSD 2-clause license. See
   doc/LICENSE for details, or visit https://github.com/aantron/markup.ml. *)

open Common
open Kstream

let state_fold f initial =
  let state = ref initial in
  (fun throw e k ->
    f !state throw e (fun (c, new_state) ->
      state := new_state; k c))
  |> make

let string s =
  state_fold (fun i _ e k ->
    if i >= String.length s then e () else k (s.[i], i + 1)) 0

let buffer b =
  state_fold (fun i _ e k ->
    if i >= Buffer.length b then e () else k (Buffer.nth b i, i + 1)) 0

type result = Byte of char | Exn of exn

let channel c =
  let ended = ref false in

  (fun throw e k ->
    let result =
      try Byte (Pervasives.input_char c)
      with exn -> Exn exn
    in
    match result with
    | Byte b -> k b
    | Exn End_of_file -> ended := true; e ()
    | Exn exn -> if !ended then e () else throw exn)
  |> make

let file f =
  let c = Pervasives.open_in f in
  let s = channel c in

  let s' =
    (fun throw e k ->
      next s
        (fun exn -> close_in_noerr c; throw exn)
        (fun () -> close_in_noerr c; e ())
        k)
    |> make
  in

  s', fun () -> close_in_noerr c

let to_buffer s throw k =
  let buffer = Buffer.create 4096 in
  iter (fun b _ k -> Buffer.add_char buffer b; k ()) s throw (fun () ->
  k buffer)

let to_string s throw k =
  to_buffer s throw (fun buffer -> k (Buffer.contents buffer))

let to_channel c s throw k =
  let write b throw k =
    let exn =
      try output_char c b; None
      with exn -> Some exn
    in
    match exn with
    | None -> k ()
    | Some exn -> throw exn
  in
  iter write s throw k

let to_file f s throw k =
  let c = Pervasives.open_out f in
  to_channel c s
    (fun exn -> close_out_noerr c; throw exn)
    (fun () -> close_out_noerr c; k ())
