(* This file is part of Markup.ml, released under the BSD 2-clause license. See
   doc/LICENSE for details, or visit https://github.com/aantron/markup.ml. *)

open Common

let installed_workaround_hook = ref false

exception TailCallWorkaround of (unit -> unit)

let tail_call_workaround k v =
  raise (TailCallWorkaround (fun () -> k v))

let ensure_tail_calls ?hook () =
  let previous_hook = !Lwt.async_exception_hook in
  Lwt.async_exception_hook := (function
    | TailCallWorkaround k -> k ()
    | exn ->
      match hook with
      | None -> previous_hook exn
      | Some hook -> !hook exn);
  installed_workaround_hook := true

let to_cps thread =
  if not !installed_workaround_hook then
    fun throw k -> Lwt.on_any (thread ()) k throw
  else
    fun throw k ->
      Lwt.on_any (thread ())
        (tail_call_workaround k) (tail_call_workaround throw)

module Adapter =
struct
  type 'a t = 'a Lwt.t

  let return = Lwt.return

  let of_cps f =
    let thread, wake = Lwt.wait () in
    f (Lwt.wakeup_exn wake) (Lwt.wakeup wake);
    thread

  let to_cps = to_cps
end

include Markup.Asynchronous (Adapter)

let lwt_stream s = (fun () -> Lwt_stream.get s) |> stream

let to_lwt_stream s = (fun () -> next s) |> Lwt_stream.from
