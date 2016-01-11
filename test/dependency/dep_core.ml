(* This file is part of Markup.ml, released under the BSD 2-clause license. See
   doc/LICENSE for details, or visit https://github.com/aantron/markup.ml. *)

open Markup

let (|>) x f = f x

let () =
  "foo"
  |> string |> parse_html
  |> drop_locations |> write_html |> to_string |> ignore
