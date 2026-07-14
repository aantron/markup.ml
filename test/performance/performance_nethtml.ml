(* This file is part of Markup.ml, released under the MIT license. See
   LICENSE.md for details, or visit https://github.com/aantron/markup.ml. *)

open Performance_common
open Nethtml

let (|>) x f = f x

let parse s =
  s
  |> Lexing.from_string
  |> parse_document ~dtd:relaxed_html40_dtd
  |> ignore

let () =
  measure 100 "nethtml" "google" "html"
    (fun () -> parse google_page);

  measure 100 "nethtml" "xml_spec" "html"
    (fun () -> parse xml_spec);

  measure 100 "nethtml" "stress_cjk" "html"
    (fun () -> parse stress_cjk);

  measure 100 "nethtml" "stress_formatting" "html"
    (fun () -> parse stress_formatting);

  measure 100 "nethtml" "stress_entities" "html"
    (fun () -> parse stress_entities);

  measure 100 "nethtml" "stress_deep_nesting" "html"
    (fun () -> parse stress_deep_nesting)
