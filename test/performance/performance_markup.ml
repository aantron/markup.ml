(* This file is part of Markup.ml, released under the MIT license. See
   LICENSE.md for details, or visit https://github.com/aantron/markup.ml. *)

open Performance_common
open Markup

let (|>) x f = f x

let parse_html_string s = string s |> parse_html |> signals |> drain

let () =
  measure 100 "markup.ml" "google" "html"
    (fun () -> parse_html_string google_page);

  measure 100 "markup.ml" "xml_spec" "xml"
    (fun () -> string xml_spec |> parse_xml |> signals |> drain);

  measure 100 "markup.ml" "stress_cjk" "html"
    (fun () -> parse_html_string stress_cjk);

  measure 100 "markup.ml" "stress_formatting" "html"
    (fun () -> parse_html_string stress_formatting);

  measure 100 "markup.ml" "stress_entities" "html"
    (fun () -> parse_html_string stress_entities);

  measure 100 "markup.ml" "stress_deep_nesting" "html"
    (fun () -> parse_html_string stress_deep_nesting)
