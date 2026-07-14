(* This file is part of Markup.ml, released under the MIT license. See
   LICENSE.md for details, or visit https://github.com/aantron/markup.ml. *)

(* Report the median of [runs] timed runs. The median is more stable than the
   mean under system noise. [f] is expected to parse an already in-memory
   string, so file I/O is excluded from the measurement. *)
let measure runs library source format f =
  let name = Printf.sprintf "%s: %s (%s)" library source format in

  (* Warm up by running [f] a few times before measuring. *)
  for _ = 1 to max 1 (runs / 10) do f () done;

  let times = Array.make runs 0. in
  for i = 0 to runs - 1 do
    let start_time = Unix.gettimeofday () in
    f ();
    times.(i) <- ((Unix.gettimeofday ()) -. start_time) *. 1000000.
  done;
  Array.sort compare times;

  Printf.printf "  %s: %.0f us\n" name times.(runs / 2)

let read_file path =
  let channel = open_in_bin path in
  let length = in_channel_length channel in
  let content = really_input_string channel length in
  close_in channel;
  content

let google_page = read_file "test/pages/google"
let xml_spec = read_file "test/pages/xml_spec"

(* The documents below each stress one part of the HTML parser. They are built
   here by repeating a small pattern to a demanding size, rather than committed
   as large files, since the pattern is all that matters. *)

let repeat n piece =
  let buffer = Buffer.create (n * String.length piece) in
  for _ = 1 to n do Buffer.add_string buffer piece done;
  Buffer.contents buffer

let html_document body =
  "<!doctype html>\n<html><head><title>stress</title></head><body>\n"
  ^ body ^ "\n</body></html>\n"

(* Multi-byte UTF-8 (CJK), which exercises the decoder's multi-byte path. *)
let stress_cjk =
  html_document
    (repeat 2500
      "<p>甀曒檃檑糲蘥蠩櫋瀩嗢剆坲姏齸圞趲葠蜄蛖砎粁擙樲橚噅尰崺廘榙榾</p>\n")

(* Formatting elements, some deliberately misnested, which exercise the
   adoption agency algorithm. *)
let stress_formatting =
  html_document
    (repeat 1500
      "<p><a href=\"/x\">lorem</a> <em>ipsum</em> <strong>dolor</strong> \
       <b><i>sit</b> amet</i></p>\n")

(* Named, decimal, and hexadecimal character references. *)
let stress_entities =
  html_document
    (repeat 1500
      "<p>lorem&amp;ipsum&#8226;dolor&#x2022;sit&copy;amet&mdash;elit&nbsp;n</p>\n")

(* A single deep chain of block elements. *)
let stress_deep_nesting =
  let depth = 700 in
  html_document (repeat depth "<div>" ^ "<p>content</p>" ^ repeat depth "</div>")
