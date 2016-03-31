(* This file is part of Markup.ml, released under the BSD 2-clause license. See
   doc/LICENSE for details, or visit https://github.com/aantron/markup.ml. *)

open Common
open Kstream

let content s =
  let filter signal _ k =
    match signal with
    | `Start_element _ | `End_element | `Text _ as signal -> k (Some signal)
    | `Comment _ | `PI _ | `Doctype _ | `Xml _ -> k None
  in
  filter_map filter s

let strings_to_bytes strings =
  let current_string = ref "" in
  let index = ref 0 in

  let rec emit throw e k =
    if !index < String.length !current_string then begin
      index := !index + 1;
      k (!current_string.[!index - 1])
    end
    else
      next strings throw e (fun s ->
        current_string := s;
        index := 0;
        emit throw e k)
  in
  make emit

let _unwrap_lists ls =
  let current_list = ref [] in

  let rec emit throw e k =
    match !current_list with
    | v::l -> current_list := l; k v
    | [] -> next ls throw e (fun l -> current_list := l; emit throw e k)
  in
  make emit

let trees ?text ?element ?comment ?pi ?xml ?doctype s =
  let rec match_node throw k none =
    next s throw none begin function
      | `Start_element (name, attributes) ->
        match_content [] throw (fun children ->
        match element with
        | None -> match_node throw k none
        | Some element -> k (element name attributes children))

      | `End_element -> none ()

      | `Text ss ->
        begin match text with
        | None -> match_node throw k none
        | Some text -> k (text ss)
        end

      | `Doctype d ->
        begin match doctype with
        | None -> match_node throw k none
        | Some doctype -> k (doctype d)
        end

      | `Xml x ->
        begin match xml with
        | None -> match_node throw k none
        | Some xml -> k (xml x)
        end

      | `PI (t, s) ->
        begin match pi with
        | None -> match_node throw k none
        | Some pi -> k (pi t s)
        end

      | `Comment s ->
        begin match comment with
        | None -> match_node throw k none
        | Some comment -> k (comment s)
        end
    end

  and match_content acc throw k =
    match_node throw
      (fun n -> match_content (n::acc) throw k)
      (fun () -> k (List.rev acc))

  in

  (fun throw e k -> match_node throw k e) |> make

let tree ?text ?element ?comment ?pi ?xml ?doctype s throw k =
  let s' = trees ?text ?element ?comment ?pi ?xml ?doctype s in
  next s' throw (fun () -> k None) (fun t -> k (Some t))

type 'a node =
  [ `Element of name * (name * string) list * 'a list
  | `Text of string
  | `Doctype of doctype
  | `Xml of xml_declaration
  | `PI of string * string
  | `Comment of string ]

let from_tree f node =
  let rec traverse acc node =
    match f node with
    | `Element (name, attributes, children) ->
      children
      |> List.fold_left traverse ((`Start_element (name, attributes))::acc)
      |> fun acc -> `End_element::acc

    | `Text s -> (`Text [s])::acc

    | `Doctype _ | `Xml _ | `PI _ | `Comment _ as node ->
      node::acc
  in

  traverse [] node |> List.rev |> of_list

let elements select s =
  let depth = ref 0 in
  let started = ref 0 in
  let finished = ref 0 in

  let rec scan throw e k =
    next s throw e begin fun signal ->
      match signal with
      | `Start_element (name, attributes)
          when !started = !finished && select name attributes ->

        let index = !started + 1 in
        started := index;
        depth := 0;

        let constructor _ k =
          push s signal;
          (fun throw e k ->
            if !finished >= index then e ()
            else
              next s throw e begin fun signal ->
                match signal with
                | `Start_element _ ->
                  depth := !depth + 1;
                  k signal

                | `End_element ->
                  depth := !depth - 1;
                  if !depth = 0 then
                    finished := index;
                  k signal

                | `Text _ | `Comment _ | `PI _ | `Doctype _ | `Xml _ -> k signal
              end)
          |> make
          |> k
        in

        construct constructor |> k

      | `Start_element _ when !started > !finished ->
        depth := !depth + 1;
        scan throw e k

      | `End_element when !started > !finished ->
        depth := !depth - 1;
        if !depth = 0 then
          finished := !started;
        scan throw e k

      | `Text _ | `Start_element _ | `End_element | `Comment _ | `PI _
      | `Doctype _ | `Xml _ ->
        scan throw e k
    end
  in

  make scan

let text s =
  let filter v _ k =
    match v with
    | `Text ss -> k (Some ss)
    | `Start_element _ | `End_element | `Comment _ | `PI _ | `Doctype _
    | `Xml _ -> k None
  in
  filter_map filter s
  |> _unwrap_lists
  |> strings_to_bytes

let trim s =
  let rec trim_string_list trim = function
    | [] -> []
    | s::more ->
      match trim s with
      | "" -> trim_string_list trim more
      | s -> s::more
  in

  s |> filter_map (fun v _ k ->
    match v with
    | `Text ss ->
      ss
      |> trim_string_list trim_string_left
      |> List.rev
      |> trim_string_list trim_string_right
      |> List.rev
      |> (function
        | [] -> k None
        | ss -> k (Some (`Text ss)))
    | _ -> k (Some v))

let normalize_text s =
  let rec match_text acc throw e k =
    next_option s throw begin function
      | Some (`Text ss) ->
        match_text (ss::acc) throw e k

      | v ->
        push_option s v;
        let ss =
          List.rev acc
          |> List.flatten
          |> List.filter (fun s -> String.length s > 0)
        in
        match ss with
        | [] -> match_other throw e k
        | _ -> k (`Text ss)
    end

  and match_other throw e k =
    next s throw e (function
      | `Text ss -> match_text [ss] throw e k
      | signal -> k signal)

  in

  make match_other

let _tab_width = 2

let pretty_print s =
  let s = s |> normalize_text |> trim in

  let indent n =
    let n = if n < 0 then 0 else n in
    String.make (n * _tab_width) ' '
  in

  let rec current_state = ref (fun throw e k -> row 0 throw e k)

  and row depth throw e k =
    next s throw e begin fun v ->
      match v with
      | `Start_element _ ->
        list [`Text [indent depth]; v; `Text ["\n"]]
          (row (depth + 1)) throw e k

      | `End_element ->
        list [`Text [indent (depth - 1)]; v; `Text ["\n"]]
          (row (depth - 1)) throw e k

      | _ ->
        list [`Text [indent depth]; v; `Text ["\n"]]
          (row depth) throw e k
    end

  and list signals state throw e k =
    match signals with
    | [] -> state throw e k
    | signal::more ->
      current_state := list more state;
      k signal

  in

  (fun throw e k -> !current_state throw e k)
  |> make
  |> normalize_text

let html5 s =
  let remove_markup v _ k =
    match v with
    | `Doctype _ | `Xml _ | `PI _ as v -> k (Some v)
    | `Text _ | `Start_element _ | `End_element | `Comment _ -> k None
  in

  s
  |> filter_map remove_markup
  |> fun s ->
    push s (`Doctype
      {doctype_name      = Some "html";
       public_identifier = None;
       system_identifier = None;
       raw_text          = None;
       force_quirks      = false});
    s

let xhtml ?dtd s =
  let doctype_text =
    match dtd with
    | Some `Strict_1_0 ->
      "html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" " ^
      "\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\""

    | Some `Transitional_1_0 ->
      "html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" " ^
      "\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\""

    | Some `Frameset_1_0 ->
      "html PUBLIC \"-//W3C//DTD XHTML 1.0 Frameset//EN\" " ^
      "\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd\""

    | Some `Strict_1_1 | None ->
      "html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" " ^
      "\"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\""
  in

  let remove_markup v _ k =
    match v with
    | `Doctype _ | `Xml _ as v -> k (Some v)
    | `Text _ | `Start_element _ | `End_element | `Comment _ | `PI _ -> k None
  in

  s
  |> filter_map remove_markup
  |> fun s ->
    push s (`Doctype
      {doctype_name      = None;
       public_identifier = None;
       system_identifier = None;
       raw_text          = Some doctype_text;
       force_quirks      = false});
    push s (`Xml {version = "1.0"; encoding = Some "utf-8"; standalone = None});
    s

let xhtml_entity name =
  let rec lookup index =
    if index >= Array.length Entities.entities then raise Exit
    else
      if fst Entities.entities.(index) <> name then lookup (index + 1)
      else snd Entities.entities.(index)
  in

  try
    let buffer = Buffer.create 8 in

    match lookup 0 with
    | `One c ->
      add_utf_8 buffer c;
      Some (Buffer.contents buffer)
    | `Two (c, c') ->
      add_utf_8 buffer c;
      add_utf_8 buffer c';
      Some (Buffer.contents buffer)

  with Exit -> None
