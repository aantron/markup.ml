(* This file is part of Markup.ml, released under the BSD 2-clause license. See
   doc/LICENSE for details, or visit https://github.com/aantron/markup.ml. *)

open Common
open Token_tag
open Kstream

let detect_context tokens throw k =
  let tokens, restore = checkpoint tokens in
  let k context = restore (); k context in

  let rec scan () =
    next_expected tokens throw begin function
      | _, `Doctype _ -> k `Document
      | _, `Char c when not @@ is_whitespace c -> k (`Fragment "body")
      | _, `Char _ -> scan ()
      | _, `EOF -> k (`Fragment "body")
      | _, `Start {name = "head" | "body" | "frameset"} ->
        k (`Fragment "html")
      | _, `Start {name =
          "base" | "basefont" | "bgsound" | "link" | "meta" | "noframes" |
          "noscript" | "script" | "style" | "template" | "title"} ->
        k (`Fragment "head")
      | _, `Start {name = "frame"} -> k (`Fragment "frameset")
      | _, `Start {name = "li"} -> k (`Fragment "ul")
      | _, `Start {name =
          "caption" | "col" | "colgroup" | "tbody" | "tfoot" | "thead"} ->
        k (`Fragment "table")
      | _, `Start {name = "tr"} -> k (`Fragment "tbody")
      | _, `Start {name = "td" | "th"} -> k (`Fragment "tr")
      | _, `Start {name = "optgroup" | "option"} -> k (`Fragment "select")
      | _, `Start {name =
          "altglyph" | "altglyphdef" | "altglyphitem" | "animate" |
          "animatecolor" | "animatemotion" | "animatetransform" | "circle" |
          "clippath" | "color-profile" | "cursor" | "defs" | "desc" |
          "ellipse" | "feblend" | "fecolormatrix" | "fecomponenttransfer" |
          "fecomposite" | "fediffuselighting" | "fedisplacementmap" |
          "fedistantlight" | "feflood" | "fefunca" | "fefuncb" | "fefuncg" |
          "fefuncr" | "fegaussianblur" | "feimage" | "femerge" |
          "femergenode" | "femorphology" | "feoffset" | "fepointlight" |
          "fespecularlighting" | "fespotlight" | "fetile" | "feturbulence" |
          "filter" | "font-face" | "font-face-format" | "font-face-name" |
          "font-face-src" | "font-face-uri" | "foreignobject" | "g" |
          "glyph" | "glyphref" | "hkern" | "image" | "line" |
          "lineargradient" | "marker" | "mask" | "metadata" |
          "missing-glyph" | "mpath" | "path" | "pattern" | "polygon" |
          "polyline" | "radialgradient" | "rect" | "set" | "stop" | "switch" |
          "symbol" | "text" | "textpath" | "tref" | "tspan" | "use"} ->
        k (`Fragment "svg")
      | _, `Start {name =
          "maction" | "maligngroup" | "malignmark" | "menclose" | "merror" |
          "mfenced" | "mfrac" | "mglyph" | "mi" | "mlabeledtr" | "mlongdiv" |
          "mmultiscripts" | "mn" | "mo" | "mover" | "mpadded" | "mphantom" |
          "mroot" | "mrow" | "ms" | "mscarries" | "mscarry" | "msgroup" |
          "msline" | "mspace" | "msqrt" | "msrow" | "mstack" | "mstyle" |
          "msub" | "msup" | "msubsup" | "mtable" | "mtd" | "mtext" | "mtr" |
          "munder" | "munderover" | "semantics" | "annotation" |
          "annotation-xml"} ->
        k (`Fragment "math")
      | _, `Start _ -> k (`Fragment "body")
      | _, (`End _ | `Comment _) -> scan ()
    end
  in

  scan ()

type namespace = [ `HTML | `MathML | `SVG | `Other of string ]

let namespace_to_string = function
  | `HTML -> html_ns
  | `MathML -> mathml_ns
  | `SVG -> svg_ns
  | `Other s -> s

type _element_state =
  {element_name              : namespace * string;
   location                  : location;
   is_html_integration_point : bool;
   suppress                  : bool;
   mutable is_open           : bool}

type _active_formatting_element =
  | Marker
  | Element of _element_state * location * Token_tag.t

let mathml_text_integration_points =
  [`MathML, "mi"; `MathML, "mo"; `MathML, "mn"; `MathML, "ms";
   `MathML, "mtext"]

let adjust_mathml_attributes attributes =
  attributes |> List.map (fun ((ns, name), value) ->
    let name =
      if ns = mathml_ns && name = "definitionurl" then "definitionURL"
      else name
    in
    (ns, name), value)

let adjust_svg_attributes attributes =
  attributes |> List.map (fun ((ns, name), value) ->
    if ns <> svg_ns then (ns, name), value
    else
      let name =
        match name with
        | "attributename" -> "attributeName"
        | "attributetype" -> "attributeType"
        | "basefrequency" -> "baseFrequency"
        | "baseprofile" -> "baseProfile"
        | "calcmode" -> "calcMode"
        | "clippathunits" -> "clipPathUnits"
        | "contentscripttype" -> "contentScriptType"
        | "contentstyletype" -> "contentStyleType"
        | "diffuseconstant" -> "diffuseConstant"
        | "edgemode" -> "edgeMode"
        | "externalresourcesrequired" -> "externalResourcesRequired"
        | "filterres" -> "filterRes"
        | "filterunits" -> "filterUnits"
        | "glyphref" -> "glyphRef"
        | "gradienttransform" -> "gradientTransform"
        | "gradientunits" -> "gradientUnits"
        | "kernelmatrix" -> "kernelMatrix"
        | "kernelunitlength" -> "kernelUnitLength"
        | "keypoints" -> "keyPoints"
        | "keysplines" -> "keySplines"
        | "keytimes" -> "keyTimes"
        | "lengthadjust" -> "lengthAdjust"
        | "limitingconeangle" -> "limitingConeAngle"
        | "markerheight" -> "markerHeight"
        | "markerunits" -> "markerUnits"
        | "markerwidth" -> "markerWidth"
        | "maskcontentunits" -> "maskContentUnits"
        | "maskunits" -> "maskUnits"
        | "numoctaves" -> "numOctaves"
        | "pathlength" -> "pathLength"
        | "patterncontentunits" -> "patternContentUnits"
        | "patterntransform" -> "patternTransform"
        | "patternunits" -> "patternUnits"
        | "pointsatx" -> "pointsAtX"
        | "pointsaty" -> "pointsAtY"
        | "pointsatz" -> "pointsAtZ"
        | "preservealpha" -> "preserveAlpha"
        | "preserveaspectratio" -> "preserveAspectRatio"
        | "primitiveunits" -> "primitiveUnits"
        | "refx" -> "refX"
        | "refy" -> "refY"
        | "repeatcount" -> "repeatCount"
        | "repeatdur" -> "repeatDur"
        | "requiredextensions" -> "requiredExtensions"
        | "requiredfeatures" -> "requiredFeatures"
        | "specularconstant" -> "specularConstant"
        | "specularexponent" -> "specularExponent"
        | "spreadmethod" -> "spreadMethod"
        | "startoffset" -> "startOffset"
        | "stddeviation" -> "stdDeviation"
        | "stitchtiles" -> "stitchTiles"
        | "surfacescale" -> "surfaceScale"
        | "systemlanguage" -> "systemLanguage"
        | "tablevalues" -> "tableValues"
        | "targetx" -> "targetX"
        | "targety" -> "targetY"
        | "textlength" -> "textLength"
        | "viewbox" -> "viewBox"
        | "viewtarget" -> "viewTarget"
        | "xchannelselector" -> "xChannelSelector"
        | "ychannelselector" -> "yChannelSelector"
        | "zoomandpan" -> "zoomAndPan"
        | _ -> name
      in
      (ns, name), value)

let adjust_svg_tag_name = function
  | "altglyph" -> "altGlyph"
  | "altglyphdef" -> "altGlyphDef"
  | "altglyphitem" -> "altGlyphItem"
  | "animatecolor" -> "animateColor"
  | "animatemotion" -> "animateMotion"
  | "animatetransform" -> "animateTransform"
  | "clippath" -> "clipPath"
  | "feblend" -> "feBlend"
  | "fecolormatrix" -> "feColorMatrix"
  | "fecomponenttransfer" -> "feComponentTransfer"
  | "fecomposite" -> "feComposite"
  | "feconvolvematrix" -> "feConvolveMatrix"
  | "fediffuselighting" -> "feDiffuseLighting"
  | "fedisplacementmap" -> "feDisplacementMap"
  | "fedistantlight" -> "feDistantLight"
  | "fedropshadow" -> "feDropShadow"
  | "feflood" -> "feFlood"
  | "fefunca" -> "feFuncA"
  | "fefuncb" -> "feFuncB"
  | "fefuncg" -> "feFuncG"
  | "fefuncr" -> "feFuncR"
  | "fegaussianblur" -> "feGaussianBlur"
  | "feimage" -> "feImage"
  | "femerge" -> "feMerge"
  | "femergenode" -> "feMergeNode"
  | "femorphology" -> "feMorphology"
  | "feoffset" -> "feOffset"
  | "fepointlight" -> "fePointLight"
  | "fespecularlighting" -> "feSpecularLighting"
  | "fespotlight" -> "feSpotLight"
  | "fetile" -> "feTile"
  | "feturbulence" -> "feTurbulence"
  | "foreignobject" -> "foreignObject"
  | "glyphref" -> "glyphRef"
  | "lineargradient" -> "linearGradient"
  | "radialgradient" -> "radialGradient"
  | "textpath" -> "textPath"
  | s -> s

let special_category =
  [`HTML, "address"; `HTML, "applet"; `HTML, "area";
   `HTML, "article"; `HTML, "aside"; `HTML, "base";
   `HTML, "basefont"; `HTML, "bgsound"; `HTML, "blockquote";
   `HTML, "body"; `HTML, "br"; `HTML, "button";
   `HTML, "caption"; `HTML, "center"; `HTML, "col";
   `HTML, "colgroup"; `HTML, "dd"; `HTML, "details";
   `HTML, "dir"; `HTML, "div"; `HTML, "dl";
   `HTML, "dt"; `HTML, "embed"; `HTML, "fieldset";
   `HTML, "figcaption"; `HTML, "figure"; `HTML, "footer";
   `HTML, "form"; `HTML, "frame"; `HTML, "frameset";
   `HTML, "h1"; `HTML, "h2"; `HTML, "h3";
   `HTML, "h4"; `HTML, "h5"; `HTML, "h6";
   `HTML, "head"; `HTML, "header"; `HTML, "hgroup";
   `HTML, "hr"; `HTML, "html"; `HTML, "iframe";
   `HTML, "img"; `HTML, "input"; `HTML, "isindex";
   `HTML, "li"; `HTML, "link"; `HTML, "listing";
   `HTML, "main"; `HTML, "marquee"; `HTML, "meta";
   `HTML, "nav"; `HTML, "noembed"; `HTML, "noframes";
   `HTML, "noscript"; `HTML, "object"; `HTML, "ol";
   `HTML, "p"; `HTML, "param"; `HTML, "plaintext";
   `HTML, "pre"; `HTML, "script"; `HTML, "section";
   `HTML, "select"; `HTML, "source"; `HTML, "style";
   `HTML, "summary"; `HTML, "table"; `HTML, "tbody";
   `HTML, "td"; `HTML, "template"; `HTML, "textarea";
   `HTML, "tfoot"; `HTML, "th"; `HTML, "thead";
   `HTML, "title"; `HTML, "tr"; `HTML, "track";
   `HTML, "ul"; `HTML, "wbr"; `HTML, "xmp";
   `MathML, "mi"; `MathML, "mo"; `MathML, "mn";
   `MathML, "ms"; `MathML, "mtext"; `MathML, "annotation-xml";
   `SVG, "foreignObject"; `SVG, "desc"; `SVG, "title"]

let parse_name default_namespace (name, value) =
  try
    let index = String.index name ':' in
    (String.sub name 0 index,
     String.sub name (index + 1) (String.length name - index - 1)),
    value

  with Not_found -> (default_namespace, name), value

let parse requested_context report (tokens, set_tokenizer_state, set_foreign) =
  let context = ref `Document in
  let context_element = ref None in

  let throw = ref (fun _ -> ()) in
  let ended = ref (fun _ -> ()) in
  let output = ref (fun _ -> ()) in

  let report_if = Error.report_if report in
  let unmatched_end_tag l name k =
    report l (`Unmatched_end_tag name) !throw k in

  let open_elements = ref [] in
  let active_formatting_elements = ref [] in
  let text = Text.prepare () in
  let template_insertion_modes = ref [] in
  let frameset_ok = ref true in

  let add_character = Text.add text in

  let current_element () =
    match !open_elements with
    | [] -> None
    | element::_ -> Some element
  in

  let adjusted_current_element () =
    match !open_elements, !context_element with
    | [_], Some element -> Some element
    | [], _ -> None
    | element::_, _ -> Some element
  in

  set_foreign (fun () ->
    match adjusted_current_element () with
    | Some {element_name = ns, _} when ns <> `HTML -> true
    | _ -> false);

  let current_element_is names =
    match !open_elements with
    | {element_name = `HTML, name}::_ -> List.mem name names
    | _ -> false
  in

  let stack_has name =
    List.exists
      (fun {element_name = ns, name'} ->
        ns = `HTML && name' = name) !open_elements
  in

  let report_if_stack_has_other_than names k =
    let rec iterate = function
      | [] -> k ()
      | {element_name = ns, name; location}::more ->
        report_if (not (ns = `HTML && List.mem name names))
          location (fun () -> `Unmatched_start_tag name) !throw (fun () ->
        iterate more)
    in
    iterate !open_elements
  in

  let default_scope_delimiters =
    [`HTML, "applet"; `HTML, "caption"; `HTML, "html";
     `HTML, "table"; `HTML, "td"; `HTML, "th";
     `HTML, "marquee"; `HTML, "object"; `HTML, "template";
     `MathML, "mi"; `MathML, "mo"; `MathML, "mn";
     `MathML, "ms"; `MathML, "mtext"; `MathML, "annotation-xml";
     `SVG, "foreignObject"; `SVG, "desc"; `SVG, "title"]
  in

  let element_in_scope ?(scope_delimiters = default_scope_delimiters) name' =
    let rec scan = function
      | [] -> false
      | {element_name = ns, name'' as name}::more ->
        if ns = `HTML && name'' = name' then true
        else
          if List.mem name scope_delimiters then false
          else scan more
    in
    scan !open_elements
  in

  let element_in_button_scope =
    element_in_scope ~scope_delimiters:
      ((`HTML, "button")::default_scope_delimiters)
  in

  let element_in_list_item_scope =
    element_in_scope ~scope_delimiters:
      ((`HTML, "ol")::(`HTML, "ul")::default_scope_delimiters)
  in

  let element_in_table_scope =
    element_in_scope ~scope_delimiters:
      [`HTML, "html"; `HTML, "table"; `HTML, "template"]
  in

  let element_in_select_scope name =
    let rec scan = function
      | [] -> false
      | {element_name = ns, name'}::more ->
        if ns <> `HTML then false
        else
          if name' = name then true
          else
            if name' = "optgroup" || name' = "option" then scan more
            else false
    in
    scan !open_elements
  in

  let one_of_elements_in_scope names =
    let rec scan = function
      | [] -> false
      | {element_name = ns, name' as name}::more ->
        if ns = `HTML && List.mem name' names then true
        else
          if List.mem name default_scope_delimiters then false
          else scan more
    in
    scan !open_elements
  in

  let one_of_elements_in_table_scope names =
    let rec scan = function
      | [] -> false
      | {element_name = ns, name' as name}::more ->
        if ns = `HTML && List.mem name' names then true
        else
          if List.mem name
              [`HTML, "html"; `HTML, "table"; `HTML, "template"] then
            false
          else scan more
    in
    scan !open_elements
  in

  let push_template_insertion_mode mode =
    template_insertion_modes := mode::!template_insertion_modes in

  let pop_template_insertion_mode () =
    match !template_insertion_modes with
    | [] -> ()
    | _::rest -> template_insertion_modes := rest
  in

  let add_formatting_marker () =
    active_formatting_elements := Marker::!active_formatting_elements in

  let clear_formatting_elements_until_marker () =
    let rec iterate = function
      | Marker::rest -> rest
      | (Element _)::rest -> iterate rest
      | [] -> []
    in
    active_formatting_elements := iterate !active_formatting_elements
  in

  let is_not_hidden tag =
    tag.Token_tag.attributes |> List.exists (fun (name, value) ->
      name = "type" && value <> "hidden")
  in

  let rec current_mode = ref initial_mode

  and emit' l s m = current_mode := m; !output (l, s)

  and emit_text m =
    match Text.emit text with
    | None -> m ()
    | Some (l', strings) ->
      emit' l' (`Text strings) m

  and emit l s m = emit_text (fun () -> emit' l s m)

  and push_and_emit
      ?(formatting = false) ?(acknowledge = false) ?(namespace = `HTML)
      location ({Token_tag.name; attributes; self_closing} as tag) mode =

    report_if (self_closing && not acknowledge) location (fun () ->
      `Bad_token ("/>", "tag", "should not be self-closing"))
      !throw (fun () ->

    let namespace_string = namespace_to_string namespace in

    let tag_name =
      match namespace with
      | `SVG -> adjust_svg_tag_name name
      | _ -> name
    in

    let is_html_integration_point =
      match namespace with
      | `HTML | `Other _ -> false
      | `MathML ->
        tag_name = "annotation-xml" &&
        attributes |> List.exists (function
          | "encoding", "text/html" -> true
          | "encoding", "application/xhtml+xml" -> true
          | _ -> false)
      | `SVG ->
        List.mem tag_name ["foreignObject"; "desc"; "title"]
    in

    let attributes = List.map (parse_name namespace_string) attributes in
    let attributes =
      match namespace with
      | `HTML | `Other _ -> attributes
      | `MathML -> adjust_mathml_attributes attributes
      | `SVG -> adjust_svg_attributes attributes
    in

    let element_entry =
      {element_name = namespace, tag_name;
       location;
       is_html_integration_point;
       suppress     = false;
       is_open      = true}
    in
    open_elements := element_entry::!open_elements;

    if formatting then
      active_formatting_elements :=
        Element (element_entry, location, tag)::
          !active_formatting_elements;

    emit location
      (`Start_element ((namespace_string, tag_name), attributes)) mode)

  and push_implicit location name mode =
    push_and_emit location
      {Token_tag.name = name; attributes = []; self_closing = false} mode

  and pop location mode =
    match !open_elements with
    | [] -> mode ()
    | element::more ->
      open_elements := more;
      element.is_open <- false;
      if element.suppress then mode ()
      else emit location `End_element mode

  and pop_until condition location mode =
    let rec iterate () =
      match !open_elements with
      | [] -> mode ()
      | element::_ ->
        if condition element then mode ()
        else pop location iterate
    in
    iterate ()

  and close_element ?(ns = `HTML) l name mode =
    pop_until
      (fun {element_name = ns', name'} -> ns' = ns && name' = name) l
      (fun () ->
    pop l mode)

  and pop_until_and_raise_errors names location mode =
    let rec iterate () =
      match !open_elements with
      | [] -> mode ()
      | {element_name = ns, name}::_ ->
        if ns = `HTML && List.mem name names then pop location mode
        else
          report location (`Unmatched_start_tag name) !throw (fun () ->
          pop location iterate)
    in
    iterate ()

  and pop_implied ?(except = "") location mode =
    pop_until (fun {element_name = _, name} ->
      name = except ||
        not @@ List.mem name
          ["dd"; "dt"; "li"; "option"; "optgroup"; "p"; "rb"; "rp"; "rt";
           "rtc"]) location mode

  and pop_to_table_context location mode =
    pop_until (function
      | {element_name = `HTML, ("table" | "template" | "html")} -> true
      | _ -> false) location mode

  and pop_to_table_body_context location mode =
    pop_until (function
      | {element_name =
          `HTML, ("tbody" | "thead" | "tfoot" | "template" | "html")} -> true
      | _ -> false) location mode

  and pop_to_table_row_context location mode =
    pop_until (function
      | {element_name = `HTML, ("tr" | "template" | "html")} -> true
      | _ -> false) location mode

  and close_element_with_implied name location mode =
    pop_implied ~except:name location (fun () ->
    let check_element k =
      match current_element () with
      | Some {element_name = `HTML, name'} when name' = name -> k ()
      | Some {element_name = _, name; location} ->
        report location (`Unmatched_start_tag name) !throw k
      | None ->
        unmatched_end_tag location name k
    in
    check_element (fun () ->
    close_element location name mode))

  and close_cell location mode =
    pop_implied location (fun () ->
    (fun mode ->
      match current_element () with
      | Some {element_name = `HTML, ("td" | "th")} -> mode ()
      | Some {element_name = _, name} ->
        unmatched_end_tag location name mode
      | None ->
        unmatched_end_tag location "" mode)
    @@ (fun () ->
    pop_until (function
      | {element_name = `HTML, ("td" | "th")} -> true
      | _ -> false) location (fun () ->
    pop location mode)))

  and close_current_p_element l mode =
    if element_in_button_scope "p" then
      close_element_with_implied "p" l mode
    else mode ()

  and close_preceding_tag names l mode =
    let rec scan = function
      | [] -> mode ()
      | {element_name = (ns, name) as name'}::more ->
        if ns = `HTML && List.mem name names then
          close_element_with_implied name l mode
        else
          if List.mem name' special_category &&
            not @@ List.mem name'
              [`HTML, "address"; `HTML, "div"; `HTML, "p"] then
            mode ()
          else
            scan more
    in
    scan !open_elements

  and emit_end l =
    pop_until (fun _ -> false) l (fun () ->
    emit_text (fun () ->
    !ended ()))

  and reconstruct_active_formatting_elements mode =
    let rec get_prefix prefix = function
      | [] -> prefix, []
      | Marker::_ as l -> prefix, l
      | Element ({is_open = true}, _, _)::_ as l -> prefix, l
      | Element ({is_open = false}, l, tag)::more ->
        get_prefix ((l, tag)::prefix) more
    in
    let to_reopen, remainder = get_prefix [] !active_formatting_elements in
    active_formatting_elements := remainder;

    let rec reopen = function
      | [] -> mode ()
      | (l, tag)::more ->
        push_and_emit l tag (fun () -> reopen more)
    in
    reopen to_reopen

  (* 8.2.5. *)
  and dispatch tokens rules =
    next_expected tokens !throw begin fun ((_, t) as v) ->
      let foreign =
        match adjusted_current_element (), t with
        | None, _ -> false
        | Some {element_name = `HTML, _}, _ -> false
        | Some {element_name}, `Start {name}
            when List.mem element_name mathml_text_integration_points
            && name <> "mglyph" && name <> "malignmark" -> false
        | Some {element_name = `MathML, "annotation-xml"},
            `Start {name = "svg"} -> false
        | Some {is_html_integration_point = true}, `Start _ -> false
        | Some {is_html_integration_point = true}, `Char _ -> false
        | _, `EOF -> false
        | _ -> true
      in

      if not foreign then rules v
      else foreign_content !current_mode (fun () -> rules v) v
    end

  (* 8.2.5.4.1. *)
  and initial_mode () =
    dispatch tokens begin function
      | _, `Char (0x0009 | 0x000A | 0x000C | 0x000D | 0x0020) ->
        initial_mode ()

      | l, `Comment s ->
        emit l (`Comment s) initial_mode

      | l, `Doctype d ->
        emit l (`Doctype d) before_html_mode

      | v ->
        push tokens v;
        before_html_mode ()
    end

  (* 8.2.5.4.2. *)
  and before_html_mode () =
    dispatch tokens begin function
      | l, `Doctype _ ->
        report l (`Bad_document "doctype should be first") !throw
          before_html_mode

      | l, `Comment s ->
        emit l (`Comment s) before_html_mode

      | _, `Char (0x0009 | 0x000A | 0x000C | 0x000D | 0x0020) ->
        before_html_mode ()

      | l, `Start ({name = "html"} as t) ->
        push_and_emit l t before_head_mode

      | l, `End {name}
          when not @@ List.mem name ["head"; "body"; "html"; "br"] ->
        unmatched_end_tag l name before_html_mode

      | l, _ as v ->
        push tokens v;
        push_implicit l "html" before_head_mode
    end

  (* 8.2.5.4.3. *)
  and before_head_mode () =
    dispatch tokens begin function
      | _, `Char (0x0009 | 0x000A | 0x000C | 0x000D | 0x0020) ->
        before_head_mode ()

      | l, `Comment s ->
        emit l (`Comment s) before_head_mode

      | l, `Doctype _ ->
        report l (`Bad_document "doctype should be first") !throw
          before_head_mode

      | _, `Start {name = "html"} as v ->
        in_body_mode_rules "html" before_head_mode v

      | l, `Start ({name = "head"} as t) ->
        push_and_emit l t in_head_mode

      | l, `End {name}
          when not @@ List.mem name ["head"; "body"; "html"; "br"] ->
        report l (`Unmatched_end_tag name) !throw before_head_mode

      | l, _ as v ->
        push tokens v;
        push_implicit l "head" in_head_mode
    end

  (* 8.2.5.4.4. *)
  and in_head_mode () =
    dispatch tokens (fun v -> in_head_mode_rules in_head_mode v)

  (* 8.2.5.4.4. *)
  and in_head_mode_rules mode = function
    | l, `Char (0x0009 | 0x000A | 0x000C | 0x000D | 0x0020 as c) ->
      add_character l c;
      mode ()

    | l, `Comment s ->
      emit l (`Comment s) mode

    | l, `Doctype _ ->
      report l (`Bad_document "doctype should be first") !throw mode

    | _, `Start {name = "html"} as v ->
      in_body_mode_rules "head" in_head_mode v

    | l, `Start ({name =
        "base" | "basefont" | "bgsound" | "link" | "meta"} as t) ->
      push_and_emit ~acknowledge:true l t (fun () ->
      pop l mode)

    | l, `Start ({name = "title"} as t) ->
      push_and_emit l t (fun () ->
      parse_rcdata mode)

    | l, `Start ({name = "noframes" | "style"} as t) ->
      push_and_emit l t (fun () ->
      parse_rawtext mode)

    | l, `Start ({name = "noscript"} as t) ->
      push_and_emit l t in_head_noscript_mode

    | l, `Start ({name = "script"} as t) ->
      push_and_emit l t (fun () ->
      set_tokenizer_state `Script_data;
      text_mode mode)

    | l, `End {name = "head"} ->
      pop l after_head_mode

    | l, `Start ({name = "template"} as t) ->
      add_formatting_marker ();
      frameset_ok := false;
      push_template_insertion_mode in_template_mode;
      push_and_emit l t in_template_mode

    | l, `End {name = "template"} ->
      if not @@ stack_has "template" then
        report l (`Unmatched_end_tag "template") !throw mode
      else begin
        clear_formatting_elements_until_marker ();
        pop_template_insertion_mode ();
        close_element_with_implied "template" l (reset_mode ())
      end

    | l, `Start {name = "head"} ->
      report l (`Misnested_tag ("head", "head")) !throw mode

    | l, `End {name} when not @@ List.mem name ["body"; "html"; "br"] ->
      report l (`Unmatched_end_tag name) !throw mode

    | l, _ as v ->
      push tokens v;
      pop l after_head_mode

  (* 8.2.5.4.5. *)
  and in_head_noscript_mode () =
    dispatch tokens begin function
      | l, `Doctype _ ->
        report l (`Bad_document "doctype should be first") !throw
          in_head_noscript_mode

      | _, `Start {name = "html"} as v ->
        in_body_mode_rules "noscript" in_head_noscript_mode v

      | l, `End {name = "noscript"} ->
        pop l in_head_mode

      | _, `Char (0x0009 | 0x000A | 0x000C | 0x000D | 0x0020)
      | _, `Comment _
      | _, `Start {name =
          "basefont" | "bgsound" | "link" | "meta" | "noframes" |
          "style"} as v ->
        in_head_mode_rules in_head_noscript_mode v

      | l, `Start {name = "head" | "noscript" as name} ->
        report l (`Misnested_tag (name, "noscript")) !throw
          in_head_noscript_mode

      | l, `End {name} when name <> "br" ->
        report l (`Unmatched_end_tag name) !throw in_head_noscript_mode

      | l, _ as v ->
        report l (`Bad_content "noscript") !throw (fun () ->
        push tokens v;
        pop l in_head_mode)
    end

  (* 8.2.5.4.6. *)
  and after_head_mode () =
    dispatch tokens begin function
      | l, `Char (0x0009 | 0x000A | 0x000C | 0x000D | 0x0020 as c) ->
        add_character l c;
        after_head_mode ()

      | l, `Comment s ->
        emit l (`Comment s) after_head_mode

      | l, `Doctype _ ->
        report l (`Bad_document "doctype should be first") !throw
          after_head_mode

      | _, `Start {name = "html"} as v ->
        in_body_mode_rules "html" after_head_mode v

      | l, `Start ({name = "body"} as t) ->
        frameset_ok := false;
        push_and_emit l t in_body_mode

      | l, `Start ({name = "frameset"} as t) ->
        push_and_emit l t in_frameset_mode

      | l, `Start {name =
          "base" | "basefont" | "bgsound" | "link" | "meta" | "noframes" |
          "script" | "style" | "template" | "title" as name} as v ->
        report l (`Misnested_tag (name, "html")) !throw (fun () ->
        in_head_mode_rules after_head_mode v)

      | _, `End {name = "template"} as v ->
        in_head_mode_rules after_head_mode v

      | l, `Start {name = "head"} ->
        report l (`Bad_document "duplicate head element") !throw
          after_head_mode

      | l, `End {name} when not @@ List.mem name ["body"; "html"; "br"] ->
        report l (`Unmatched_end_tag name) !throw after_head_mode

      | l, _ as t ->
        push tokens t;
        push_implicit l "body" in_body_mode
    end

  (* 8.2.5.4.7. *)
  and in_body_mode () =
    dispatch tokens (fun v -> in_body_mode_rules "body" in_body_mode v)

  (* 8.2.5.4.7. *)
  and in_body_mode_rules context_name mode = function
    | l, `Char 0 ->
      report l (`Bad_token ("U+0000", "body", "null")) !throw mode

    | l, `Char (0x0009 | 0x000A | 0x000C | 0x000D | 0x0020 as c) ->
      reconstruct_active_formatting_elements (fun () ->
      add_character l c;
      mode ())

    | l, `Char c ->
      frameset_ok := false;
      reconstruct_active_formatting_elements (fun () ->
      add_character l c;
      mode ())

    | l, `Comment s ->
      emit l (`Comment s) mode

    | l, `Doctype _ ->
      report l (`Bad_document "doctype should be first") !throw mode

    | l, `Start {name = "html"} ->
      report l (`Misnested_tag ("html", context_name)) !throw mode

    | _, `Start {name =
        "base" | "basefont" | "bgsound" | "link" | "meta" | "noframes" |
        "script" | "style" | "template" | "title"}
    | _, `End {name = "template"} as v ->
      in_head_mode_rules mode v

    | l, `Start {name = "body"} ->
      report l (`Misnested_tag ("body", context_name)) !throw mode

    | l, `Start ({name = "frameset"} as t) ->
      report l (`Misnested_tag ("frameset", context_name)) !throw (fun () ->
      match !open_elements with
      | {element_name = `HTML, "body"}::_::_ ->
        if not !frameset_ok then mode ()
        else
          pop_until
            (fun _ -> match !open_elements with [_] -> true | _ -> false)
            l (fun () ->
          push_and_emit l t in_frameset_mode)
      | _ -> mode ())

    | l, `EOF as v ->
      report_if_stack_has_other_than
        ["dd"; "dt"; "li"; "p"; "tbody"; "td"; "tfoot"; "th"; "thead"; "tr";
         "body"; "html"] (fun () ->
      match !template_insertion_modes with
      | [] -> emit_end l
      | _ -> in_template_mode_rules mode v)

    | l, `End {name = "body"} ->
      if not @@ element_in_scope "body" then
        report l (`Unmatched_end_tag "body") !throw mode
      else
        report_if_stack_has_other_than
          ["dd"; "dt"; "li"; "optgroup"; "option"; "p"; "rb"; "rp"; "rt";
           "rtc"; "tbody"; "td"; "tfoot"; "th"; "thead"; "tr"; "body";
           "html"] (fun () ->
        close_element l "body" after_body_mode)

    | l, `End {name = "html"} as v ->
      if not @@ element_in_scope "body" then
        report l (`Unmatched_end_tag "html") !throw mode
      else
        report_if_stack_has_other_than
          ["dd"; "dt"; "li"; "optgroup"; "option"; "p"; "rb"; "rp"; "rt";
           "rtc"; "tbody"; "td"; "tfoot"; "th"; "thead"; "tr"; "body";
           "html"] (fun () ->
        push tokens v;
        close_element l "body" after_body_mode)

    | l, `Start ({name =
        "address" | "article" | "aside" | "blockquote" | "center" |
        "details" | "dialog" | "dir" | "div" | "dl" | "fieldset" |
        "figcaption" | "figure" | "footer" | "header" | "hgroup" | "main" |
        "nav" | "ol" | "p" | "section" | "summary" | "ul"} as t) ->
      close_current_p_element l (fun () ->
      push_and_emit l t mode)

    | l, `Start ({name =
        "h1" | "h2" | "h3" | "h4" | "h5" | "h6" as name} as t) ->
      close_current_p_element l (fun () ->
      (fun mode' ->
        match current_element () with
        | Some {element_name = `HTML,
            ("h1" | "h2" | "h3" | "h4" | "h5" | "h6" as name')} ->
          report l (`Misnested_tag (name, name')) !throw (fun () ->
          pop l mode')
        | _ -> mode' ()) (fun () ->
      push_and_emit l t mode))

    | l, `Start ({name = "pre" | "listing"} as t) ->
      frameset_ok := false;
      close_current_p_element l (fun () ->
      push_and_emit l t (fun () ->
      next_expected tokens !throw (function
        | _, `Char 0x000A -> mode ()
        | v ->
          push tokens v;
          mode ())))

    | l, `Start ({name = "form"} as t) ->
      close_current_p_element l (fun () ->
      push_and_emit l t mode)

    | l, `Start ({name = "li"} as t) ->
      frameset_ok := false;
      close_preceding_tag ["li"] l (fun () ->
      close_current_p_element l (fun () ->
      push_and_emit l t mode))

    | l, `Start ({name = "dd" | "dt"} as t) ->
      frameset_ok := false;
      close_preceding_tag ["dd"; "dt"] l (fun () ->
      close_current_p_element l (fun () ->
      push_and_emit l t mode))

    | l, `Start ({name = "plaintext"} as t) ->
      close_current_p_element l (fun () ->
      set_tokenizer_state `PLAINTEXT;
      push_and_emit l t mode)

    | l, `Start ({name = "button"} as t) ->
      (fun mode' ->
        if element_in_scope "button" then
          report l (`Misnested_tag ("button", "button")) !throw (fun () ->
          close_element_with_implied "button" l mode')
        else mode' ())
      (fun () ->
        frameset_ok := false;
        reconstruct_active_formatting_elements (fun () ->
        push_and_emit l t mode))

    | l, `End {name =
        "address" | "article" | "aside" | "blockquote" | "button" |
        "center" | "details" | "dialog" | "dir" | "div" | "dl" | "fieldset" |
        "figcaption" | "figure" | "footer" | "header" | "hgroup" | "listing" |
        "main" | "nav" | "ol" | "pre" | "section" | "summary" | "ul"
        as name} ->
      if not @@ element_in_scope name then
        report l (`Unmatched_end_tag name) !throw mode
      else
        close_element_with_implied name l mode

    | l, `End {name = "form"} ->
      close_element_with_implied "form" l mode

    | l, `End {name = "p"} ->
      (fun mode' ->
        if not @@ element_in_button_scope "p" then
          report l (`Unmatched_end_tag "p") !throw (fun () ->
          push_implicit l "p" mode')
        else mode' ())
      (fun () -> close_element_with_implied "p" l mode)

    | l, `End {name = "li"} ->
      if not @@ element_in_list_item_scope "li" then
        report l (`Unmatched_end_tag "li") !throw mode
      else
        close_element_with_implied "li" l mode

    | l, `End {name = "dd" | "dt" as name} ->
      if not @@ element_in_scope name then
        report l (`Unmatched_end_tag name) !throw mode
      else
        close_element_with_implied name l mode

    | l, `End {name = "h1" | "h2" | "h3" | "h4" | "h5" | "h6" as name} ->
      if not @@
          one_of_elements_in_scope ["h1"; "h2"; "h3"; "h4"; "h5"; "h6"] then
        report l (`Unmatched_end_tag name) !throw mode
      else
        pop_implied l (fun () ->
          (fun next ->
            match current_element () with
            | Some {element_name = `HTML, name'}
                when List.mem name' ["h1"; "h2"; "h3"; "h4"; "h5"; "h6"] ->
              next ()
            | _ ->
              report l (`Unmatched_end_tag name) !throw next)
          @@ (fun () ->
            pop_until_and_raise_errors
              ["h1"; "h2"; "h3"; "h4"; "h5"; "h6"] l mode))

    | l, `Start ({name = "a"} as t) ->
      reconstruct_active_formatting_elements (fun () ->
      push_and_emit ~formatting:true l t mode)

    | l, `Start ({name =
        "b" | "big" | "code" | "em" | "font" | "i" | "s" | "small" |
        "strike" | "strong" | "tt" | "u"} as t) ->
      reconstruct_active_formatting_elements (fun () ->
      push_and_emit ~formatting:true l t mode)

    | l, `Start ({name = "nobr"} as t) ->
      reconstruct_active_formatting_elements (fun () ->
      push_and_emit ~formatting:true l t mode)

    | l, `End {name =
        "a" | "b" | "big" | "code" | "em" | "font" | "i" | "nobr" | "s" |
        "small" | "strike" | "strong" | "tt" | "u" as name} ->
      close_element_with_implied name l mode

    | l, `Start ({name = "applet" | "marquee" | "object"} as t) ->
      frameset_ok := false;
      reconstruct_active_formatting_elements (fun () ->
      add_formatting_marker ();
      push_and_emit l t mode)

    | l, `End {name = "applet" | "marquee" | "object" as name} ->
      if not @@ element_in_scope name then
        report l (`Unmatched_end_tag name) !throw mode
      else begin
        clear_formatting_elements_until_marker ();
        close_element_with_implied name l mode
      end

    | l, `Start ({name = "table"} as t) ->
      frameset_ok := false;
      close_current_p_element l (fun () ->
      push_and_emit l t in_table_mode)

    | l, `End {name = "br"} ->
      report l (`Unmatched_end_tag "br") !throw (fun () ->
      in_body_mode_rules context_name mode
        (l, `Start {name = "br"; attributes = []; self_closing = false}))

    | l, `Start ({name =
        "area" | "br" | "embed" | "img" | "keygen" | "wbr"} as t) ->
      frameset_ok := false;
      reconstruct_active_formatting_elements (fun () ->
      push_and_emit ~acknowledge:true l t (fun () ->
      pop l mode))

    | l, `Start ({name = "input"} as t) ->
      if is_not_hidden t then frameset_ok := false;
      reconstruct_active_formatting_elements (fun () ->
      push_and_emit ~acknowledge:true l t (fun () ->
      pop l mode))

    | l, `Start ({name = "param" | "source" | "track"} as t) ->
      push_and_emit ~acknowledge:true l t (fun () ->
      pop l mode)

    | l, `Start ({name = "hr"} as t) ->
      frameset_ok := false;
      close_current_p_element l (fun () ->
      push_and_emit ~acknowledge:true l t (fun () ->
      pop l mode))

    | l, `Start ({name = "image"} as t) ->
      report l (`Bad_token ("image", "tag", "should be 'img'")) !throw
        (fun () ->
      push tokens (l, `Start {t with name = "img"});
      mode ())

    | l, `Start ({name = "textarea"} as t) ->
      frameset_ok := false;
      push_and_emit l t (fun () ->
      next_expected tokens !throw (function
        | _, `Char 0x000A -> parse_rcdata mode
        | v ->
          push tokens v;
          parse_rcdata mode))

    | l, `Start {name = "xmp"} ->
      frameset_ok := false;
      close_current_p_element l (fun () ->
      reconstruct_active_formatting_elements (fun () ->
      parse_rawtext mode))

    | _, `Start {name = "iframe"} ->
      frameset_ok := false;
      parse_rawtext mode

    | _, `Start {name = "noembed"} ->
      parse_rawtext mode

    | l, `Start ({name = "select"} as t) ->
      frameset_ok := false;
      select_in_body l t in_select_mode

    | l, `Start ({name = "optgroup" | "option"} as t) ->
      (fun mode' ->
        if current_element_is ["option"] then
          pop l mode'
        else mode' ())
      (fun () ->
        reconstruct_active_formatting_elements (fun () ->
        push_and_emit l t mode))

    | l, `Start ({name = "rb" | "rp" | "rtc" as name} as t) ->
      (fun mode' ->
        if element_in_scope "ruby" then
          pop_implied l (fun () ->
          if current_element_is ["ruby"] then
            mode' ()
          else
            report l (`Misnested_tag (name, context_name)) !throw mode'))
      (fun () ->
        push_and_emit l t mode)

    | l, `Start ({name = "rt"} as t) ->
      (fun mode' ->
        if element_in_scope "ruby" then
          pop_implied ~except:"rtc" l (fun () ->
          if current_element_is ["ruby"; "rtc"] then
            mode' ()
          else
            report l (`Misnested_tag ("rt", context_name)) !throw mode'))
      (fun () ->
        push_and_emit l t mode)

    | l, `Start ({name = "math"} as t) ->
      reconstruct_active_formatting_elements (fun () ->
      push_and_emit ~acknowledge:true ~namespace:`MathML l t (fun () ->
      if t.self_closing then pop l mode
      else mode ()))

    | l, `Start ({name = "svg"} as t) ->
      reconstruct_active_formatting_elements (fun () ->
      push_and_emit ~acknowledge:true ~namespace:`SVG l t (fun () ->
      if t.self_closing then pop l mode
      else mode ()))

    | l, `Start {name =
        "caption" | "col" | "colgroup" | "frame" | "head" | "tbody" | "td" |
        "tfoot" | "th" | "thead" | "tr" as name} ->
      report l (`Misnested_tag (name, context_name)) !throw mode

    | l, `Start t ->
      reconstruct_active_formatting_elements (fun () ->
      push_and_emit l t mode)

    | l, `End {name} ->
      let rec close () =
        match current_element () with
        | None -> mode ()
        | Some {element_name = (ns, name') as name''} ->
          if ns = `HTML && name' = name then
            pop_implied ~except:name l mode
          else
            if List.mem name'' special_category then
              report l (`Unmatched_end_tag name) !throw mode
            else
              pop l close
      in
      close ()

  (* Part of 8.2.5.4.7. *)
  and select_in_body l t next_mode =
    frameset_ok := false;
    reconstruct_active_formatting_elements (fun () ->
    push_and_emit l t next_mode)

  (* 8.2.5.4.8. *)
  and text_mode original_mode =
    dispatch tokens begin function
      | l, `Char c ->
        add_character l c;
        text_mode original_mode

      | l, `EOF as v ->
        report l (`Unexpected_eoi "content") !throw (fun () ->
        push tokens v;
        pop l original_mode)

      | l, `End _ ->
        pop l original_mode

      | _ ->
        text_mode original_mode
    end

  (* 8.2.5.2. *)
  and parse_rcdata original_mode =
    set_tokenizer_state `RCDATA;
    text_mode original_mode

  (* 8.2.5.2. *)
  and parse_rawtext original_mode =
    set_tokenizer_state `RAWTEXT;
    text_mode original_mode

  and anything_else_in_table mode (l, _ as v) =
    report l (`Bad_content "table") !throw (fun () ->
    in_body_mode_rules "table" mode v)

  (* 8.2.5.4.9. *)
  and in_table_mode () =
    dispatch tokens (fun v -> in_table_mode_rules in_table_mode v)

  and in_table_mode_rules mode = function
    | _, `Char _ as v
        when current_element_is ["table"; "tbody"; "tfoot"; "thead"; "tr"] ->
      push tokens v;
      in_table_text_mode true [] mode

    | l, `Comment s ->
      emit l (`Comment s) mode

    | l, `Doctype _ ->
      report l (`Bad_document "doctype should be first") !throw mode

    | l, `Start ({name = "caption"} as t) ->
      pop_to_table_context l (fun () ->
      add_formatting_marker ();
      push_and_emit l t in_caption_mode)

    | l, `Start ({name = "colgroup"} as t) ->
      pop_to_table_context l (fun () ->
      push_and_emit l t in_column_group_mode)

    | l, `Start {name = "col"} as v ->
      pop_to_table_context l (fun () ->
      push tokens v;
      push_implicit l "colgroup" in_column_group_mode)

    | l, `Start ({name = "tbody" | "tfoot" | "thead"} as t) ->
      pop_to_table_context l (fun () ->
      push_and_emit l t in_table_body_mode)

    | l, `Start {name = "td" | "th" | "tr"} as v ->
      pop_to_table_context l (fun () ->
      push tokens v;
      push_implicit l "tbody" in_table_body_mode)

    | l, `Start {name = "table"} as v ->
      report l (`Misnested_tag ("table", "table")) !throw (fun () ->
      if not @@ stack_has "table" then mode ()
      else begin
        push tokens v;
        close_element l "table" (reset_mode ())
      end)

    | l, `End {name = "table"} ->
      if not @@ element_in_table_scope "table" then
        report l (`Unmatched_end_tag "table") !throw mode
      else
        close_element l "table" (reset_mode ())

    | l, `End {name =
      "body" | "caption" | "col" | "colgroup" | "html" | "tbody" | "td" |
      "tfoot" | "th" | "thead" | "tr" as name} ->
      report l (`Unmatched_end_tag name) !throw mode

    | _, `Start {name = "style" | "script" | "template"}
    | _, `End {name = "template"} as v ->
      in_head_mode_rules mode v

    | l, `Start ({name = "input"} as t) when is_not_hidden t ->
      report l (`Misnested_tag ("input", "table")) !throw (fun () ->
      push_and_emit ~acknowledge:true l t (fun () ->
      pop l mode))

    | l, `Start ({name = "form"} as t) ->
      report l (`Misnested_tag ("form", "table")) !throw (fun () ->
      push_and_emit l t (fun () ->
      pop l mode))

    | _, `EOF as v ->
      in_body_mode_rules "table" mode v

    | v ->
      anything_else_in_table mode v

  (* 8.2.5.4.10. *)
  and in_table_text_mode only_space cs mode =
    dispatch tokens begin function
      | l, `Char 0 ->
        report l (`Bad_token ("U+0000", "table", "null")) !throw (fun () ->
        in_table_text_mode only_space cs mode)

      | _, `Char (0x0009 | 0x000A | 0x000C | 0x000D | 0x0020) as v ->
        in_table_text_mode only_space (v::cs) mode

      | _, `Char _ as v ->
        in_table_text_mode false (v::cs) mode

      | v ->
        push tokens v;
        if not only_space then
          let rec reprocess = function
            | [] -> mode ()
            | v::more -> anything_else_in_table (fun () -> reprocess more) v
          in
          reprocess (List.rev cs)
        else begin
          List.rev cs |> List.iter (function
            | l, `Char c -> add_character l c
            | _ -> ());
          mode ()
        end
    end

  (* 8.2.5.4.11. *)
  and in_caption_mode () =
    dispatch tokens begin function
      | l, `End {name = "caption"} ->
        if not @@ element_in_table_scope "caption" then
          report l (`Unmatched_end_tag "caption") !throw in_caption_mode
        else begin
          clear_formatting_elements_until_marker ();
          close_element_with_implied "caption" l in_table_mode
        end

      | l, `Start {name =
          "caption" | "col" | "colgroup" | "tbody" | "td" | "tfoot" | "th" |
          "thead" | "tr" as name} as v ->
        report l (`Misnested_tag (name, "caption")) !throw (fun () ->
        if not @@ element_in_table_scope "caption" then in_caption_mode ()
        else begin
          clear_formatting_elements_until_marker ();
          push tokens v;
          close_element l "caption" in_table_mode
        end)

      | l, `End {name = "table"} as v ->
        report l (`Unmatched_end_tag "table") !throw (fun () ->
        if not @@ element_in_table_scope "caption" then in_caption_mode ()
        else begin
          clear_formatting_elements_until_marker ();
          push tokens v;
          close_element l "caption" in_table_mode
        end)

      | l, `End {name =
          ("body" | "col" | "colgroup" | "html" | "tbody" | "td" | "tfoot" |
           "th" | "thead" | "tr") as name} ->
        report l (`Unmatched_end_tag name) !throw in_caption_mode

      | l, `Start ({name = "select"} as t) ->
        select_in_body l t in_select_in_table_mode

      | v ->
        in_body_mode_rules "caption" in_caption_mode v
    end

  (* 8.2.5.4.12. *)
  and in_column_group_mode () =
    dispatch tokens begin function
      | l, `Char (0x0009 | 0x000A | 0x000C | 0x000D | 0x0020 as c) ->
        add_character l c;
        in_column_group_mode ()

      | l, `Comment s ->
        emit l (`Comment s) in_column_group_mode

      | l, `Doctype _ ->
        report l (`Bad_document "doctype should be first") !throw
          in_column_group_mode

      | _, `Start {name = "html"} as v ->
        in_body_mode_rules "colgroup" in_column_group_mode v

      | l, `Start ({name = "col"} as t) ->
        push_and_emit ~acknowledge:true l t (fun () ->
        pop l in_column_group_mode)

      | l, `End {name = "colgroup"} ->
        if not @@ current_element_is ["colgroup"] then
          report l (`Unmatched_end_tag "colgroup") !throw in_column_group_mode
        else
          pop l in_table_mode

      | l, `End {name = "col"} ->
        report l (`Unmatched_end_tag "col") !throw in_column_group_mode

      | _, `Start {name = "template"}
      | _, `End {name = "template"} as v ->
        in_head_mode_rules in_column_group_mode v

      | _, `EOF as v ->
        in_body_mode_rules "colgroup" in_column_group_mode v

      | l, _ as v ->
        if not @@ current_element_is ["colgroup"] then
          report l (`Bad_content "colgroup") !throw in_table_mode
        else begin
          push tokens v;
          pop l in_table_mode
        end
    end

  (* 8.2.5.4.13. *)
  and in_table_body_mode () =
    dispatch tokens begin function
      | l, `Start ({name = "tr"} as t) ->
        pop_to_table_body_context l (fun () ->
        push_and_emit l t in_row_mode)

      | l, `Start {name = ("th" | "td") as name} as v ->
        report l (`Misnested_tag (name, "table")) !throw (fun () ->
        pop_to_table_body_context l (fun () ->
        push tokens v;
        push_implicit l "tr" in_row_mode))

      | l, `End {name = "tbody" | "tfoot" | "thead" as name} ->
        if not @@ element_in_table_scope name then
          report l (`Unmatched_end_tag name) !throw in_table_body_mode
        else
          pop_to_table_body_context l (fun () ->
          pop l in_table_mode)

      | l, `Start {name =
          "caption" | "col" | "colgroup" | "tbody" | "tfoot" | "thead"
          as name} as v ->
        if not @@ one_of_elements_in_table_scope
            ["tbody"; "thead"; "tfoot"] then
          report l (`Misnested_tag (name, "table")) !throw in_table_body_mode
        else begin
          push tokens v;
          pop_to_table_body_context l (fun () ->
          pop l in_table_mode)
        end

      | l, `End {name = "table" as name} as v ->
        if not @@ one_of_elements_in_table_scope
            ["tbody"; "thead"; "tfoot"] then
          report l (`Unmatched_end_tag name) !throw in_table_body_mode
        else begin
          push tokens v;
          pop_to_table_body_context l (fun () ->
          pop l in_table_mode)
        end

      | l, `End {name =
          "body" | "caption" | "col" | "colgroup" | "html" | "td" | "th" |
          "tr" as name} ->
        report l (`Unmatched_end_tag name) !throw in_table_body_mode

      | v ->
        in_table_mode_rules in_table_body_mode v
    end

  (* 8.2.5.4.14. *)
  and in_row_mode () =
    dispatch tokens begin function
      | l, `Start ({name = "th" | "td"} as t) ->
        add_formatting_marker ();
        pop_to_table_row_context l (fun () ->
        push_and_emit l t in_cell_mode)

      | l, `End {name = "tr"} ->
        if not @@ element_in_table_scope "tr" then
          report l (`Unmatched_end_tag "tr") !throw in_row_mode
        else
          pop_to_table_row_context l (fun () ->
          pop l in_table_body_mode)

      | l, `Start {name =
          ("caption" | "col" | "colgroup" | "tbody" | "tfoot" | "thead" |
           "tr")}
      | l, `End {name = "table"} as v ->
        if not @@ element_in_table_scope "tr" then
          match snd v with
          | `Start {name} ->
            report l (`Misnested_tag (name, "tr")) !throw in_row_mode
          | `End {name} ->
            report l (`Unmatched_end_tag name) !throw in_row_mode
        else
          pop_to_table_row_context l (fun () ->
          push tokens v;
          pop l in_table_body_mode)

      | l, `End {name = "tbody" | "tfoot" | "thead" as name} as v ->
        if not @@ element_in_table_scope name then
          report l (`Unmatched_end_tag name) !throw in_row_mode
        else
          if not @@ element_in_table_scope "tr" then in_row_mode ()
          else
            pop_to_table_row_context l (fun () ->
            push tokens v;
            pop l in_table_body_mode)

      | l, `End {name =
          "body" | "caption" | "col" | "colgroup" | "html" | "td" | "th"
          as name} ->
        report l (`Unmatched_end_tag name) !throw in_row_mode

      | v ->
        in_table_mode_rules in_row_mode v
    end

  (* 8.2.5.4.15. *)
  and in_cell_mode () =
    dispatch tokens begin function
      | l, `End {name = "td" | "th" as name} ->
        if not @@ element_in_table_scope name then
          report l (`Unmatched_end_tag name) !throw in_cell_mode
        else
          close_element_with_implied name l (fun () ->
          clear_formatting_elements_until_marker ();
          in_row_mode ())

      | l, `Start {name =
          "caption" | "col" | "colgroup" | "tbody" | "td" | "tfoot" | "th" |
          "thead" | "tr" as name} as v ->
        if not @@ one_of_elements_in_table_scope ["td"; "th"] then
          report l (`Misnested_tag (name, "td/th")) !throw in_cell_mode
        else
          close_cell l (fun () ->
          clear_formatting_elements_until_marker ();
          push tokens v;
          in_row_mode ())

      | l, `End {name =
          "body" | "caption" | "col" | "colgroup" | "html" as name} ->
        report l (`Unmatched_end_tag name) !throw in_cell_mode

      | l, `End {name =
          "table" | "tbody" | "tfoot" | "thead" | "tr" as name} as v ->
        if not @@ element_in_table_scope name then
          report l (`Unmatched_end_tag name) !throw in_cell_mode
        else
          close_cell l (fun () ->
          clear_formatting_elements_until_marker ();
          push tokens v;
          in_row_mode ())

      | l, `Start ({name = "select"} as t) ->
        select_in_body l t in_select_in_table_mode

      | v ->
        in_body_mode_rules "td" in_cell_mode v
    end

  (* 8.2.5.4.16. *)
  and in_select_mode () =
    dispatch tokens (fun v -> in_select_mode_rules in_select_mode v)

  and in_select_mode_rules mode = function
    | l, `Char 0 ->
      report l (`Bad_token ("U+0000", "select", "null")) !throw mode

    | l, `Char c ->
      add_character l c;
      mode ()

    | l, `Comment s ->
      emit l (`Comment s) mode

    | l, `Doctype _ ->
      report l (`Bad_document "doctype should be first") !throw mode

    | _, `Start {name = "html"} as v ->
      in_body_mode_rules "select" mode v

    | l, `Start ({name = "option"} as t) ->
      (fun mode' ->
        if current_element_is ["option"] then pop l mode'
        else mode' ())
      (fun () -> push_and_emit l t mode)

    | l, `Start ({name = "optgroup"} as t) ->
      (fun mode' ->
        if current_element_is ["option"] then pop l mode'
        else mode' ())
      @@ (fun mode' () ->
        if current_element_is ["optgroup"] then pop l mode'
        else mode' ())
      @@ (fun () -> push_and_emit l t mode)

    | l, `End {name = "optgroup"} ->
      (fun mode' ->
        match !open_elements with
        | {element_name = `HTML, "option"}::
            {element_name = `HTML, "optgroup"}::_ ->
          pop l mode'
        | _ -> mode' ())
      (fun () ->
        if current_element_is ["optgroup"] then
          pop l mode
        else
          report l (`Unmatched_end_tag "optgroup") !throw mode)

    | l, `End {name = "option"} ->
      if current_element_is ["option"] then
        pop l mode
      else
        report l (`Unmatched_end_tag "option") !throw mode

    | l, `End {name = "select"} ->
      if not @@ element_in_select_scope "select" then
        report l (`Unmatched_end_tag "select") !throw mode
      else
        close_element l "select" (reset_mode ())

    | l, `Start {name = "select"} ->
      report l (`Misnested_tag ("select", "select")) !throw (fun () ->
      close_element l "select" (reset_mode ()))

    | l, `Start {name = "input" | "keygen" | "textarea" as name} as v ->
      report l (`Misnested_tag (name, "select")) !throw (fun () ->
      if not @@ element_in_select_scope "select" then
        mode ()
      else begin
        push tokens v;
        close_element l "select" (reset_mode ())
      end)

    | _, (`Start {name = "script" | "template"} |
          `End {name = "template"}) as v ->
      in_head_mode_rules mode v

    | _, `EOF as v ->
      in_body_mode_rules "select" mode v

    | l, _ ->
      report l (`Bad_content "select") !throw mode

  (* 8.2.5.4.17. *)
  and in_select_in_table_mode () =
    dispatch tokens begin function
      | l, `Start {name =
          "caption" | "table" | "tbody" | "tfoot" | "thead" | "tr" | "td" |
          "th" as name} as v ->
        report l (`Misnested_tag (name, "table")) !throw (fun () ->
        push tokens v;
        close_element l "select" (reset_mode ()))

      | l, `End {name =
          "caption" | "table" | "tbody" | "tfoot" | "thead" | "tr" | "td" |
          "th" as name} as v ->
        report l (`Unmatched_end_tag "name") !throw (fun () ->
        if not @@ element_in_table_scope name then in_select_in_table_mode ()
        else begin
          push tokens v;
          close_element l "select" (reset_mode ())
        end)

      | v ->
        in_select_mode_rules in_select_in_table_mode v
    end

  (* 8.2.5.4.18. *)
  and in_template_mode () =
    dispatch tokens (fun v -> in_table_mode_rules in_template_mode v)

  (* 8.2.5.4.18. *)
  and in_template_mode_rules mode = function
    | _, (`Char _ | `Comment _ | `Doctype _) as v ->
      in_body_mode_rules "template" mode v

    | _, `Start {name =
        "base" | "basefont" | "bgsound" | "link" | "meta" | "noframes" |
        "script" | "style" | "template" | "title"}
    | _, `End {name = "template"} as v ->
      in_head_mode_rules mode v

    | _, `Start {name =
        "caption" | "colgroup" | "tbody" | "tfoot" | "thead"} as v ->
      pop_template_insertion_mode ();
      push_template_insertion_mode in_table_mode;
      push tokens v;
      in_table_mode ()

    | _, `Start {name = "col"} as v ->
      pop_template_insertion_mode ();
      push_template_insertion_mode in_column_group_mode;
      push tokens v;
      in_column_group_mode ()

    | _, `Start {name = "tr"} as v ->
      pop_template_insertion_mode ();
      push_template_insertion_mode in_table_body_mode;
      push tokens v;
      in_table_body_mode ()

    | _, `Start {name = "td" | "th"} as v ->
      pop_template_insertion_mode ();
      push_template_insertion_mode in_row_mode;
      push tokens v;
      in_row_mode ()

    | _, `Start _ as v ->
      pop_template_insertion_mode ();
      push_template_insertion_mode in_body_mode;
      push tokens v;
      in_body_mode ()

    | l, `End {name} ->
      report l (`Unmatched_end_tag name) !throw mode

    | l, `EOF as v ->
      if not @@ stack_has "template" then emit_end l
      else begin
        report l (`Unmatched_end_tag "template") !throw (fun () ->
        clear_formatting_elements_until_marker ();
        pop_template_insertion_mode ();
        push tokens v;
        close_element l "template" (reset_mode ()))
      end

  (* 8.2.5.4.19. *)
  and after_body_mode () =
    dispatch tokens begin function
      | _, `Char (0x0009 | 0x000A | 0x000C | 0x000D | 0x0020) as v ->
        in_body_mode_rules "html" after_body_mode v

      | l, `Comment s ->
        emit l (`Comment s) after_body_mode

      | l, `Doctype _ ->
        report l (`Bad_document "doctype should be first") !throw
          after_body_mode

      | _, `Start {name = "html"} as v ->
        in_body_mode_rules "html" after_body_mode v

      | l, `End {name = "html"} ->
        close_element l "html" after_after_body_mode

      | l, `EOF ->
        emit_end l

      | l, _ as v ->
        report l (`Bad_document "content after body") !throw (fun () ->
        push tokens v;
        in_body_mode ())
    end

  (* 8.2.5.4.20. *)
  and in_frameset_mode () =
    dispatch tokens begin function
      | l, `Char (0x0009 | 0x000A | 0x000C | 0x000D | 0x0020 as c) ->
        add_character l c;
        in_frameset_mode ()

      | l, `Comment s ->
        emit l (`Comment s) in_frameset_mode

      | l, `Doctype _ ->
        report l (`Bad_document "doctype should be first") !throw
          in_frameset_mode

      | _, `Start {name = "html"} as v ->
        in_body_mode_rules "frameset" in_frameset_mode v

      | l, `Start ({name = "frameset"} as t) ->
        push_and_emit l t in_frameset_mode

      | l, `End {name = "frameset"} ->
        (fun mode' ->
          if current_element_is ["html"] then
            report l (`Unmatched_end_tag "frameset") !throw mode'
          else
            pop l mode')
        (fun () ->
          if current_element_is ["frameset"] then in_frameset_mode ()
          else after_frameset_mode ())

      | l, `Start ({name = "frame"} as t) ->
        push_and_emit ~acknowledge:true l t (fun () ->
        pop l in_frameset_mode)

      | _, `Start {name = "noframes"} as v ->
        in_head_mode_rules in_frameset_mode v

      | l, `EOF ->
        (fun mode' ->
          if current_element_is ["html"] then
            report l (`Unexpected_eoi "frameset") !throw mode'
          else mode' ())
        (fun () -> emit_end l)

      | l, _ ->
        report l (`Bad_content "frameset") !throw in_frameset_mode
    end

  (* 8.2.5.4.21. *)
  and after_frameset_mode () =
    dispatch tokens begin function
      | l, `Char (0x0009 | 0x000A | 0x000C | 0x000D | 0x0020 as c) ->
        add_character l c;
        after_frameset_mode ()

      | l, `Comment s ->
        emit l (`Comment s) after_frameset_mode

      | l, `Doctype _ ->
        report l (`Bad_document "doctype should be first") !throw
          after_frameset_mode

      | _, `Start {name = "html"} as v ->
        in_body_mode_rules "html" after_frameset_mode v

      | l, `End {name = "html"} ->
        close_element l "html" after_after_frameset_mode

      | _, `Start {name = "noframes"} as v ->
        in_head_mode_rules after_frameset_mode v

      | l, `EOF ->
        emit_end l

      | l, _ ->
        report l (`Bad_content "html") !throw after_frameset_mode
    end

  (* 8.2.5.4.22. *)
  and after_after_body_mode () =
    dispatch tokens begin function
      | l, `Comment s ->
        emit l (`Comment s) after_after_body_mode

      | _, `Doctype _
      | _, `Char (0x0009 | 0x000A | 0x000C | 0x000D | 0x0020)
      | _, `Start {name = "html"} as v ->
        in_body_mode_rules "html" after_after_body_mode v

      | l, `EOF ->
        emit_end l

      | l, _ ->
        report l (`Bad_content "html") !throw after_after_body_mode
    end

  (* 8.2.5.4.23. *)
  and after_after_frameset_mode () =
    dispatch tokens begin function
      | l, `Comment s ->
        emit l (`Comment s) after_after_frameset_mode

      | _, `Doctype _
      | _, `Char (0x0009 | 0x000A | 0x000C | 0x000D | 0x0020)
      | _, `Start {name = "html"} as v ->
        in_body_mode_rules "html" after_after_frameset_mode v

      | l, `EOF ->
        emit_end l

      | _, `Start {name = "noframes"} as v ->
        in_head_mode_rules after_after_frameset_mode v

      | l, _ ->
        report l (`Bad_content "html") !throw after_after_frameset_mode
    end

  (* 8.2.5.5. *)
  and foreign_start_tag mode l tag =
    let namespace =
      match adjusted_current_element () with
      | None -> `HTML
      | Some {element_name = ns, _} -> ns
    in

    push_and_emit ~acknowledge:true ~namespace l tag (fun () ->
    if tag.self_closing then pop l mode
    else mode ())

  and is_html_font_tag tag =
    tag.attributes |> List.exists (function
      | ("color" | "face" | "size"), _ -> true
      | _ -> false)

  and foreign_content mode force_html v =
    match v with
    | l, `Char 0 ->
      report l (`Bad_token ("U+0000", "foreign content", "null")) !throw
        (fun () ->
      add_character l Uutf.u_rep;
      mode ())

    | l, `Char (0x0009 | 0x000A | 0x000C | 0x000D | 0x0020 as c) ->
      add_character l c;
      mode ()

    | l, `Char c ->
      frameset_ok := false;
      add_character l c;
      mode ()

    | l, `Comment s ->
      emit l (`Comment s) mode

    | l, `Doctype _ ->
      report l (`Bad_document "doctype should be first") !throw mode

    | l, `Start ({name =
        "b" | "big" | "blockquote" | "body" | "br" | "center" | "code" |
        "dd" | "div" | "dl" | "dt" | "em" | "embed" | "font" | "h1" | "h2" |
        "h3" | "h4" | "h5" | "h6" | "head" | "hr" | "i" | "img" | "li" |
        "listing" | "main" | "meta" | "nobr" | "ol" | "p" | "pre" | "ruby" |
        "s" | "small" | "span" | "strong" | "strike" | "sub" | "sup" |
        "table" | "tt" | "u" | "ul" | "var" as name} as t) as v ->
      if name = "font" && not @@ is_html_font_tag t then
        foreign_start_tag mode l t
      else
        report l (`Misnested_tag (name, "xml tag")) !throw (fun () ->
        push tokens v;
        pop l (fun () ->
        pop_until (function
          | {element_name = `HTML, _} -> true
          | {is_html_integration_point = true} -> true
          | {element_name} ->
            List.mem element_name mathml_text_integration_points)
          l mode))

    | l, `Start t ->
      foreign_start_tag mode l t

    | l, `End {name = "script"}
        when
          match current_element () with
          | Some {element_name = `SVG, "script"} -> true
          | _ -> false ->
      pop l mode

    | l, `End {name} ->
      (fun mode' ->
        match current_element () with
        | Some {element_name = _, name'} when String.lowercase name' = name ->
          mode' ()
        | _ ->
          report l (`Unmatched_end_tag name) !throw (fun () ->
          mode' ()))
      (fun () ->
        let rec scan = function
          | [] -> mode ()
          | {element_name = ns, name'}::_
              when String.lowercase name' = name ->
            close_element ~ns l name mode
          | {element_name = `HTML, _}::_ -> force_html ()
          | _::rest -> scan rest
        in
        scan !open_elements)

    | _, `EOF -> force_html ()

  (* 8.2.3.1. *)
  and reset_mode () =
    let rec iterate last = function
      | [e] when not last && !context <> `Document ->
        begin match !context with
        | `Document -> failwith "impossible"
        | `Fragment name -> iterate true [{e with element_name = name}]
        end
      | {element_name = _, "select"}::ancestors ->
        let rec iterate' = function
          | [] -> in_select_mode
          | {element_name = _, "template"}::_ -> in_select_mode
          | {element_name = _, "table"}::_ -> in_select_in_table_mode
          | _::ancestors -> iterate' ancestors
        in
        iterate' ancestors
      | {element_name = _, ("tr" | "th")}::_::_ -> in_cell_mode
      | {element_name = _, "tr"}::_ -> in_row_mode
      | {element_name = _, ("tbody" | "thead" | "tfoot")}::_ ->
        in_table_body_mode
      | {element_name = _, "caption"}::_ -> in_caption_mode
      | {element_name = _, "colgroup"}::_ -> in_column_group_mode
      | {element_name = _, "table"}::_ -> in_table_mode
      | {element_name = _, "template"}::_ ->
        begin match !template_insertion_modes with
        | [] -> initial_mode (* This is an internal error, actually. *)
        | mode::_ -> mode
        end
      | [{element_name = _, "head"}] -> in_body_mode
      | {element_name = _, "head"}::_::_ -> in_head_mode
      | {element_name = _, "body"}::_ -> in_body_mode
      | {element_name = _, "frameset"}::_ -> in_frameset_mode
      | {element_name = _, "html"}::_ -> after_head_mode
      | _::rest -> iterate last rest
      | [] -> in_body_mode
    in
    iterate false !open_elements

  in

  let constructor throw_ k =
    (fun k ->
      match requested_context with
      | Some c -> k c
      | None -> detect_context tokens throw_ k) (fun detected_context ->

    context :=
      begin match detected_context with
      | `Document -> `Document
      | `Fragment "math" -> `Fragment (`MathML, "math")
      | `Fragment "svg" -> `Fragment (`SVG, "svg")
      | `Fragment name -> `Fragment (`HTML, name)
      end;

    context_element :=
      begin match !context with
      | `Document -> None
      | `Fragment name ->
        let is_html_integration_point =
          match name with
          | `SVG, ("foreignObject" | "desc" | "title") -> true
          | _ -> false
        in

        Some
          {element_name = name;
           location     = 1, 1;
           is_html_integration_point;
           suppress     = true;
           is_open      = true}
      end;

    let initial_tokenizer_state =
      match !context with
      | `Fragment (`HTML, ("title" | "textarea")) -> `RCDATA
      | `Fragment
          (`HTML, ("style" | "xmp" | "iframe" | "noembed" | "noframes")) ->
        `RAWTEXT
      | `Fragment (`HTML, "script") -> `Script_data
      | `Fragment (`HTML, "plaintext") -> `PLAINTEXT
      | _ -> `Data
    in

    set_tokenizer_state initial_tokenizer_state;

    begin match !context with
    | `Document -> ()
    | `Fragment _ ->
      let notional_root =
        {element_name              = `HTML, "html";
         location                  = 1, 1;
         is_html_integration_point = false;
         suppress                  = true;
         is_open                   = true}
      in
      open_elements := [notional_root]
    end;

    begin match !context with
    | `Fragment (`HTML, "template") ->
      push_template_insertion_mode in_template_mode
    | _ -> ()
    end;

    current_mode :=
      begin match !context with
      | `Fragment _ -> reset_mode ()
      | `Document -> initial_mode
      end;

    (fun throw_ e k ->
      throw := throw_;
      ended := e;
      output := k;
      !current_mode ())
    |> make
    |> k)
  in

  construct constructor
