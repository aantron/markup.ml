let () =
  let flags_file = open_out "src/flags.sexp" in

  output_string flags_file "(";

  let major, minor =
    Scanf.sscanf Sys.ocaml_version "%u.%u"
      (fun major minor -> major, minor)
  in

  (* Compilers starting from 4.03.0 support the -O3 flag. *)
  if (major, minor) >= (4, 3) then
    output_string flags_file "-O3";

  output_string flags_file ")";

  close_out flags_file
