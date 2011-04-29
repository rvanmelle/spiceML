open Ocamlbuild_plugin;;
open Command;;
open Printf;;

let read_command_output f s =
  let ic = Unix.open_process_in s in
  (try
     while true do
       f (input_char ic)
     done
   with End_of_file -> ());
  match Unix.close_process_in ic with
      Unix.WEXITED 0 -> ()
    | _ -> invalid_arg ("read_command_output: " ^ s)

let slurp_command s =
  let buf = Buffer.create 256 in
  read_command_output (Buffer.add_char buf) s;
  Buffer.contents buf

let split s i =
  ( String.sub s 0 i,
    String.sub s (i+1) (String.length s - (i+1)) )

let rec split_path p =
  try
    let i = String.index p '/' in
    let e,r = split p i in
      e :: split_path r
  with Not_found -> [p]

let join_path pe =
  (function
       [] -> ""
     | l::r ->
	 List.fold_right
	   Pathname.concat
	   (List.rev r)
	   l
  ) (List.rev pe)
;;

let flatten_path p =
  let rec flatten = function
      "." :: r       -> flatten r
    | _ :: ".." :: r -> flatten r
    | x :: r         -> x :: flatten r
    | []             -> []
  in
    join_path (flatten (split_path p))
;;

let preprocess_rule ext =
  let prod = "%.pp." ^ ext in
  let dep = "%." ^ ext in
  let desc = Printf.sprintf "Generate preprocessed files: %s -> %s" dep prod in
    rule desc ~prod ~dep
      begin fun env build ->
	let src = env dep and tgt = env prod in
	List.iter Outcome.ignore_good (build [["cells/pa_cells.cma"]]);
	Cmd(S([A "camlp4o"; A "cells/pa_cells.cma"; A "-printer";
	       A "Ocaml"; P src; A "-o"; Px tgt]));
	(*Sh ">"; Px tgt]));*)
      end;
;;

let ocamlfind_location package =
  let cmd = "ocamlfind query " ^ package in
  let (ich, _, _) as channels =  Unix.open_process_full cmd (Unix.environment ()) in
    try
      let l = input_line ich in
        ignore (Unix.close_process_full channels);
        l
    with End_of_file ->
      ignore (Unix.close_process_full channels);
      raise Not_found

let get_location package =
    try
      ocamlfind_location package
    with Not_found ->
      ""
let get_root = Sys.getcwd()

let karma_root = try Sys.getenv "KARMA_ROOT"
  with Not_found -> get_root ;;

let try_location ?default ?fallback package =
  try
    ocamlfind_location package
  with Not_found ->
    let local_lib = List.fold_left Filename.concat get_root ["lib"; package] in
    match default with
        None -> local_lib
      | Some n ->
          if Sys.file_exists n then n else match fallback with
              None -> local_lib
            | Some n -> n

let get_pyversion =
  try Sys.getenv "PYTHON_VERSION"
  with Not_found -> "2.5";;

let get_pyroot =
  try Sys.getenv "PYTHON_ROOT"
  with Not_found -> "/usr/lib" ;;

let python_lib = "python" ^ get_pyversion;;

let python_config = get_pyroot ^ "/" ^ python_lib ^ "/config";;

let try_ocamlfind package () =
  try
    Some (ocamlfind_location package)
  with Not_found -> None
;;

let if_exists fn () =
  if Sys.file_exists fn then Some fn else None
;;

let rec first_of (fallback:string) =
  let rec f = function
      [] -> fallback
    | l::ls ->
        match l () with
            Some l -> l
          | None -> f ls
  in f
;;

let mpich_lib_dir = first_of "/usr/local/mpich/lib"
  (List.map if_exists ["/usr/lib/mpich/lib"; "/usr/local/mpich-1.2.7/lib"])

let mpich2_lib_dir = first_of "/usr/local/mpich2/lib"
  (List.map if_exists ["/mars/mpich2/lib"; "/usr/local/mpich2-1.0.6p1/lib"; "/home/al/mars/mpich2-1.0.7/lib"])

let openmpi_lib_dir = first_of "/usr/local/openmpi/lib"
  (List.map if_exists ["/mars/openmpi-1.2.5/lib"; "/usr/local/openmpi-1.2.5/lib"])

let add_lib (fl:string) ?(custom=false) ?(requires=[]) (package:string) ?(fallback=[]) ?(libs=[]) ?(objs=[]) =
  let location =
    first_of ("+"^package) (try_ocamlfind package :: List.map if_exists fallback)
  in
  let location_flags = if "" = location then N else S[A"-I"; A location] in
  let tag_flags = if requires = [] then N else T (Tags.of_list requires) in
  let add_ext e l = A (l -.- e) in
    function
        Before_hygiene ->
          flag [ "ocaml"; "compile"; fl ] tag_flags;
          flag [ "byte"; "ocaml"; "link"; fl ] tag_flags;
	  if custom then
	    flag ["ocaml"; "link"; "byte"; fl] (A"-custom");
          flag [ "native"; "ocaml"; "link"; fl ] tag_flags

      | After_rules ->
          flag [ "ocaml"; "compile"; fl ] location_flags;
          flag [ "ocaml"; "infer_interface"; fl ] location_flags;
          flag [ "ocaml"; "doc"; fl ] location_flags;

          flag [ "byte"; "ocaml"; "link"; fl ]
            (S[location_flags;
               S (List.map (add_ext "cma") libs);
               S (List.map (add_ext "cmo") objs);
              ]);

          flag [ "native"; "ocaml"; "link"; "program"; fl ]
            (S[location_flags;
               S (List.map (add_ext "cmxa") libs);
               S (List.map (add_ext "cmx") objs);
              ]);

          flag [ "native"; "ocaml"; "link"; "library"; fl ]
            (S[location_flags;
               S (List.map (add_ext "cmx") objs);
              ]);
      | _ -> ()
;;

let add_syntax (fl:string) (file:string) =
  function
      After_rules ->
        flag ["ocaml"; "pp"; fl] (P file);
        dep ["ocaml"; "ocamldep"; fl] [file];
    | _ -> ()
;;

let add_external_syntax (fl:string) (file:string) (package:string) ?(includes=[]) =
  let location =
    first_of ("+"^package) (try_ocamlfind package :: List.map if_exists includes)
  in
  let location_flags = 
    S[A"-I"; A location]
  in
  function
      After_rules ->
        flag ["ocaml"; "pp"; fl] (S [location_flags; A file]);
    | _ -> ()
;;

let rec go () =
  let (l:(hook -> unit) list) =
    [
      dtypes;
      add_lib "use_extlib" "extlib" ~libs:["extLib"];
      add_lib "use_sexplib" "sexplib" ~libs:["sexplib"];
      add_external_syntax "use_sexplib" "pa_type_conv.cmo" "type-conv";
      add_external_syntax "use_sexplib" "pa_sexp_conv.cmo" "sexplib";
      add_lib "use_gsl" "gsl" ~custom:true ~libs:["gsl"] ~requires:["use_bigarray"];
      add_syntax "use_cells_syntax" "cells/pa_cells.cmo";
      add_syntax "use_foreach" "syntax/pa_foreach.cmo";
      add_lib "use_luaml" "lua-ml" ~libs:["lua-std"]
        ~fallback:[get_root / ".." / ".." / "lib"/ "lua-ml"];
      add_lib "use_gtk2" "lablgtk2" ~libs:["lablgtk"] ~objs:["gtkInit"];
      add_lib "use_gtkglext" "gtkglext" ~libs:["gtkglext"] ~requires:["use_gtk2"];
      add_lib "use_cairo" "cairo" ~requires:["use_bigarray"] ~libs:["cairo"; "cairo_lablgtk"] ~fallback:["+cairo"];
      add_lib "use_mpi" "ocamlmpi" ~libs:["mpi"];
      add_lib "use_gnuplot" "gnuplot" ~libs:["gnuplot"];
      add_lib "use_opengl" "lablGL" ~libs:["lablgl"];
      add_lib "use_glut" "" ~libs:["lablglut"] ~requires:["use_opengl"];
      add_lib "use_camlimages" "camlimages" ~libs:["ci_core"; "ci_bmp"; "ci_ppm"];
      add_lib "use_pcre" "pcre" ~libs:["pcre"];
      add_lib "use_netsys" "netsys" ~libs:["netsys"] ~requires:["use_pcre"; "use_unix"];
      add_lib "use_annexlib" "annexlib" ~libs:["annexlib"] ~requires:["use_pcre"];
      add_lib "use_netstring" "netstring" ~libs:["netstring"] ~requires:["use_netsys"];
      add_lib "use_json" "json-wheel" ~libs:["jsonwheel"] ~requires:["use_netstring"];
      add_lib "use_pvm" "pvm" ~libs:["pvm"];
      add_lib "use_type_conv" "type-conv" ~objs:["pa_type_conv"];
      pycaml_rules;
      pp_rules;
      test_rules;
      native_test_rules;
      ocaml_libevent_rules;
      lemmy_test_rules;
      mpich_rules;
      mpich2_rules;
      openmpi_rules;
      internal_libs;
    ]
  in
    dispatch begin
      fun stage ->
        List.iter (fun f -> f stage) l
    end
and pp_rules = function
    After_rules ->
      preprocess_rule "ml";
      preprocess_rule "mli";
  | _ -> ()
and mpich_rules = function
    After_rules ->
      let lib_dir = sprintf "-L%s" mpich_lib_dir in
      flag ["ocaml"; "link"; "use_mpich"] (S[A"-ccopt"; A lib_dir; A"-cclib"; A"-lpmpich"; A"-cclib"; A"-lmpich"]);
  | _ -> ()
and mpich2_rules = function
    After_rules ->
      let lib_dir = sprintf "-L%s" mpich2_lib_dir in
      let lib_flags = if (slurp_command "uname") = "Darwin\n" then
	S[A"-ccopt"; A lib_dir;  A"-cclib"; A"-lmpich"; A"-cclib"; A"-lpmpich"]
      else
	S[A"-ccopt"; A lib_dir;  A"-cclib"; A"-lmpich"]
      in
      flag ["ocaml"; "link"; "use_mpich2"] lib_flags;
  | _ -> ()
and openmpi_rules = function
    After_rules ->
      let lib_dir = sprintf "-L%s" openmpi_lib_dir in
      flag ["ocaml"; "link"; "use_openmpi"] (S[A"-ccopt"; A"-D_REENTRANT"]);
      (*flag ["ocaml"; "link"; "use_openmpi"] (S[A"-ccopt"; A"-Wl,-u,_munmap"; A"-ccopt"; A"-Wl,-multiply_defined,suppress"; A"-ccopt"; A lib_dir; A"-cclib"; A"-lmpi"; A"-cclib"; A"-lopen-rte"; A"-cclib"; A"-lopen-pal"]);*)
      flag ["ocaml"; "link"; "use_openmpi"] (S[A"-ccopt"; A lib_dir; A"-cclib"; A"-lmpi"; A"-cclib"; A"-lopen-rte"; A"-cclib"; A"-lopen-pal"]);
  | _ -> ()
and ocaml_libevent_rules = function
    After_rules ->
      flag ["file:ocaml-libevent/libmlevent.a"; "ocamlmklib"] (S[A"-L/opt/local/lib"; A"-levent"]);
      flag ["file:ocaml-libevent/event_stubs.c"; "compile"] (S[A"-ccopt"; A"-I/opt/local/include"]);
      flag ["ocaml"; "link"; "use_libevent"] (S[A"-ccopt"; A"-L/opt/local/lib"; A"-cclib"; A"-levent"; P"ocaml-libevent/libmlevent.a"]);
      dep  ["ocaml"; "link"; "use_libevent"]  ["ocaml-libevent/libmlevent.a"];
      flag ["ocaml"; "link"; "byte"; "use_libevent"] (A"-custom")
  | _ -> ()
and internal_libs = function
    After_rules ->
      ocaml_lib ~tag_name:"use_sched" "util/linuxScheduler";
      flag ["file:util/linuxScheduler.a"; "ocamlmklib"] (S[A"-L/opt/local/lib"; A"-levent"]);
      flag ["ocaml"; "link"; "use_sched"] (S[P"util/liblinuxScheduler.a"]);
      dep  ["ocaml"; "link"; "use_sched"]  ["util/liblinuxScheduler.a"];
      flag ["ocaml"; "link"; "byte"; "use_sched"] (A"-custom")
  | _ -> ()
and test_rules = function
    After_rules ->
      let prod = "%(path)/test_%(test).passed"
      and dep = "%(path)/test_%(test).d.byte" in
      rule "Run unit tests: test_%.d.byte -> test_%.passed"
        ~prod ~dep
        begin fun env build ->
          let test = env dep
          and p = env prod in
          Seq[Cmd(P test); Cmd(S[A"touch"; Px p])]
        end;
      let ocrp = "OCAMLRUNPARAM" in
      let prev_OCRP = try Unix.getenv ocrp with Not_found -> "" in
      Unix.putenv ocrp ("b"^prev_OCRP)
  | _ -> ()
and lemmy_test_rules = function
    After_rules ->
      let prod = "lemmy/test_%(test).passed"
      and dep = "lemmy/test_%(test).scm" in
      rule "Run lemmy unit tests: test_%.scm -> test_%.passed"
        ~prod ~dep
        begin fun env build ->
          let test = env dep
          and p = env prod
          and interp = "lemmy/lemmy_repl.d.byte"
          in
          List.iter Outcome.ignore_good (build [[interp]]);
          Seq[Cmd(S[P interp; P test]); Cmd(S[A"touch"; Px p])]
        end;
      let ocrp = "OCAMLRUNPARAM" in
      let prev_OCRP = try Unix.getenv ocrp with Not_found -> "" in
      Unix.putenv ocrp ("b"^prev_OCRP)
  | _ -> ()
and native_test_rules = function
    After_rules ->
      let prod = "%(path)/test_%(test).native.passed"
      and dep = "%(path)/test_%(test).native" in
      rule "Run unit tests: test_%.native -> test_%.passed"
        ~prod ~dep
        begin fun env build ->
          let test = env dep
          and p = env prod in
          Seq[Cmd(P test); Cmd(S[A"touch"; Px p])]
        end;
      let ocrp = "OCAMLRUNPARAM" in
      let prev_OCRP = try Unix.getenv ocrp with Not_found -> "" in
      Unix.putenv ocrp ("b"^prev_OCRP)
  | _ -> ()
and pycaml_rules = function
    After_rules ->
      (* linking with pycaml *)
      flag [ "byte"; "ocaml"; "link"; "use_pycaml" ]
        (S[A(get_root ^ "/lib/pycaml-0.82/pycaml.cmo");
           A"-custom"; A(get_root ^ "/lib/pycaml-0.82/pycaml_ml.o");
           A"-cclib"; A("-L" ^ python_config);
           A"-cclib"; A("-l" ^ python_lib)
          ]);
  | _ -> ()
and dtypes = function
    After_rules ->
      flag ["ocaml"; "compile"] (A"-dtypes");
      flag ["ocaml"; "compile"; "native"] (S[A"-inline";A"20"])
  | _ -> ()
and gtk2_cflags = ["-I/usr/include/gtk-2.0"; "-I/usr/lib/gtk-2.0/include"; "-I/usr/include/atk-1.0"; "-I/usr/include/cairo"; "-I/usr/include/pango-1.0"; "-I/usr/include/glib-2.0"; "-I/usr/lib/glib-2.0/include"; "-I/usr/include/freetype2"; "-I/usr/include/libpng12"]
;;

(*
let _ = begin function

  (*c Here one can change the default value of options, they can still be updated by a command line option. *)
  | Before_options -> ()

  (*c Here one can change the final value of options. *)
  | After_options -> ()

  (*c This hook is called before the hygiene phase.
      This phase also serve as collecting all the information about the
      source tree. *)
  | Before_hygiene ->

      (*c Here you can dynamically tag some files or directories. *)
      (*c This is done here by checking the [SOME_COND] variable which is
          impossible in the \tags file. *)
      ()

  (*c One can also do things after the hygiene step. *)
  | After_hygiene -> ()

  (*c One can setup rules before the standard ones but that's not recommended. *)
  | Before_rules -> ()

  (*c Here one can add or override new rules *)
end
*)

go()
