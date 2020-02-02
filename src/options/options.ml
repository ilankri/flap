(** Options *)

open Util.ExtStd

let make_string_option what kind =
  let language = ref "" in
  let get () =
    if !language = "" then
      Util.Error.global_error
        "during analysis of options"
        (Printf.sprintf "You should specify the %s %s using '--%s'."
           kind what kind);
    !language
  in
  let set = ( := ) language in
  let is_set () = !language <> "" in
  get, set, is_set

let (get_source_language, set_source_language, is_source_language_set) =
  make_string_option "language" "source"

let (get_target_language, set_target_language, is_target_language_set) =
  make_string_option "language" "target"

type mode = Interactive | Batch

let mode = ref Batch

let set_mode = ( := ) mode

let get_mode () = !mode

let (get_input_filename, set_input_filename, is_input_filename_set) =
  make_string_option "filename" "input"

let using : string list ref = ref []
let insert_using x = using := x :: !using
let get_using () = !using

let set_interactive_mode = function
  | true -> set_mode Interactive
  | false -> set_mode Batch

let set_running_mode, get_running_mode = Ref.as_functions false
let set_verbose_mode, get_verbose_mode = Ref.as_functions false
let set_dry_mode, get_dry_mode         = Ref.as_functions false
let set_benchmark, get_benchmark       = Ref.as_functions false
let set_unsafe, get_unsafe             = Ref.as_functions false
let set_gcc, get_gcc                   = Ref.as_functions false
let set_show_types, get_show_types     = Ref.as_functions false
let set_infer_types, get_infer_types   = Ref.as_functions false
let set_check_types, get_check_types   = Ref.as_functions true
let set_verbose_eval, get_verbose_eval = Ref.as_functions false
let set_retromips, get_retromips       = Ref.as_functions false

let compilation_unit_name = ref "Flap"

(** Command line arguments analysis. *)
module CommandLine = struct
  let options_list =
    ref []

  let push local_options =
    options_list := !options_list @ local_options

  let options names kind doc =
    let first_shot =
      let state = ref true in
      fun s ->
        if !state then (state := false; s)
        else
          (List.hd (Str.(split (regexp " ") doc)))
    in
    List.map (fun n -> (n, kind, first_shot doc)) names

  let show_version_and_exits () =
    Printf.printf "flap %s\n%!" Version.number;
    exit 0

  let generic_options = Arg.(align (List.flatten [
    options
      ["--version"; "-v"]
      (Unit show_version_and_exits)
      " Show the version number and exits.";

    options
      ["--source"; "-s"]
      (String set_source_language)
      (" Set the source programming language");

    options
      ["--target"; "-t"]
      (String set_target_language)
      (" Set the target programming language");

    options
      ["--interactive"; "-i"]
      (Bool set_interactive_mode)
      ("(true|false) Set the compiler mode");

    options
      ["--run"; "-r"]
      (Bool set_running_mode)
      ("(true|false) Ask the compiler to run the compiled code");

    options
      ["--verbose"; "-V"]
      (Bool set_verbose_mode)
      ("(true|false) Ask the compiler to be verbose");

    options
      ["--verbose-eval"; "-VV"]
      (Bool set_verbose_eval)
      ("(true|false) Ask the compiler to be show the result of evaluation");

    options
      ["--dry"; "-d"]
      (Bool set_dry_mode)
      ("(true|false) Ask the compiler not to produce compiled file");

    options
      ["--unsafe"; "-u"]
      (Bool set_unsafe)
      ("(true|false) Ask the compiler not to typecheck");

    options
      ["--bench"; "-B"]
      (Bool set_benchmark)
      ("(true|false) Ask the compiler to show evaluation time.");

    options
      ["--using"; "-!" ]
      (String insert_using)
      (" Force the compilation to use this intermediate language");

    options
      ["--gcc"; "-G" ]
      (Bool set_gcc)
      ("(true|false) Ask to compiler to produce assembly code in GCC format");

    options
      ["--types"; "-T"]
      (Bool set_show_types)
      ("(true|false) Ask the compiler to show types for toplevel values.");

    options
      ["--infer"; "-I"]
      (Bool set_infer_types)
      ("(true|false) Ask the compiler to infer types for toplevel values.");

    options
      ["--typechecking"; "-C"]
      (Bool set_check_types)
      ("(true|false) Ask the compiler to check types for toplevel values.");

    options
      ["--retromips"]
      (Bool set_retromips)
      ("(true|false) Activate the MIPS version of Retrolix.")

  ]))

  let usage_msg =
    "flap [options] input_filename"

  let parse () =
    Arg.parse !options_list set_input_filename usage_msg

  let initialize =
    push generic_options
end
