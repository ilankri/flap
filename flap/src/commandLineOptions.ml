(** Command line arguments analysis. *)

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
    (String Options.set_source_language)
    (" Set the source programming language");

  options
    ["--target"; "-t"]
    (String Options.set_target_language)
    (" Set the target programming language");

  options
    ["--interactive"; "-i"]
    (Bool Options.set_interactive_mode)
    ("(true|false) Set the compiler mode");

  options
    ["--run"; "-r"]
    (Bool Options.set_running_mode)
    ("(true|false) Ask the compiler to run the compiled code");

  options
    ["--verbose"; "-V"]
    (Bool Options.set_verbose_mode)
    ("(true|false) Ask the compiler to be verbose");

  options
    ["--verbose-eval"; "-VV"]
    (Bool Options.set_verbose_eval)
    ("(true|false) Ask the compiler to be show the result of evaluation");

  options
    ["--dry"; "-d"]
    (Bool Options.set_dry_mode)
    ("(true|false) Ask the compiler not to produce compiled file");

  options
    ["--unsafe"; "-u"]
    (Bool Options.set_unsafe)
    ("(true|false) Ask the compiler not to typecheck");

  options
    ["--bench"; "-B"]
    (Bool Options.set_benchmark)
    ("(true|false) Ask the compiler to show evaluation time.");

  options
    ["--using"; "-!" ]
    (String Options.insert_using)
    (" Force the compilation to use this intermediate language");

  options
    ["--gcc"; "-G" ]
    (Bool Options.set_gcc)
    ("(true|false) Ask to compiler to produce assembly code in GCC format");

  options
    ["--types"; "-T"]
    (Bool Options.set_show_types)
    ("(true|false) Ask the compiler to show types for toplevel values.");

  options
    ["--infer"; "-I"]
    (Bool Options.set_infer_types)
    ("(true|false) Ask the compiler to infer types for toplevel values.");

  options
    ["--typechecking"; "-C"]
    (Bool Options.set_check_types)
    ("(true|false) Ask the compiler to check types for toplevel values.");

]))

let usage_msg =
  "flap [options] input_filename"

let parse () =
  Arg.parse !options_list Options.set_input_filename usage_msg

let initialize =
  push generic_options
