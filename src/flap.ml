(** The main driver module.

    The role of this module is to have [flap] behave as the command
    line options say. In particular, these options determine:

    - if the compiler is run in interactive or batch mode.
    - what is the source language of the compiler.
    - what is the target language of the compiler.

*)

(* -------------------------- *)
(*   Initialization process   *)
(* -------------------------- *)

open Options

let rec initialize () =
  initialize_options ();
  initialize_languages ();
  initialize_prompt ();

and initialize_prompt () =
  Util.UserInput.set_prompt "flap> "

and initialize_options () =
  Options.CommandLine.parse ()

and initialize_languages () =
  Hopix.initialize ();
  Hobix.initialize ();
  Fopix.initialize ();
  Retrolix.initialize ();
  Mips.initialize ();
  Javix.initialize ();
  Anfix.initialize ();
  Kontix.initialize ();
  Jakix.initialize ()

(** Given the source language and the target language returns
    the right compiler (as a first-class module). *)
let get_compiler () : (module Common.Compilers.Compiler) =
  let source_language =
    get_source_language ()
  in
  let target_language =
    if is_target_language_set () then
      get_target_language ()
    else
      source_language
  in
  let using = List.map Common.Languages.get (Options.get_using ()) in
  Common.Compilers.get
    ~using
    (Common.Languages.get source_language)
    (Common.Languages.get target_language)

(** The evaluation function evaluates some code and prints the results
    into the standard output. It also benchmarks the time taken to
    evaluates the code, if asked. *)
let eval runtime eval print =
  let now = Unix.gettimeofday () in
  let observation, runtime = eval runtime in
  let elapsed_time = Unix.gettimeofday () -. now in
  if Options.get_benchmark () then
    print_endline ("(" ^ string_of_float elapsed_time ^ "s)");
  if Options.get_verbose_eval () then
    print_endline (print runtime observation);
  runtime

(* -------------------- **)
(*   Interactive mode    *)
(* -------------------- **)
(**

   The interactive mode is a basic read-compile-eval-print loop.

*)
let interactive_loop () =

  Printf.printf "        Flap version %s\n\n%!" Version.number;

  let module Compiler = (val get_compiler () : Common.Compilers.Compiler) in
  let open Compiler in

  let read () =
    initialize_prompt ();
    let b = Buffer.create 13 in
    let rec read prev =
      let c = Util.UserInput.input_char stdin in
      if c = "\n" then
        if prev <> "\\" then (
          Buffer.add_string b prev;
          Buffer.contents b
        ) else (
          Util.UserInput.set_prompt "....> ";
          read c
        )
      else (
        Buffer.add_string b prev;
        read c
      )
    in
    read ""
  in

  let rec step
    : Target.Interpreter.runtime ->
      Compiler.environment -> Source.Typechecker.typing_environment
      -> Target.Interpreter.runtime * Compiler.environment *
         Source.Typechecker.typing_environment =
    fun runtime cenvironment tenvironment ->
      try
        match read () with
        | "+debug" ->
            Options.set_verbose_mode true;
            step runtime cenvironment tenvironment

        | "-debug" ->
            Options.set_verbose_mode false;
            step runtime cenvironment tenvironment

        | input ->
            let ast = Compiler.Source.Parser.parse_string input in
            let tenvironment =
              if Options.get_unsafe () then
                tenvironment
              else
                Compiler.Source.Typechecker.typecheck tenvironment ast
            in
            let cast, cenvironment = Compiler.translate ast cenvironment in
            if Options.get_verbose_mode () then
              print_endline (Target.Parser.print_ast cast);
            let runtime = Compiler.Target.(
              eval
                runtime
                (fun init ->
                   Interpreter.Machine.run ~init (Interpreter.evaluate cast))
                Interpreter.print_observable
            )
            in
            step runtime cenvironment tenvironment
      with
      | e when !Sys.interactive -> raise e (* display exception at toplevel *)
      | Util.Error.Error (positions, msg) ->
          output_string stdout (Util.Error.print_error positions msg);
          step runtime cenvironment tenvironment
      | End_of_file ->
          (runtime, cenvironment, tenvironment)
      | e ->
          print_endline (Printexc.get_backtrace ());
          print_endline (Printexc.to_string e);
          step runtime cenvironment tenvironment
  in
  Util.Error.resume_on_error ();
  ignore (step
            (Target.Interpreter.initial_runtime ())
            (Compiler.initial_environment ())
            (Source.Typechecker.initial_typing_environment ())
         )

(* ------------- **)
(*   Batch mode   *)
(* ------------- **)
(**

   In batch mode, the compiler loads a file written in the source
   language and produces a file written in the target language.

   The filename of the output file is determined by the basename
   of the input filename concatenated with the extension of the
   target language.

   If the running mode is set, the compiler will also interpret
   the compiled code.

*)
let batch_compilation () =
  Util.Error.exit_on_error ();
  let module Compiler = (val get_compiler () : Common.Compilers.Compiler) in
  let open Compiler in
  let input_filename = Options.get_input_filename () in
  let module_name = Filename.chop_extension input_filename in
  let ast = Source.Parser.parse_filename input_filename in
  if not (Options.get_unsafe ()) then
    Compiler.Source.(
      let tenv =
        Typechecker.typecheck (Typechecker.initial_typing_environment ()) ast
      in
      if Options.get_show_types () then (
        print_endline (Typechecker.print_typing_environment tenv)
      )
    );
  let cast, _ = translate ast (initial_environment ()) in
  let output_filename = module_name ^ Target.extension in
  if Options.get_verbose_mode () then
    output_string stdout (Target.Parser.print_ast cast ^ "\n");
  if not (Options.get_dry_mode ()) then (
    let cout = open_out output_filename in
    output_string cout (Target.Parser.print_ast cast);
    close_out cout;
  );
  if Options.get_running_mode () then Compiler.Target.(
    ignore (
      try
        let print =
          if Options.get_verbose_eval () then
            Interpreter.print_observable
          else
            fun _ _ -> ""
        in
        eval
          (Interpreter.initial_runtime ())
          (fun init ->
             Interpreter.Machine.run ~init (Interpreter.evaluate cast))
          print
      with
      | e ->
          print_endline (Printexc.get_backtrace ());
          print_endline (Printexc.to_string e);
          exit 1
    )
  )

(* -------------- *)
(*   Entry point  *)
(* -------------- *)
let main =
  if !Sys.interactive
  then ()
  else begin
    initialize ();
    match get_mode () with
    | Interactive -> interactive_loop ()
    | Batch -> batch_compilation ()
  end
