(** This module implements the interpreter of the MIPS programming
    language. *)

open Util.Error
open Ast

let error msg =
  global_error "Mips execution" msg

type runtime = unit

type observable = int

let initial_runtime () = ()

let show_runtime = ()

(** -------------------------- *)
(** {1 Instruction execution } *)
(** -------------------------- *)

let evaluate runtime (ast : t) =
  let fname = Filename.temp_file "flap" "mips" in
  let cout = open_out fname in
  output_string cout (PrettyPrinter.(to_string program ast));
  close_out cout;
  Printf.eprintf "Running spim in %s\n%!" fname;
  let status = Sys.command (Printf.sprintf "spim -file %s" fname) in
  (runtime, status)

let print_observable _ (obs : observable) =
  "spim exited with status " ^ string_of_int obs
