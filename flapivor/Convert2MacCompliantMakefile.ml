(*
Compile command : ocamlc str.cma Convert2MacCompliantMakefile.ml -o Convert2MacCompliantMakefile
Execute command : Convert2MacCompliantMakefile ./
*)
open Printf

let modifyCut text =
  let r = Str.regexp {|cut -f1 -d\' \'|} in
  Str.global_replace r "tr -s ' ' | cut -f2 -d' '" text

let modifyRM text =
  let r = Str.regexp {|rm --force|} in
  Str.global_replace r "rm -f" text

let modifyRmQuote text =
let r = Str.regexp {|\$(RM) --force|} in
  Str.global_replace r "$(RM) -f" text

let modifyTimeout text =
let r = Str.regexp {|timeout|} in
  Str.global_replace r "gtimeout" text

let modify text =
    let text1 = modifyCut text in
    let text2 = modifyRM text1 in
    modifyRmQuote text2

let readFile file = 
  let ic = open_in file in
  let new_file = file^".new" in
  let oc = open_out new_file in
  try 
    while true; do
    let line = input_line ic in       (* read line from in_channel and discard \n *)
    fprintf oc "%s\n" (modify line);
    flush stdout                      (* write on the underlying device now *)
    done;
  with
  | End_of_file -> 
    let _ = close_in ic in             (* close the input channel *)
    let _ = close_out oc in
    print_endline ("Closing file:"^file^"...");
    print_endline ("Closing out file:"^new_file^"...");
    (*if (Sys.file_exists new_file) then
      (print_endline ("Delete file:"^file);let _ = Sys.remove file in Sys.rename new_file file;))*)
  | e -> close_in_noerr ic;                (* emergency closing *)
         raise e                           (* exit with error: files are closed but channels are not flushed *)

let rec checkFile parent file=
  let full_path = parent^"/"^file in
  if (Sys.is_directory full_path) then 
      (*let _ = print_endline ("Dir: "^full_path) in *)
      exploreDir full_path
  else
    let r = Str.regexp {|Makefile.*|} in
      if (Str.string_match r file 0) then
	let _ = print_endline ("Read file: "^full_path) in
 	readFile full_path;
and exploreDir dir =
  let children = Sys.readdir dir in
  Array.iter (checkFile dir) children;;

let rec renameMlFiles parent file=
  let full_path = parent^"/"^file in
  if (Sys.is_directory full_path) then
    let () = print_endline ("Rename Makefile under: "^full_path) in
    renameMlFilesInDir full_path
  else
    let r = Str.regexp {|Makefile.*\.new$|} in
    if (Str.string_match r full_path 0) then
      (let path_no_new = (String.sub full_path 0 ((String.length full_path)-4)) in
      print_endline ("Rename file :"^full_path^" to "^path_no_new);
      Sys.remove path_no_new;
      Sys.rename full_path path_no_new)
and renameMlFilesInDir dir=
  let child = Sys.readdir dir in
  Array.iter (renameMlFiles dir) child;;

let () = exploreDir "."; renameMlFilesInDir ".";;
