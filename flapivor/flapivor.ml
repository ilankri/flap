open Unix

let force = false

let testsuites = String.concat " " (List.tl (Array.to_list Sys.argv))

let check_testsuites =
  if testsuites = "" then (
    Printf.printf "At least one testsuite is required.\n";
    exit 1
  )

let update () =
  ignore (Sys.command "mr up 2> /dev/null 1> /dev/null")

let lines_of command =
  let out = open_process_in command in
  let rec aux lines =
    match try Some (input_line out) with _ -> None with
      | None -> List.rev lines
      | Some l -> aux (l :: lines)
  in
  let lines = aux [] in
  ignore (close_process_in out);
  lines

let list_projects () =
  lines_of "./list-projects.sh"

let commits = Hashtbl.create 13

let commit_of project = Hashtbl.find commits project

let git project cmd =
   Printf.sprintf "cd %s && git %s" project cmd

let is_updated project =
  let current_commit = List.hd (lines_of (git project "log --format=\"%H\" -n 1")) in
  let current_subject = List.hd (lines_of (git project "log --format=\"%s\" -n 1")) in
  let must_ignore = not force && try 
     ignore Str.(search_forward (regexp "IGNORE") current_subject 0);
     true
  with Not_found -> false
  in
  let update () = Hashtbl.replace commits project current_commit in
  if must_ignore then (
     Printf.printf "%s: Ignore commit %s as requested.\n%!" project current_commit;
     update ();
     false
  ) else  (
    try
      let last_commit = commit_of project in
      update ();
      last_commit <> current_commit && not must_ignore
    with Not_found ->
      update ();
      true
  )

let command cmd =
   let status = Sys.command cmd in
   if status <> 0 then Printf.printf "Command `%s' failed\n%!" cmd

let evaluate project =
  let commit = commit_of project in
  let subject = List.hd (lines_of (git project "log --format=\"%s\" -n 1")) in
  let logfile = Printf.sprintf "%s/log/%s" project commit in
  if not (Sys.file_exists logfile) then (
    command (Printf.sprintf "mkdir -p %s/log" project);
    command (Printf.sprintf "./flapitest %s %s %s \"%s\" %s"  project commit logfile subject testsuites)
  ) else Printf.printf "%s already exists.\n" logfile
     

let rec loop () = Unix.(
  update ();
  list_projects () 
  |> List.filter is_updated 
  |> List.iter evaluate;
  Unix.sleep 120;
  loop ()
)

let main = loop ()