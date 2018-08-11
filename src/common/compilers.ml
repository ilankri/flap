(** Compilers. *)

open Languages

(** A compiler translates programs from a source language
    into programs of a target language. *)
module type Compiler = sig

  module Source : Language
  module Target : Language

  type environment
  val initial_environment : unit -> environment

  val translate : Source.ast -> environment -> Target.ast * environment

end

(** Given a compiler from L1 to L2 and a compiler from L2 to L3,
    one can get compiler from L1 to L3. *)
let compose (module C1 : Compiler) (module C2 : Compiler) : (module Compiler) =
  let c2_source_is_c1_target =
    Obj.magic (* Do not do that at home, kids! *)
  in
  (module struct
    module Source = C1.Source

    module Target = C2.Target

    type environment = C1.environment * C2.environment

    let initial_environment () =
      (C1.initial_environment (), C2.initial_environment ())

    let translate source (env1, env2) =
      let (intermediate, env1') = C1.translate source env1 in
      let (target, env2') = C2.translate (c2_source_is_c1_target intermediate) env2 in
      (target, (env1', env2'))
  end : Compiler)

let rec join = function
  | [x] -> x
  | [x; y] -> compose x y
  | x :: xs -> compose x (join xs)
  | _ -> assert false

let string_of_compiler_passes xs =
  String.concat " -> " (
    List.map (fun (module C : Compiler) -> C.Source.extension
  ) xs)

(** Compiler implementations are stored in the following
    hashing table. *)
let compilers : (string  * (string * (module Compiler))) list ref =
  ref []

let register (module C : Compiler) =
  let source = C.Source.name and target = C.Target.name in
  compilers := (source, (target, (module C))) :: !compilers

let compilers_from source =
  List.(
    !compilers
    |> filter (fun (source', _) -> source = source')
    |> map snd
  )

let find compilers source target using = List.(ExtStd.List.Monad.(
  let rec search seen source target =
    if List.mem source seen then
      fail
    else (
      take_one (compilers_from source) >>= fun (target', m) ->
      if target = target' then
        return [(target', m)]
      else (
        search (source :: seen) target' target >>= fun ms ->
        return ((target', m) :: ms)
      )
    )
  in
  run (search [] source target)
  |> filter (fun p -> for_all (fun u -> exists (fun (l, _) -> l = u) p) using)
  |> map (map snd)
))

let get ?(using=[]) (module Source : Language) (module Target : Language) =
  let using = List.map (fun (module L : Language) -> L.name) using in
  match find compilers Source.name Target.name using with
    | [] ->
      Error.global_error
        "during compilation"
        "Sorry, there is no such compiler in flap."
    | [x] ->
      join x
    | xs ->
      Error.global_error
        "during compilation"
        ("Sorry, there are many ways to implement this compiler in flap:\n" ^
            String.concat "\n" (List.map string_of_compiler_passes xs))

(** There is an easy way to compile a language into itself:
    just use the identity function :-). *)
module Identity (L : Language) : Compiler = struct
  module Source = L
  module Target = L
  type environment = unit
  let initial_environment () = ()
  let translate x () = (x, ())
end
