(** This module implements a compiler from Hobix to Fopix. *)

(** As in any module that implements {!Compilers.Compiler}, the source
    language and the target language must be specified. *)

module Source = Hobix
module S = Source.AST
module Target = Fopix
module T = Target.AST

(**

   The translation from Hobix to Fopix turns anonymous
   lambda-abstractions into toplevel functions and applications into
   function calls. In other words, it translates a high-level language
   (like OCaml) into a first order language (like C).

   To do so, we follow the closure conversion technique.

   The idea is to make explicit the construction of closures, which
   represent functions as first-class objects. A closure is a block
   that contains a code pointer to a toplevel function [f] and all the
   values needed to execute the body of [f]. For instance, consider
   the following OCaml code:

   let f =
     let x = 6 * 7 in
     let z = x + 1 in
     fun y -> x + y * z

   The values needed to execute the function "fun y -> x + y * z" are
   its free variables "x" and "z". The same program with explicit usage
   of closure can be written like this:

   let g y env = env[1] + y * env[2]
   let f =
      let x = 6 * 7 in
      let z = x + 1 in
      [| g; x; z |]

   (in an imaginary OCaml in which arrays are untyped.)

   Once closures are explicited, there are no more anonymous functions!

   But, wait, how to we call such a function? Let us see that on an
   example:

   let f = ... (* As in the previous example *)
   let u = f 0

   The application "f 0" must be turned into an expression in which
   "f" is a closure and the call to "f" is replaced to a call to "g"
   with the proper arguments. The argument "y" of "g" is known from
   the application: it is "0". Now, where is "env"? Easy! It is the
   closure itself! We get:

   let g y env = env[1] + y * env[2]
   let f =
      let x = 6 * 7 in
      let z = x + 1 in
      [| g; x; z |]
   let u = f[0] 0 f

   (Remark: Did you notice that this form of "auto-application" is
   very similar to the way "this" is defined in object-oriented
   programming languages?)

*)

let error pos msg =
  Error.error "compilation" pos msg

let make_fresh_variable =
  let r = ref 0 in
  fun () -> incr r; T.Id (Printf.sprintf "_%d" !r)

let make_fresh_function_identifier =
  let r = ref 0 in
  fun () -> incr r; T.FunId (Printf.sprintf "_%d" !r)

let define e f =
  let x = make_fresh_variable () in
  T.Define (x, e, f x)

let rec defines ds e =
  match ds with
    | [] ->
      e
    | (x, d) :: ds ->
      T.Define (x, d, defines ds e)

let seq a b =
  define a (fun _ -> b)

let rec seqs = function
  | [] -> assert false
  | [x] -> x
  | x :: xs -> seq x (seqs xs)

let allocate_block e =
  T.(FunCall (FunId "allocate_block", [e]))

let write_block e i v =
  T.(FunCall (FunId "write_block", [e; i; v]))

let read_block e i =
  T.(FunCall (FunId "read_block", [e; i]))

let lint i =
  T.(Literal (LInt (Int32.of_int i)))

let free_variables =
  let module M = Set.Make (struct type t = S.identifier let compare = compare end) in
  let rec unions f = function
    | [] -> M.empty
    | [s] -> f s
    | s :: xs -> M.union (f s) (unions f xs)
  in
  let rec fvs = function
    | S.Literal l ->
      M.empty
    | S.Variable x ->
      M.singleton x
    | S.While (cond, e) ->
       unions fvs [cond; e]
    | S.Define (x, a, b) ->
      let sa = fvs a in
      let sb = fvs b in
      M.(union sa (remove x sb))
    | S.DefineRec (rdefs, a) ->
       let fs = List.map fst rdefs in
       let xs = M.(unions fvs (a :: List.map snd rdefs)) in
       List.fold_left (fun s x -> M.remove x s) xs fs
    | S.ReadBlock (a, b) ->
      unions fvs [a; b]
    | S.Apply (a, b) ->
       unions fvs (a :: b)
    | S.WriteBlock (a, b, c) | S.IfThenElse (a, b, c) ->
      unions fvs [a; b; c]
    | S.AllocateBlock a ->
      fvs a
    | S.Fun (xs, e) ->
      List.fold_left (fun s x -> M.remove x s) (fvs e) xs
    | S.Switch (a, b, c) ->
      let c = match c with None -> [] | Some c -> [c] in
      unions fvs (a :: Array.to_list b @ c)
  in
  fun e -> M.elements (fvs e)

(**

    A closure compilation environment relates an identifier to the way
    it is accessed in the compiled version of the function's
    body.

    Indeed, consider the following example. Imagine that the following
    function is to be compiled:

    fun x -> x + y

    In that case, the closure compilation environment will contain:

    x -> x
    y -> "the code that extract the value of y from the closure environment"

    Indeed, "x" is a local variable that can be accessed directly in
    the compiled version of this function's body whereas "y" is a free
    variable whose value must be retrieved from the closure's
    environment.

*)
type environment =
    (HobixAST.identifier, FopixAST.expression) Dict.t

let initial_environment () =
  Dict.empty

(** [translate p env] turns an Hobix program [p] into a Fopix program
    using [env] to retrieve contextual information. *)
let translate (p : S.t) env =
  let rec program env defs =
    List.(flatten (map definition defs)), env
  and definition = function
    | S.DeclareExtern id ->
      [T.ExternalFunction (function_identifier id)]
    | S.DefineValue (x, e) ->
      let fs, e = expression Dict.empty e in
      fs @ [T.DefineValue (identifier x, e)]
    | S.DefineRecFuns rdefs ->
      let fs, defs = define_recursive_functions rdefs in
      fs @ List.map (fun (x, e) -> T.DefineValue (x, e)) defs
  and define_recursive_functions rdefs =
       failwith "Students! This is your job!"
  and expression env = function
    | S.Literal l ->
      [], T.Literal (literal l)
    | S.While (cond, e) ->
       let cfs, cond = expression env cond in
       let efs, e = expression env e in
       cfs @ efs, T.While (cond, e)
    | S.Variable x ->
      let xc =
	match Dict.lookup x env with
	  | None -> T.Variable (identifier x)
	  | Some e -> e
      in
      ([], xc)
    | S.Define (x, a, b) ->
	 failwith "Students! This is your job!"
    | S.DefineRec (rdefs, a) ->
	 failwith "Students! This is your job!"
    | S.Apply (a, bs) ->
      let idfs, id = expression env a in
      let fsWithExprs =  List.map (expression env) bs in
      let (fs, es) = List.fold_left (fun (la, lb) (a,b) -> (a@la, b::lb)) ([],[]) fsWithExprs in
      begin
        match id with
        | T.Variable (T.Id x) -> idfs@fs, T.FunCall(T.FunId x, es)
        | _ -> failwith "Apply should only have a string id as its first expression"
      end
    | S.IfThenElse (a, b, c) ->
      let afs, a = expression env a in
      let bfs, b = expression env b in
      let cfs, c = expression env c in
      afs @ bfs @ cfs, T.IfThenElse (a, b, c)

    | S.Fun (x, e) ->
	 failwith "Students! This is your job!"
    | S.AllocateBlock a ->
      let afs, a = expression env a in
      (afs, allocate_block a)
    | S.WriteBlock (a, b, c) ->
      let afs, a = expression env a in
      let bfs, b = expression env b in
      let cfs, c = expression env c in
      afs @ bfs @ cfs,
      T.FunCall (T.FunId "write_block", [a; b; c])
    | S.ReadBlock (a, b) ->
      let afs, a = expression env a in
      let bfs, b = expression env b in
      afs @ bfs,
      T.FunCall (T.FunId "read_block", [a; b])
    | S.Switch (a, bs, default) ->
      let afs, a = expression env a in
      let bsfs, bs = List.(split (map (expression env) (Array.to_list bs))) in
      let dfs, default = match default with
	| None -> [], None
	| Some e -> let bs, e = expression env e in bs, Some e
      in
      afs @ List.flatten bsfs @ dfs,
      T.Switch (a, Array.of_list bs, default)

  and expressions env = function
    | [] ->
       [], []
    | e :: es ->
       let efs, es = expressions env es in
       let fs, e = expression env e in
       fs @ efs, e :: es

  and literal = function
    | S.LInt x -> T.LInt x
    | S.LString s -> T.LString s
    | S.LChar c -> T.LChar c

  and identifier (S.Id x) = T.Id x

  and function_identifier (S.Id x) = T.FunId x

  in
  program env p
