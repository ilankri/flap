(** This module implements a compiler from Hobix to Fopix. *)

(** As in any module that implements {!Compilers.Compiler}, the source
    language and the target language must be specified. *)

module Source = Hobix
module S = Source.Ast
module Target = Language
module T = Target.Ast

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
  Util.Error.error "compilation" pos msg

let make_fresh_variable =
  let r = ref 0 in
  fun () -> incr r; T.Id (Printf.sprintf "_fopix_%d" !r)

let make_fresh_function_identifier =
  let r = ref 0 in
  fun () -> incr r; T.FunId (Printf.sprintf "_fopix_f%d" !r)

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

let fun_call f args = T.FunCall (T.FunId f, args)

type block_kind = Block | String

let allocate_block' kind e =
  fun_call
    (match kind with Block -> "allocate_block" | String -> "allocate_string")
    [e]

let allocate_block e = allocate_block' Block e

let allocate_string e = allocate_block' String e

let write_block' kind e i v =
  fun_call
    (match kind with Block -> "write_block" | String -> "write_string")
    [e; i; v]

let write_block e i v = write_block' Block e i v

let write_string e i v = write_block' String e i v

let read_block e i = fun_call "read_block" [e; i]

let lint i =
  T.(Literal (LInt (Int32.of_int i)))

let is_primitive x =
  Interpreter.is_binary_primitive x || x = "print_int" ||
  x = "print_string" || x = "equal_string" || x = "equal_char"

let unwrap_rec_defs rdefs =
  List.split (
    List.map (fun (f, e) ->
      match e with
      | S.Fun (formals, body) -> (f, (formals, body))
      | _ -> assert false
    ) rdefs
  )

let free_variables =
  let module M =
    Set.Make (struct type t = S.identifier let compare = compare end)
  in
  let rec unions f = function
    | [] -> M.empty
    | [s] -> f s
    | s :: xs -> M.union (f s) (unions f xs)
  in
  let rec fvs = function
    | S.Literal _ ->
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
        let xs = unions fvs (a :: List.map snd rdefs) in
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
  fun e ->
    M.elements (
      M.filter (fun (S.Id x) ->
        not (is_primitive x) &&
        not (x = "true" || x = "false" || x = "nothing")
      ) (fvs e)
    )

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
  (Hobix.Ast.identifier, Ast.expression) Util.Dict.t

let initial_environment () =
  Util.Dict.empty

(** [translate p env] turns an Hobix program [p] into a Fopix program
    using [env] to retrieve contextual information. *)
let translate (p : S.t) env =
  let rec program env defs =
    List.(flatten (map definition defs)), env

  and definition = function
    | S.DeclareExtern id -> [T.ExternalFunction (function_identifier id)]
    | S.DefineValue (x, e) ->
        let fs, e = expression Util.Dict.empty e in
        fs @ [T.DefineValue (identifier x, e)]
    | S.DefineRecFuns rdefs ->
        let fs, closures, fill_closures =
          define_recursive_functions rdefs
        in
        fs
        @ List.map (fun (x, e) -> T.DefineValue (x, e)) closures
        @ [T.DefineValue (make_fresh_variable (), fill_closures)]

  and define_recursive_functions rdefs =
    let fs, defs = unwrap_rec_defs rdefs in
    mk_closures fs defs

  and alloc_closure fvs_count =
    let closure = make_fresh_variable () in
    (closure, allocate_block (lint (fvs_count + 1)))

  and fill_closure closure fvs formals body fun_id =
    let offsets = List.map lint (Util.ExtStd.List.range 1 (List.length fvs)) in
    let fenv = make_fresh_variable () in
    let fdefs, body =
      let env =
        match fun_id with
        | None -> env
        | Some f -> Util.Dict.insert f (T.Variable fenv) env
      in
      let env =
        List.fold_left2 (fun env fv offset ->
          Util.Dict.insert fv (read_block (T.Variable fenv) offset) env
        ) env fvs offsets
      in
      expression env body
    in
    let f = make_fresh_function_identifier () in
    let fdef =
      T.DefineFunction (f, fenv :: List.map identifier formals, body)
    in
    let fill_closure =
      let closure =
        T.Variable (match fun_id with None -> closure | Some x -> identifier x)
      in
      let store_free_variable offset fv =
        write_block closure offset (T.Variable fv)
      in
      seqs (
        write_block closure (lint 0) (T.Literal (T.LFun f))
        :: List.map2 store_free_variable offsets (List.map identifier fvs)
        @ (match fun_id with None -> [closure] | Some _ -> [])
      )
    in
    (fdef :: fdefs, fill_closure)

  and mk_closure formals body =
    let fvs = free_variables (S.Fun (formals, body)) in
    let closure, alloc_closure_e = alloc_closure (List.length fvs) in
    let fdefs, e = fill_closure closure fvs formals body None in
    (fdefs, T.Define (closure, alloc_closure_e, e))

  and mk_closures fs defs =
    let fvs =
      List.map2 (fun f (formals, body) ->
        List.filter (( <> ) f) (free_variables (S.Fun (formals, body)))
      ) fs defs
    in
    let closures =
      List.map (fun fvs -> alloc_closure (List.length fvs)) fvs
    in
    let fdefs, es =
      let closure_ids, _ = List.split closures in
      List.split (
        List.map2 (fun (closure, fvs) (f, (formals, body)) ->
          fill_closure closure fvs formals body (Some f)
        ) (List.combine closure_ids fvs) (List.combine fs defs)
      )
    and closure_defs =
      List.map2 (fun f (closure_id, closure_def) ->
        ( identifier f
        , T.Define (closure_id, closure_def, T.Variable closure_id) )
      ) fs closures
    in
    (List.flatten fdefs, closure_defs, seqs es)

  and expression env = function
    | S.Literal (S.LString s) when Options.get_retromips () ->
        let s_len = String.length s in
        ( []
        , define (allocate_string @@ lint s_len) (fun str ->
            let str = T.Variable str in
            let fill_string =
              let write_char i =
                write_string str (lint i) (lint @@ Char.code s.[i])
              in
              List.map write_char (Util.ExtStd.List.range 0 (s_len - 1))
              @ [write_string str (lint s_len) (lint 0)]

            in
            seqs (fill_string @ [str])
          ) )

    | S.Literal l -> ([], T.Literal (literal l))

    | S.While (cond, e) ->
        let cfs, cond = expression env cond in
        let efs, e = expression env e in
        (cfs @ efs, T.While (cond, e))

    | S.Variable (S.Id id as x) ->
        let xc =
          if is_primitive id then
            T.Literal (T.LFun (function_identifier x))
          else
            match Util.Dict.lookup x env with
            | None -> T.Variable (identifier x)
            | Some e -> e
        in
        ([], xc)

    | S.Define (x, a, b) ->
        let afs, a = expression env a in
        let bfs, b = expression env b in
        (afs @ bfs, T.Define (identifier x, a, b))

    | S.DefineRec (rdefs, a) ->
        let fdefs, closures, e =
          let fs, defs = unwrap_rec_defs rdefs in
          mk_closures fs defs
        and fdefs', a = expression env a in
        let e =
          List.fold_left (fun acc (x, e) ->
            T.Define (x, e, acc))
            (seq e a) closures
        in
        (fdefs @ fdefs', e)

    | S.Apply (a, bs) ->
        let idfs, id = expression env a in
        let fsWithExprs =  List.map (expression env) bs in
        let (fs, es) =
          List.fold_right
            (fun (a,b) (la, lb) -> (a@la, b::lb))
            fsWithExprs
            ([],[])
        in
        begin
          match id with
          | T.Literal (T.LFun f) -> idfs@fs, T.FunCall(f, es)
          | e ->
              idfs @ fs, T.UnknownFunCall (read_block e (lint 0), e :: es)
        end

    | S.IfThenElse (a, b, c) ->
        let afs, a = expression env a in
        let bfs, b = expression env b in
        let cfs, c = expression env c in
        (afs @ bfs @ cfs, T.IfThenElse (a, b, c))

    | S.Fun (x, e) -> mk_closure x e

    | S.AllocateBlock a ->
        let afs, a = expression env a in
        (afs, allocate_block a)

    | S.WriteBlock (a, b, c) ->
        let afs, a = expression env a in
        let bfs, b = expression env b in
        let cfs, c = expression env c in
        (afs @ bfs @ cfs, fun_call "write_block" [a; b; c])

    | S.ReadBlock (a, b) ->
        let afs, a = expression env a in
        let bfs, b = expression env b in
        (afs @ bfs, fun_call "read_block" [a; b])

    | S.Switch (a, bs, default) ->
        let afs, a = expression env a in
        let bsfs, bs = List.(split (map (expression env) (Array.to_list bs))) in
        let dfs, default = match default with
          | None -> [], None
          | Some e -> let bs, e = expression env e in bs, Some e
        in
        (afs @ List.flatten bsfs @ dfs, T.Switch (a, Array.of_list bs, default))

  and _expressions env = function
    | [] ->
        [], []
    | e :: es ->
        let efs, es = _expressions env es in
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
