(** From Hopix to Hobix *)

module Source = Hopix
module Target = Language

(** The compilation environment.
    ———————————————————————————–

    To translate a program written in a source language into another
    semantically equivalent program written in a target language, it
    is convenient to carry some information about the correspondence
    between the two programs along the process. The compilation
    environment is meant to that.

    In this particular pass, we want to remember an assignment of
    integers to constructor identifiers. Therefore, the compilation
    environment is composed of a map representing this assignment. The
    environment is populated each time we cross a type definitions while
    it is read each time we translate language constructions related to
    tagged values.
*)

module ConstructorMap = Map.Make (struct
    type t = Hopix.Ast.constructor
    let compare = compare
  end)

type environment = {
  constructor_tags : Int32.t ConstructorMap.t;
}

let initial_environment () = {
  constructor_tags = ConstructorMap.empty;
}

let index_of_constructor env k =
  try ConstructorMap.find k env.constructor_tags with
  | Not_found -> assert false   (* By typing.  *)

(** Code generation
    ———————————————

    A compilation pass produces code. We could directly
    write down caml expressions made of applications of
    HobixAST constructors. Yet, the resulting code would
    be ugly...

    A better way consists in defining functions that build
    Hobix AST terms and are convenient to use. Here are a
    list of functions that may be convenient to you when
    you will implement this pass.

*)

(** [fresh_identifier ()] returns a fresh identifier, that is
    an identifier that has never been seen before. *)
let fresh_identifier =
  let r = ref 0 in
  fun () -> incr r; Ast.Id ("_hobix_" ^ string_of_int !r)

(** [def w (fun x -> e)] returns an abstract syntax tree of
    the form:

    val x = w; e

    where [x] is chosen fresh.
*)
let def w f =
  let x = fresh_identifier () in
  Ast.Define (x, w, f x)

(** [defines [d1; ..; dN] e] returns an abstract syntax tree of
    the form:

    val d1;
    ..
    val dN;
    e

*)
let defines =
  List.fold_right (fun (x, xe) e -> Ast.Define (x, xe, e))

(** [seq s1 s2] is

    val _ = s1;
    s2

*)
let seq s1 s2 =
  Ast.Define (fresh_identifier (), s1, s2)

(** [htrue] represents the primitive true in Hobix. *)
let htrue =
  Ast.(Variable (Id "true"))

(** [seqs [s1; ...; sN]] is

    val _ = s1;
    ...
    val _ = s(N - 1);
    sN
*)
let rec seqs = function
  | [] -> assert false
  | [e] -> e
  | e :: es -> seq e (seqs es)

(** [is_equal e1 e2] is the boolean expression [e1 = e2]. *)
let is_equal l e1 e2 =
  let equality = Ast.(match l with
    | LInt _ -> "`="
    | LString _ -> "equal_string"
    | LChar _ -> "equal_char"
  ) in
  Ast.(Apply (Variable (Id equality), [e1; e2]))

let is_equal_int e1 e2 = is_equal (Ast.LInt Int32.zero) e1 e2

(** [conj e1 e2] is the boolean expression [e1 && e2]. *)
let conj e1 e2 =
  Ast.(Apply (Variable (Id "`&&"), [ e1; e2 ]))

(** [conjs [e1; ..; eN]] is the boolean expression [e1 && .. && eN]. *)
let rec conjs = function
  | [] -> htrue
  | [c] -> c
  | c :: cs -> conj c (conjs cs)

let int32_literal i = Ast.(Literal (LInt i))

let int_literal i = int32_literal (Int32.of_int i)

(** [read_block hobixe i] returns [hobixe[i]]. *)
let read_block hobixe i = Ast.ReadBlock (hobixe, int_literal i)

let write_block b i e = Ast.WriteBlock (b, int_literal i, e)

(** Typical expression whose evaluation lead the program to crash.  *)
let crash =
  (* We just build an expression that does a division by zero.  *)
  let zero = int_literal 0 in
  Ast.(Apply (Variable (Id "`/"), [zero; zero]))

let located  f x = f (Util.Position.value x)

let located' f l = f (List.map Util.Position.value l)

(** Build the Hobix expression corresponding to the tag of the given
    constructor.  *)
let tag_of_constructor env k = int32_literal (index_of_constructor env k)

let tag_of_constructor' env k = located (tag_of_constructor env) k

(** [program env p] turns an Hopix program into an equivalent
    Hobix program. *)
let rec program env p =
  let env, defs = Util.ExtStd.List.foldmap definition' env p in
  (List.flatten defs, env)

(** Compilation of Hopix toplevel definitions. *)
and definition' env p = located (definition env) p

and definition env = Ast.(function
  | Hopix.Ast.DeclareExtern (x, _) ->
      env, [DeclareExtern (located identifier x)]

  | Hopix.Ast.DefineValue (x, e) ->
      env, [DefineValue (located identifier x, located (expression env) e)]

  | Hopix.Ast.DefineRecFuns recs ->
      env, [DefineRecFuns (List.map (function_definition env) recs)]

  | Hopix.Ast.DefineType (_, _, tydef) ->
      type_definition env tydef, []
)

and value_definition env (x, e) =
  (located identifier x, located (expression env) e)

(* TODO: Handle all patterns, not only PVariable.  *)
and function_definition' env (Hopix.Ast.FunctionDefinition(_, ps, e)) =
  let xs = List.map (located pattern_as_identifier) ps in
  Ast.Fun (xs, located (expression env) e)

and function_definition env (f, { Util.Position.value = fdef }) =
  (located identifier f, function_definition' env fdef)

and pattern_as_identifier = function
  | Hopix.Ast.PVariable x -> located identifier x
  | Hopix.Ast.PTypeAnnotation (p, _) ->
      pattern_as_identifier (Util.Position.value p)
  | _ -> assert false (* By syntax. *)

and identifier' id = located identifier id

and identifier (Hopix.Ast.Id x) =
  Ast.Id x

and expression' env e = located (expression env) e

(** Compilation of Hopix expressions. *)
and expression env = Ast.(function
  | Hopix.Ast.Variable x ->
      Variable (located identifier x)

  | Hopix.Ast.Tagged (k, _, es) ->
      let fill_then_return_block env b k es =
        let tag_block env b k =
          write_block b 0 (tag_of_constructor' env k)
        in
        let fill_cell env b i e = write_block b (i + 1) (expression' env e) in
        seqs (tag_block env b k :: List.mapi (fill_cell env b) es @ [b])
      in
      def
        (AllocateBlock (int_literal (List.length es + 1)))
        (fun b -> fill_then_return_block env (Variable b) k es)

  (* If the pattern matching fails, we make the program crash...  *)
  | Hopix.Ast.Case (e, branches) ->
      let branch scrutinee b next = branch env (Variable scrutinee) next b in
      let branches = located' expands_or_patterns branches in
      def
        (expression' env e)
        (fun scrutinee -> List.fold_right (branch scrutinee) branches crash)

  | Hopix.Ast.Ref e ->
      let init_then_return_ref env r e =
        seqs [write_block r 0 (expression' env e); r]
      in
      def
        (AllocateBlock (int_literal 1))
        (fun r -> init_then_return_ref env (Variable r) e)

  | Hopix.Ast.Read r ->
      read_block (located (expression env) r) 0

  | Hopix.Ast.Write (r, v) ->
      WriteBlock (located (expression env) r,
                  Literal (LInt Int32.zero),
                  located (expression env) v)

  | Hopix.Ast.While (c, b) ->
      Ast.While (located (expression env) c,
                      located (expression env) b)

  | Hopix.Ast.Apply (e1, _, es) ->
      Apply (located (expression env) e1,
             List.map (located (expression env)) es)

  | Hopix.Ast.Literal l ->
      Literal (located literal l)

  | Hopix.Ast.Define (x, e1, e2) ->
      Define (located identifier x,
              located (expression env) e1,
              located (expression env) e2)

  | Hopix.Ast.DefineRec (recs, e) ->
      DefineRec (List.map (function_definition env) recs,
                 located (expression env) e)

  | Hopix.Ast.TypeAnnotation (e, _) ->
      located (expression env) e

  | Hopix.Ast.If (conditions, final) ->
      let final = match final with
        | None -> Ast.(Variable (Id "nothing"))
        | Some e -> located (expression env) e
      in
      List.fold_left (fun t (cond, thenb) ->
        Ast.IfThenElse (located (expression env) cond,
                             located (expression env) thenb,
                             t)
      ) final (List.rev conditions)

  | Hopix.Ast.Fun fdef -> function_definition' env fdef
)

and branch env scrutinee next (Hopix.Ast.Branch (p, e)) =
  let cond, defs = pattern' env scrutinee p in
  Ast.IfThenElse (cond, defines defs (expression' env e), next)

and expand_pattern = Hopix.Ast.(Util.ListMonad.(function
  | PTypeAnnotation (p, _) -> located expand_pattern p
  | PWildcard | PLiteral _ | PVariable _ as p -> return p
  | PTaggedValue (k, ps) ->
      expand_patterns' ps (fun ps -> PTaggedValue (k, ps))
  | PAnd ps -> expand_patterns' ps (fun ps -> PAnd ps)
  | POr ps ->
      located' pick ps >>= fun p ->
      return p
))

and expand_patterns' ps wrap = Util.ListMonad.(
  located' expand_patterns ps >>= fun ps ->
  return (wrap (List.map Util.Position.unknown_pos ps))
)

and expand_patterns ps = Util.ListMonad.(
  List.fold_right (fun p macc ->
    expand_pattern p >>= fun p ->
    macc >>= fun acc ->
    return (p :: acc)
  )
    ps (return [])
)

(** [expands_or_patterns branches] returns a sequence of branches
    equivalent to [branches] except that their patterns do not contain
    any disjunction. {!ListMonad} can be useful to implement this
    transformation. *)
and expands_or_patterns branches =
  let expand_branch (Hopix.Ast.Branch (p, e)) =
    let ps = Util.ListMonad.run (located expand_pattern p) in
    List.map (fun p -> Hopix.Ast.Branch (Util.Position.unknown_pos p, e)) ps
  in
  List.flatten (List.map expand_branch branches)

and pattern' env scrutinee p = located (pattern env scrutinee) p

(** [pattern env scrutinee p] returns a boolean condition [c]
    and a list of definitions [ds] such that:

    - [c = true] if and only if [p] matches the [scrutinee] ;
    - [ds] binds all the variables that appear in [p].

*)
and pattern env scrutinee = Ast.(function
  | Hopix.Ast.PTypeAnnotation (p, _) -> located (pattern env scrutinee) p

  | Hopix.Ast.PVariable id -> (htrue, [(identifier' id, scrutinee)])

  | Hopix.Ast.PTaggedValue (k, ps) ->
      let conds, defs =
        List.split (
          List.mapi
            (fun i p -> pattern' env (read_block scrutinee (i + 1)) p)
            ps
        )
      in
      let same_tag =
        is_equal_int (read_block scrutinee 0) (tag_of_constructor' env k)
      in
      (conjs (same_tag :: conds), List.flatten defs)

  | Hopix.Ast.PWildcard -> (htrue, [])

  | Hopix.Ast.PLiteral l ->
      let l = literal' l in
      (is_equal l scrutinee (Literal l), [])

  | Hopix.Ast.POr _ -> assert false

  | Hopix.Ast.PAnd ps ->
      let conds, defs = List.split (List.map (pattern' env scrutinee) ps) in
      (conjs conds, List.flatten defs)
)

and literal' l = located literal l

and literal = Ast.(function
  | Hopix.Ast.LInt x -> LInt x
  | Hopix.Ast.LString s -> LString s
  | Hopix.Ast.LChar c -> LChar c
)

(** Compilation of type definitions. *)
and type_definition env t =
  match t with
  | Hopix.Ast.Abstract -> env
  | Hopix.Ast.DefineSumType l ->
      let r = ref Int32.zero in
      let incr_32 re = re := Int32.succ !re in
      let fIter ctags (k, _) =
        incr_32 r;
        ConstructorMap.add (Util.Position.value k) !r ctags
      in
      {constructor_tags = List.fold_left fIter env.constructor_tags l}

(** Here is the compiler! *)
let translate source env =
  program env source
