(** From Hopix to Hobix *)

module Source = Hopix
module Target = Hobix

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
    type t = HopixAST.constructor
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
  fun () -> incr r; HobixAST.Id ("_" ^ string_of_int !r)

(** [def w (fun x -> e)] returns an abstract syntax tree of
    the form:

    val x = w; e

    where [x] is chosen fresh.
*)
let def w f =
  let x = fresh_identifier () in
  HobixAST.Define (x, w, f x)

(** [defines [d1; ..; dN] e] returns an abstract syntax tree of
    the form:

    val d1;
    ..
    val dN;
    e

*)
let defines =
  List.fold_right (fun (x, xe) e -> HobixAST.Define (x, xe, e))

(** [seq s1 s2] is

    val _ = s1;
    s2

*)
let seq s1 s2 =
  HobixAST.Define (fresh_identifier (), s1, s2)

(** [htrue] represents the primitive true in Hobix. *)
let htrue =
  HobixAST.(Variable (Id "true"))

(** [seqs [s1; ...; sN] is

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
let is_equal e1 e2 =
  HobixAST.(Apply (Variable (Id "`="), [e1; e2]))

(** [conj e1 e2] is the boolean expression [e1 && e2]. *)
let conj e1 e2 =
  HobixAST.(Apply (Variable (Id "`&&"), [ e1; e2 ]))

(** [conjs [e1; ..; eN]] is the boolean expression [e1 && .. && eN]. *)
let rec conjs = HobixAST.(function
    | [] -> htrue
    | [c] -> c
    | c :: cs -> conj c (conjs cs)
  )

let int32_literal i = HobixAST.(Literal (LInt i))

let int_literal i = int32_literal (Int32.of_int i)

(** [read_block hobixe i] returns [hobixe[i]]. *)
let read_block hobixe i = HobixAST.ReadBlock (hobixe, int_literal i)

let write_block b i e = HobixAST.WriteBlock (b, int_literal i, e)

(** Typical expression whose evaluation lead the program to crash.  *)
let crash =
  (* We just build an expression that does a division by zero.  *)
  let zero = int_literal 0 in
  HobixAST.(Apply (Variable (Id "`/"), [zero; zero]))

let located  f x = f (Position.value x)

let located' f l = f (List.map Position.value l)

(** Build the Hobix expression corresponding to the tag of the given
    constructor.  *)
let tag_of_constructor env k = int32_literal (index_of_constructor env k)

let tag_of_constructor' env k = located (tag_of_constructor env) k

(** [program env p] turns an Hopix program into an equivalent
    Hobix program. *)
let rec program env p =
  let env, defs = ExtStd.List.foldmap definition' env p in
  (List.flatten defs, env)

(** Compilation of Hopix toplevel definitions. *)
and definition' env p = located (definition env) p

and definition env = HobixAST.(function
    | HopixAST.DeclareExtern (x, _) ->
      env, [DeclareExtern (located identifier x)]

    | HopixAST.DefineValue (x, e) ->
      env, [DefineValue (located identifier x, located (expression env) e)]

    | HopixAST.DefineRecFuns recs ->
      env, [DefineRecFuns (List.map (function_definition env) recs)]

    | HopixAST.DefineType (_, _, tydef) ->
      type_definition env tydef, []
  )

and value_definition env (x, e) =
  (located identifier x, located (expression env) e)

(* TODO: Handle all patterns, not only PVariable.  *)
and function_definition' env (HopixAST.FunctionDefinition(_, ps, e)) =
  let xs = List.map (located pattern_as_identifier) ps in
  HobixAST.Fun (xs, located (expression env) e)

and function_definition env (f, { Position.value = fdef }) =
  (located identifier f, function_definition' env fdef)

and pattern_as_identifier = function
  | HopixAST.PVariable x -> located identifier x
  | HopixAST.PTypeAnnotation (p, _) -> pattern_as_identifier (Position.value p)
  | _ -> assert false (* By syntax. *)

and identifier' id = located identifier id

and identifier (HopixAST.Id x) =
  HobixAST.Id x

and expression' env e = located (expression env) e

(** Compilation of Hopix expressions. *)
and expression env = HobixAST.(function
    | HopixAST.Variable x ->
      Variable (located identifier x)

    | HopixAST.Tagged (k, _, es) ->
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
    | HopixAST.Case (e, branches) ->
      let branch scrutinee b next = branch env (Variable scrutinee) next b in
      let branches = located' expands_or_patterns branches in
      def
        (expression' env e)
        (fun scrutinee -> List.fold_right (branch scrutinee) branches crash)

    | HopixAST.Ref e ->
      let x = fresh_identifier () in
      HobixAST.(Define (
          x,
          AllocateBlock (Literal (LInt (Int32.of_int 1))),
          WriteBlock (Variable x, Literal (LInt Int32.zero),
                      located (expression env) e))
        )

    | HopixAST.Read r ->
      read_block (located (expression env) r) 0

    | HopixAST.Write (r, v) ->
      WriteBlock (located (expression env) r,
                  Literal (LInt Int32.zero),
                  located (expression env) v)

    | HopixAST.While (c, b) ->
      HobixAST.While (located (expression env) c,
                      located (expression env) b)

    | HopixAST.Apply (e1, _, es) ->
      Apply (located (expression env) e1,
             List.map (located (expression env)) es)

    | HopixAST.Literal l ->
      Literal (located literal l)

    | HopixAST.Define (x, e1, e2) ->
      Define (located identifier x,
              located (expression env) e1,
              located (expression env) e2)

    | HopixAST.DefineRec (recs, e) ->
      DefineRec (List.map (function_definition env) recs,
                 located (expression env) e)

    | HopixAST.TypeAnnotation (e, ty) ->
      located (expression env) e

    | HopixAST.If (conditions, final) ->
      let final = match final with
        | None -> HobixAST.(Variable (Id "nothing"))
        | Some e -> located (expression env) e
      in
      List.fold_left (fun t (cond, thenb) ->
          HobixAST.IfThenElse (located (expression env) cond,
                               located (expression env) thenb,
                               t)
        ) final (List.rev conditions)

    | HopixAST.Fun fdef -> function_definition' env fdef
  )

and branch env scrutinee next (HopixAST.Branch (p, e)) =
  let cond, defs = pattern' env scrutinee p in
  HobixAST.IfThenElse (cond, defines defs (expression' env e), next)

and expand_pattern = HopixAST.(ListMonad.(function
    | PTypeAnnotation (p, _) -> located expand_pattern p
    | PWildcard | PLiteral _ | PVariable _ as p -> return p
    | PTaggedValue (k, ps) ->
      expand_patterns' ps (fun ps -> PTaggedValue (k, ps))
    | PAnd ps -> expand_patterns' ps (fun ps -> PAnd ps)
    | POr ps ->
      located' pick ps >>= fun p ->
      return p
  ))

and expand_patterns' ps wrap = ListMonad.(
    located' expand_patterns ps >>= fun ps ->
    return (wrap (List.map Position.unknown_pos ps))
  )

and expand_patterns = ListMonad.(function
    | [] -> return []
    | p :: ps ->
      expand_pattern p >>= fun p ->
      expand_patterns ps >>= fun ps ->
      return (p :: ps)
  )

(** [expands_or_patterns branches] returns a sequence of branches
    equivalent to [branches] except that their patterns do not contain
    any disjunction. {ListMonad} can be useful to implement this
    transformation. *)
and expands_or_patterns branches =
  let expand_branch (HopixAST.Branch (p, e)) =
    let ps = ListMonad.run (located expand_pattern p) in
    List.map (fun p -> HopixAST.Branch (Position.unknown_pos p, e)) ps
  in
  List.flatten (List.map expand_branch branches)

and pattern' env scrutinee p = located (pattern env scrutinee) p

(** [pattern env scrutinee p] returns a boolean condition [c]
    and a list of definitions [ds] such that:

    - [c = true] if and only if [p] matches the [scrutinee] ;
    - [ds] binds all the variables that appear in [p].

*)
and pattern env scrutinee = HobixAST.(function
    | HopixAST.PTypeAnnotation (p, _) -> located (pattern env scrutinee) p

    | HopixAST.PVariable id -> (htrue, [(identifier' id, scrutinee)])

    | HopixAST.PTaggedValue (k, ps) ->
      let conds, defs =
        List.split (
          List.mapi
            (fun i p -> pattern' env (read_block scrutinee (i + 1)) p)
            ps
        )
      in
      let same_tag =
        is_equal (read_block scrutinee 0) (tag_of_constructor' env k)
      in
      (conjs (same_tag :: conds), List.flatten defs)

    | HopixAST.PWildcard -> (htrue, [])

    | HopixAST.PLiteral l ->
      (* Pb: If scrutinee is not an int, is_equal is undefined...  *)
      (is_equal scrutinee (Literal (literal' l)), [])

    | HopixAST.POr ps -> assert false

    | HopixAST.PAnd ps ->
      let conds, defs = List.split (List.map (pattern' env scrutinee) ps) in
      (conjs conds, List.flatten defs)
  )

and literal' l = located literal l

and literal = HobixAST.(function
    | HopixAST.LInt x -> LInt x
    | HopixAST.LString s -> LString s
    | HopixAST.LChar c -> LChar c
  )

(** Compilation of type definitions. *)
and type_definition env t =
  match t with
  | HopixAST.Abstract -> env
  | HopixAST.DefineSumType l ->
    let r = ref Int32.zero in
    let incr_32 re = re := Int32.succ !re in
    let fIter ctags (k, _) =
      incr_32 r;
      ConstructorMap.add (Position.value k) !r ctags
    in
    {constructor_tags = List.fold_left fIter env.constructor_tags l}

(** Here is the compiler! *)
let translate source env =
  program env source
