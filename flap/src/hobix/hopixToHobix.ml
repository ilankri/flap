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
    integers to constructor and label identifiers. Therefore, the
    compilation environment is composed of two maps representing these
    assignments. The environment is populated each time we cross a
    type definitions while it is read each time we translate language
    constructions related to record and tagged values.
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
  ConstructorMap.find k env.constructor_tags

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

(** [read_block hobixe i] returns [hobixe[i]]. *)
let read_block hobixe i =
  HobixAST.(ReadBlock (hobixe, Literal (LInt (Int32.of_int i))))

let located  f x = f (Position.value x)
let located' f x = Position.map f x

(** [program env p] turns an Hopix program into an equivalent
    Hobix program. *)
let rec program env p =
  let env, defs = ExtStd.List.foldmap definition' env p in
  (List.flatten defs, env)

(** Compilation of Hopix toplevel definitions. *)
and definition' env p =
  definition env (Position.value p)

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

and function_definition env (f, { Position.value = HopixAST.FunctionDefinition (_, ps, e) }) =
  let xs = List.map (located pattern_as_identifier) ps in
  (located identifier f, HobixAST.Fun (xs, located (expression env) e))

and pattern_as_identifier = function
  | HopixAST.PVariable x -> located identifier x
  | _ -> assert false (* By syntax. *)

and identifier (HopixAST.Id x) =
  HobixAST.Id x

(** Compilation of Hopix expressions. *)
and expression env = HobixAST.(function
  | HopixAST.Variable x ->
    Variable (located identifier x)

  | HopixAST.Tagged (k, _, es) ->
    failwith "Students! This is your job!"

  | HopixAST.Case (e, bs) ->
    failwith "Students! This is your job!"

  | HopixAST.Ref e ->
    let x = fresh_identifier () in
    HobixAST.(Define (
      x,
      AllocateBlock (Literal (LInt (Int32.of_int 1))),
      WriteBlock (Variable x, Literal (LInt Int32.zero),
                  located (expression env) e))
    )

  | HopixAST.Read r ->
    ReadBlock (located (expression env) r, Literal (LInt Int32.zero))

  | HopixAST.Write (r, v) ->
    WriteBlock (located (expression env) r,
                Literal (LInt Int32.zero),
                located (expression env) v)

  | HopixAST.While (c, b) ->
    HobixAST.While (located (expression env) c,
                    located (expression env) b)

  (* </corrige> *)

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

  | HopixAST.Fun (HopixAST.FunctionDefinition (_, ps, e)) ->
    failwith "Students! This is your job!"
)


(** [expands_or_patterns branches] returns a sequence of branches
    equivalent to [branches] except that their patterns do not contain
    any disjunction. {ListMonad} can be useful to implement this
    transformation. *)
and expands_or_patterns branches =
 failwith "Students! This is your job!"


(** [pattern env scrutinee p] returns a boolean condition [c]
    and a list of definitions [ds] such that:

    - [c = true] if and only if [p] matches the [scrutinee] ;
    - [ds] binds all the variables that appear in [p].

*)
and pattern env scrutinee p = HobixAST.(
    failwith "Students! This is your job!"
)

and literal = HobixAST.(function
  | HopixAST.LInt x -> LInt x
  | HopixAST.LString s -> LString s
  | HopixAST.LChar c -> LChar c
)

(** Compilation of type definitions. *)
and type_definition env t =
  failwith "Students! This is your job!"

(** Here is the compiler! *)
let translate source env =
  program env source
