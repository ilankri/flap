(** This module implements a type checker for Hopix. *)
open HopixAST
open HopixTypes
open HopixPrettyPrinter

let initial_typing_environment = HopixTypes.initial_typing_environment

type typing_environment = HopixTypes.typing_environment

let type_error = HopixTypes.type_error

let located f x = f (Position.position x) (Position.value x)

(** [check_program_is_fully_annotated ast] traverses the [ast] of the
    program and for each definition, it makes sure that bound
    variables have a declared types and that recursive functions also
    have a type annotation to declare their return type. To simplify
    even further type checking, wildcard patterns must also be
    annotated by a type. *)
let check_program_is_fully_annotated ast =

  let rec program p = List.iter (located definition) p

  and definition pos = function
    | DefineValue (x, e) ->
      failwith "Students! This is your job!"
    | DefineRecFuns recdefs ->
      List.iter (fun (_, fdef) -> located function_definition fdef) recdefs
    | _ ->
      ()
  and function_definition pos = function
    | FunctionDefinition (_, ps, e) ->
      failwith "Students! This is your job!"

  and expression pos = function
    | Define (_, e1, e2) ->
	 failwith "Students! This is your job!"
    | DefineRec (recdefs, e) ->
      failwith "Students! This is your job!"
    | Apply (a, _, args) ->
      failwith "Students! This is your job!"
    | If (cts, f) ->
      failwith "Students! This is your job!"
    | Fun (FunctionDefinition (_, ps, e)) ->
      failwith "Students! This is your job!"
    | Tagged (_, _, es) ->
      failwith "Students! This is your job!"
    | Case (e, bs) ->
      failwith "Students! This is your job!"
    | TypeAnnotation (e, _) | Ref e | Read e ->
      failwith "Students! This is your job!"
    | Write (e1, e2) | While (e1, e2) ->
      failwith "Students! This is your job!"
    | Literal _ | Variable _ ->
      failwith "Students! This is your job!"
  and pattern pos = function
    | PTypeAnnotation ({ Position.value = (PWildcard | PVariable _) }, _) ->
      failwith "Students! This is your job!"
    | PTypeAnnotation (p, _) ->
      failwith "Students! This is your job!"
    | PVariable _ | PWildcard ->
      failwith "Students! This is your job!"
    | PTaggedValue (_, ps) | POr ps | PAnd ps ->
      failwith "Students! This is your job!"
    | PLiteral _ ->
      failwith "Students! This is your job!"
  and branch pos = function
    | Branch (p, e) ->
      failwith "Students! This is your job!"
  and missing_type_annotation pos =
    type_error pos "A type annotation is missing."
  in
  program ast

(** [typecheck tenv ast] checks that [ast] is a well-formed program
    under the typing environment [tenv]. *)
let typecheck tenv ast : typing_environment =

  check_program_is_fully_annotated ast;

  let rec program p =
    List.fold_left (fun env x -> located (definition env) x) tenv p

  and definition tenv pos = function
    | DefineValue (x, e) ->
      bind_value (Position.value x) (
	located (type_scheme_of_expression tenv) e
      ) tenv
    | DefineRecFuns recdefs ->
      failwith "Students! This is your job!"

    | DefineType (t, ts, tdef) ->
	 failwith "Students! This is your job!"

    | DeclareExtern (x, ty) ->
	 failwith "Students! This is your job!"

  (** [extract_function_type_scheme tenv pos fdef] constructs a type
      scheme from the user type annotations found in the function
      definition [fdef]. This function does not check that the function
      definition actually has the type scheme written by the programmer. *)
  and extract_function_type_scheme tenv pos (FunctionDefinition (ts, ps, e)) =
       failwith "Students! This is your job!"

  (** [check_function_definition tenv pos fdef] checks that the
      function definition [fdef] is well-typed with respect to the
      type annotations written by the programmer. We assume that
      [tenv] already contains the type scheme of the function [f]
      defined by [fdef] as well as all the functions which are
      mutually recursively defined with [f]. *)
  and check_function_definition tenv pos = function
    | FunctionDefinition (ts, ps, e) ->
      failwith "Students! This is your job!"

  (** [check_expected_type pos xty ity] verifies that the expected
      type [xty] is syntactically equal to the inferred type [ity]
      and raises an error otherwise. *)
  and check_expected_type pos xty ity =
    if xty <> ity then
      type_error pos (
	Printf.sprintf "Type error:\nExpected:%s\nGiven:%s\n"
	  (print_aty xty) (print_aty ity)
      )

  (** [check_expression_monotype tenv xty e] checks if [e] has
      the monotype [xty] under the context [tenv]. *)
  and check_expression_monotype tenv xty e =
    let pos = Position.position e in
    let s = located (type_scheme_of_expression tenv) e in
    begin match s with
      | Scheme ([], ity) -> check_expected_type pos xty ity; s
      | _ -> type_error pos (
	Printf.sprintf "The type of this expression is too polymorphic."
      )
    end

  (** [type_scheme_of_expression tenv pos e] computes a type scheme
      for [e] if it exists. Besides, this type scheme must be a
      monotype if no type instanciation is provided in the enclosing
      expression. *)
  and type_scheme_of_expression tenv pos = function
    | Define (x, e1, e2) ->
      failwith "Students! This is your job!"

    | TypeAnnotation (e, xty) ->
      failwith "Students! This is your job!"

    | DefineRec (recdefs, e) ->
      failwith "Students! This is your job!"

    | Apply (a, types, args) ->
      failwith "Students! This is your job!"

    | If (cts, f) ->
      failwith "Students! This is your job!"

    | Fun fdef ->
      failwith "Students! This is your job!"

    | Tagged ({ Position.value = (KId x) as k }, types, args) ->
      failwith "Students! This is your job!"

    | Case (e, bs) ->
      failwith "Students! This is your job!"

    | Ref e ->
      failwith "Students! This is your job!"

    | Read e ->
      failwith "Students! This is your job!"

    | Write (e1, e2) ->
      failwith "Students! This is your job!"

    | While (e1, e2) ->
      failwith "Students! This is your job!"

    | Literal l ->
      failwith "Students! This is your job!"

    | Variable ({ Position.value = (Id s) as x }) ->
      failwith "Students! This is your job!"

  (** [apply pos tenv s types args] computes the instanciation of the
      type scheme [s] with [types] and checks that the resulting type
      is an arrow. The input types of this arrow must correspond to
      the types of the expressions [args]. *)
  and apply pos tenv s types args =
       failwith "Students! This is your job!"

  and type_of_literal pos = function
    | LInt _ -> hint
    | LString _ -> hstring
    | LChar _ -> hchar

  and type_of_pattern pos tenv p =
    snd (pattern pos tenv p)

  and patterns tenv = function
    | [] ->
      tenv, []
    | p :: ps ->
      let tenv, ty = located (pattern tenv) p in
      let tenv, tys = patterns tenv ps in
      tenv, ty :: tys

  (** [pattern tenv pos p] computes a new environment completed with
      the variables introduced by the pattern [p] as well as the type
      of this pattern. *)
  and pattern tenv pos = function
    | PTypeAnnotation ({ Position.value = PVariable x }, ty) ->
      failwith "Students! This is your job!"

    | PVariable _ ->
      assert false (* by check_program_is_fully_annotated. *)

    | PTaggedValue (k, ps) ->
      failwith "Students! This is your job!"

    | PTypeAnnotation ({ Position.value = PTaggedValue (k, ps) }, ty) ->
      failwith "Students! This is your job!"

    | POr ps ->
      failwith "Students! This is your job!"

    | PAnd ps ->
      failwith "Students! This is your job!"



    | PWildcard ->
	 failwith "Students! This is your job!"

    | PTypeAnnotation (p, ty) ->
	 failwith "Students! This is your job!"

    | PLiteral l ->
	 failwith "Students! This is your job!"

  (** [branches tenv sty oty bs] checks that the patterns of the
      branches [bs] have type [sty] and that the bodies of these
      branches all have the same type oty. *)
  and branches tenv sty oty bs =
    let oty =
      List.fold_left (fun oty b ->
	located (branch tenv sty oty) b
      ) None bs
    in
    match oty with
      | None -> assert false (* By syntax. *)
      | Some oty -> oty

  and branch tenv sty oty pos = function
    | Branch (p, e) ->
	 failwith "Students! This is your job!"

  in
  program ast

let print_typing_environment =
  HopixTypes.print_typing_environment
