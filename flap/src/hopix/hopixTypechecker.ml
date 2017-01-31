(** This module implements a type checker for Hopix. *)
open HopixAST
open HopixTypes
open HopixPrettyPrinter

let initial_typing_environment = HopixTypes.initial_typing_environment

type typing_environment = HopixTypes.typing_environment

let type_error = Error.error "typechecking"

let located f x = f (Position.position x) (Position.value x)

(** [check_program_is_fully_annotated ast] traverses the [ast] of the
    program and for each definition, it makes sure that bound variables
    have a declared types and that recursive functions also have a type
    annotation to declare their return type. *)
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
    | Fun fdef ->
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
    | PTypeAnnotation ({ Position.value = PVariable _ }, _) ->
      failwith "Students! This is your job!"
    | PTypeAnnotation (p, _) ->
      failwith "Students! This is your job!"
    | PVariable _ ->
      failwith "Students! This is your job!"
    | PTaggedValue (_, ps) | POr ps | PAnd ps ->
      failwith "Students! This is your job!"
    | PWildcard | PLiteral _ ->
      failwith "Students! This is your job!"
  and branch pos = function
    | Branch (p, e) ->
      failwith "Students! This is your job!"
  and missing_type_annotation pos =
    Error.error "typechecking" pos "A type annotation is missing."
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
	located (type_of_expression tenv) e
      ) tenv
    | DefineRecFuns recdefs ->
      failwith "Student! This is your job!"
    | _ ->
      failwith "Student! This is your job!"

  and function_definition pos tenv = function
    | FunctionDefinition (_, ps, e) ->
      failwith "Student! This is your job!"

  and type_of_expression pos tenv = function
    | Define (x, e1, e2) ->
      failwith "Student! This is your job!"
    | TypeAnnotation (e, ty) ->
      failwith "Student! This is your job!"
    | DefineRec (recdefs, e) ->
      failwith "Student! This is your job!"
    | Apply (a, types, args) ->
      failwith "Student! This is your job!"
    | If (cts, f) ->
      failwith "Student! This is your job!"
    | Fun fdef ->
      failwith "Student! This is your job!"
    | Tagged (_, _, es) ->
      failwith "Student! This is your job!"
    | Case (e, bs) ->
      failwith "Student! This is your job!"
    | Ref e ->
      failwith "Student! This is your job!"
    | Read e ->
      failwith "Student! This is your job!"
    | Write (e1, e2) ->
      failwith "Student! This is your job!"
    | While (e1, e2) ->
      failwith "Student! This is your job!"
    | Literal l ->
      failwith "Student! This is your job!"
    | Variable x ->
      failwith "Student! This is your job!"

  and type_of_literal pos = function
    | LInt _ -> hint
    | LString _ -> hstring
    | LChar _ -> hchar

  and pattern pos tenv = function
    | PTypeAnnotation ({ Position.value = PVariable _ }, _) ->
      failwith "Student! This is your job!"
    | PTypeAnnotation (p, _) ->
      failwith "Student! This is your job!"
    | PVariable _ ->
      assert false (* by check_program_is_fully_annotated. *)
    | PTaggedValue (_, ps) | POr ps | PAnd ps ->
      failwith "Student! This is your job!"
    | PWildcard ->
      failwith "Student! This is your job!"
    | PLiteral _ ->
      failwith "Student! This is your job!"

  and branch pos = function
    | Branch (p, e) ->
      failwith "Student! This is your job!"
  in
  program ast

let print_typing_environment =
  HopixTypes.print_typing_environment
