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

  let rec program p = List.iter (fun item -> located definition item) p

  and definition pos = function
    | DefineValue (x, e) ->
      located expression e
    | DefineRecFuns recdefs ->
      List.iter (fun (_, fdef) -> located function_definition fdef) recdefs
    | _ -> ()

  and function_definition pos = function
    | FunctionDefinition (_, ps, e) ->
      List.iter (located pattern) ps;
      begin
        match (Position.value e) with
        | TypeAnnotation (e, _) -> located expression e
        | _ -> missing_type_annotation pos
      end

  and expression pos = function
    | Define (_, e1, e2) | Write (e1, e2) | While (e1, e2) ->
      located expression e1;
      located expression e2;
    | DefineRec (recdefs, e) ->
      List.iter (fun (_, fdef) -> located function_definition fdef) recdefs;
      located expression e
    | Apply (e, _, exprlist) ->
      located expression e;
      List.iter (fun expr -> located expression expr) exprlist;
    | If (eelist, optexpr) ->
      List.iter (fun (e1, e2) -> located expression e1; located expression e2) eelist;
      begin
        match optexpr with
        | Some expr -> located expression expr
        | None -> ()
      end
    | Fun fdef -> function_definition pos fdef
    | Tagged (_, _, exprlist) ->
      List.iter (fun e -> located expression e) exprlist;
    | Case (e, branchlist) ->
      located expression e;
      List.iter (fun br -> located branch br) branchlist;
    | TypeAnnotation (e, _) | Ref e | Read e ->
      located expression e
    | Literal _ | Variable _ -> ()

  and pattern pos = function
    | PTypeAnnotation ({ Position.value = (PWildcard | PVariable _) }, _) -> ()
    | PTypeAnnotation (p, _) ->
      located pattern p
    | PVariable _ | PWildcard | PLiteral _ -> ()
    | PTaggedValue (_, palist) | POr palist | PAnd palist ->
      List.iter (fun p -> located pattern p) palist

  and branch pos = function
    | Branch (p, e) ->
      located pattern p;
      located expression e
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
        Printf.sprintf "Type error:\nExpected:\n  %s\nGiven:\n  %s\n"
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
    (* Γ ⊢ e : σ    Γ(x : σ) ⊢ e' : σ'
       ———————————————————————————————
       Γ ⊢ val x = e; e' : σ'          *)
    | Define (x, e, e') ->
      let sigma = located (type_scheme_of_expression tenv) e in
      let tenv' = {tenv with values = (Position.value x, sigma)::tenv.values} in
      let sigma' = located (type_scheme_of_expression tenv') e' in sigma'

    (* Γ ⊢ e : σ'    σ' = σ
       ————————————————————
       Γ ⊢ (e : σ) : σ      *)
    | TypeAnnotation (e, ty) ->
      let sigma = located (type_scheme_of_expression tenv) e in
      let pty = type_of_monotype sigma in
      check_expected_type (Position.position ty) pty (aty_of_ty (Position.value ty));
      sigma

    | DefineRec (recdefs, e) ->
      failwith "Students! This is your job!"

    (* Γ ⊢ e : ∀α₁ … αn τ'₁⋆ … ⋆τ'm → τ     ∀i Γ ⊢ ei: τ'i[αi ↦ τi] … [αn↦ τn]
       ———————————————————————————————————————----------------------—————————————
       Γ ⊢ e[τ₁, …, τn] (e₁, …, em) : τ[α₁ ↦ τ₁] … [αn ↦ τn] *)
    | Apply (a, types, args) ->
      failwith "Students! This is your job!"

    (* With else branch:

       Γ ⊢ c : bool    ∀i Γ ⊢ cᵢ : bool
       Γ ⊢ e : σ    ∀i Γ ⊢ eᵢ : σ    Γ ⊢ e' : σ
       —————————————————————————————————————————
       Γ ⊢ if c then e
           elif c₁ then e₁
           …
           elif cₙ then eₙ
           else e'         : σ

       and without:

       Γ ⊢ c : bool    ∀i Γ ⊢ cᵢ : bool
       Γ ⊢ e : unit    ∀i Γ ⊢ eᵢ : unit
       ————————————————————————————————
       Γ ⊢ if c then e
           elif c₁ then e₁
           …
           elif cₙ then eₙ : unit       *)
    | If (eelist, expr) ->
      let elsety =
        match expr with
        | Some e -> type_of_monotype(located (type_scheme_of_expression tenv) e)
        | None -> hunit
      in
      let f (e1, e2) =
        (
          let e1ty = type_of_monotype(located (type_scheme_of_expression tenv) e1) in
          check_expected_type (Position.position e1) e1ty hbool;
          let e2ty = type_of_monotype(located (type_scheme_of_expression tenv) e2) in
          check_expected_type (Position.position e2) e2ty elsety;
        )
      in
      List.iter f eelist; monotype elsety

    (* Γ₀ = Γ(α₁ … αₖ)    ∀i Γᵢ₋₁ ⊢ pᵢ ⇒ Γᵢ, τᵢ    Γₙ ⊢ e : τ
       ———————————————————————————————————————————————————————
       Γ ⊢ \[α₁ … αₖ](p₁, …, pₙ) => e : ∀α₁ … αₖ.τ₁⋆ … ⋆τₙ → τ *)
    | Fun fdef ->
      failwith "Students! This is your job!"

    (* (K : ∀α₁ … αₖ.τ₁'⋆ … ⋆τₙ' → τ) ∈ Γ
       ∀i Γ ⊢ eᵢ : τᵢ'[α₁ ↦ τ₁] … [αₖ ↦ τₖ]
       ————————————————————————————————————————————————————
       Γ ⊢ K[τ₁, …, τₖ](e₁, …, eₙ) : τ[α₁ ↦ τ₁] … [αₖ ↦ τₖ] *)
    | Tagged ({ Position.value = (KId x) as k }, types, args) ->
      failwith "Students! This is your job!"

    (* Γ ⊢ e : σ'    ∀i Γ ⊢ pᵢ ⇒ Γᵢ, σ'    ∀i Γᵢ ⊢ eᵢ : σ
       ——————————————————————————————————————————————————
       Γ ⊢ e ? p₁ => e₁ | … | pₙ => eₙ : σ                *)
    | Case (e, bs) ->
      failwith "Students! This is your job!"

    (* Γ ⊢ e : τ
       ——————————————————
       Γ ⊢ ref e : ref(τ) *)
    | Ref e ->
      let tau = located (type_scheme_of_expression tenv) e in
      monotype (href (type_of_monotype tau))

    (* Γ ⊢ e : ref(τ)
       ——————————————
       Γ ⊢ !e : τ     *)
    | Read e ->
      let oneRef = located (type_scheme_of_expression tenv) e in
      monotype (type_of_reference_type (type_of_monotype oneRef))

    (* Γ ⊢ e : ref(τ)    Γ ⊢ e' : τ
       ————————————————————————————
       Γ ⊢ e := e' : unit           *)
    | Write (e, e') ->
      let oneRef = located (type_scheme_of_expression tenv) e in
      let tau = type_of_reference_type (type_of_monotype oneRef) in
      let tyToWrite = located (type_scheme_of_expression tenv) e' in
      check_expected_type (Position.position e') tau (type_of_monotype tyToWrite);
      monotype hunit

    (* Γ ⊢ e : bool    Γ ⊢ e' : unit
       —————————————————————————————
       Γ ⊢ while e { e' } : unit     *)
    | While (e, e') ->
      let expectBool = type_of_monotype(located (type_scheme_of_expression tenv) e) in
      let expectUnit = type_of_monotype(located (type_scheme_of_expression tenv) e') in
      check_expected_type (Position.position e) expectBool hbool;
      check_expected_type (Position.position e') expectUnit hunit;
      monotype hunit

    | Literal l ->
      mk_type_scheme(located type_of_literal l)

    (* (x : σ) ∈ Γ
       ———————————
       Γ ⊢ x : σ   *)
    | Variable x -> 
      located lookup_type_scheme_of_value x tenv

  (** [apply pos tenv s types args] computes the instanciation of the
      type scheme [s] with [types] and checks that the resulting type
      is an arrow. The input types of this arrow must correspond to
      the types of the expressions [args]. *)
  and apply pos tenv s types args =
    failwith "Students! This is your job!"

  and type_of_literal pos = function
    (*
       —————————
       ⊢ n : int *)
    | LInt _ -> hint

    (*
       ————————————
       ⊢ s : string *)
    | LString _ -> hstring

    (*
       ——————————
       ⊢ c : char *)
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
    (*
       ———————————————————————
       Γ ⊢ x : σ ⇒ Γ(x : σ), σ *)
    | PTypeAnnotation ({ Position.value = PVariable x }, ty) ->
      failwith "Students! This is your job!"

    | PVariable _ ->
      assert false (* by check_program_is_fully_annotated. *)

    (* (K : ∀α₁ … αₖ.τ₁⋆ … ⋆τₙ → τ) ∈ Γ    ∀i Γᵢ₋₁ ⊢ pᵢ ⇒ Γᵢ, τᵢ
       ——————————————————————————————————————————————————————————
       Γ₀ ⊢ K (p₁, …, pₙ) ⇒ Γₙ, τ                                 *)
    | PTaggedValue (k, ps) ->
      failwith "Students! This is your job!"

    (* | PTypeAnnotation ({ Position.value = PTaggedValue (k, ps) }, ty) -> *)
    (*   failwith "Students! This is your job!" *)

    (* ∀i Γ ⊢ pᵢ ⇒ Γᵢ, σᵢ    σ₁ = … = σₙ    Γ₁ = … = Γₙ
       ————————————————————————————————————————————————
       Γ ⊢ p₁ | … | pₙ ⇒ Γ₁, σ₁                         *)
    | POr ps ->
      failwith "Students! This is your job!"

    (* ∀i Γᵢ₋₁ ⊢ pᵢ ⇒ Γᵢ, σᵢ   σ₁ = … = σₙ
       ———————————————————————————————————
       Γ₀ ⊢ p₁ & … & pₙ ⇒ Γₙ, σₙ           *)
    | PAnd ps ->
      failwith "Students! This is your job!"

    (*
       ———————————————
       Γ ⊢ _ ⇒ Γ, ∀α.α *)
    | PWildcard ->
      let newAlpha = ATyVar(fresh ()) in
      tenv, (monotype newAlpha )

    (* Γ ⊢ p ⇒ Γ', σ'    σ' = σ
       ————————————————————————
       Γ ⊢ p : σ ⇒ Γ', σ        *)
    | PTypeAnnotation (p, ty) ->
      let tenv', tau' = pattern (Position.position p) tenv (Position.value p) in
      let ty' = type_of_monotype tau' in
      let aty = aty_of_ty (Position.value ty) in
      check_expected_type (Position.position ty) ty' aty;
      tenv', monotype aty
      
    (*
       ——————————————    ———————————————    —————————————————
       Γ ⊢ n ⇒ Γ, int    Γ ⊢ n ⇒ Γ, char    Γ ⊢ n ⇒ Γ, string *)
    | PLiteral l -> tenv, monotype (located type_of_literal l)

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

let print_new_type_bindings = HopixTypes.print_new_type_bindings
