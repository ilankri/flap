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
    | DefineValue (x, e) -> located expression e
    | DefineRecFuns recdefs ->
      List.iter
        (fun (_, fdef) -> located (function_definition true) fdef) recdefs;
    | DefineType _ | DeclareExtern _ -> ()

  and function_definition need_return_type pos = function
    | FunctionDefinition (_, ps, e) ->
      List.iter (located pattern) ps;
      if need_return_type then
        match Position.value e with
        | TypeAnnotation (e, _) -> located expression e
        | Literal _ | Variable _ | Define _ | DefineRec _ | Apply _ | If _ |
          Fun _ | Tagged _ | Case _ | Ref _ | Read _ | Write _ | While _ ->
          missing_type_annotation pos
      else located expression e

  and expression pos = function
    | Define (_, e1, e2) | Write (e1, e2) | While (e1, e2) ->
      located expression e1;
      located expression e2;
    | DefineRec (recdefs, e) ->
      List.iter
        (fun (_, fdef) -> located (function_definition true) fdef) recdefs;
      located expression e
    | Apply (e, _, exprlist) ->
      located expression e;
      List.iter (fun expr -> located expression expr) exprlist;
    | If (eelist, optexpr) ->
      List.iter
        (fun (e1, e2) -> located expression e1; located expression e2)
        eelist;
      begin
        match optexpr with
        | Some expr -> located expression expr
        | None -> ()
      end
    | Fun fdef -> function_definition false pos fdef
    | Tagged (_, _, exprlist) ->
      List.iter (fun e -> located expression e) exprlist;
    | Case (e, branchlist) ->
      located expression e;
      List.iter (fun br -> located branch br) branchlist;
    | TypeAnnotation (e, _) | Ref e | Read e ->
      located expression e
    | Literal _ | Variable _ -> ()

  and pattern pos = function
    | PTypeAnnotation ({ Position.value = (PWildcard | PVariable _) }, _) |
      PLiteral _ -> ()
    | PTypeAnnotation (p, _) -> located pattern p
    | PVariable _ | PWildcard -> missing_type_annotation pos
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
    try List.fold_left (fun env x -> located (definition env) x) tenv p with
    | exn -> HopixTypes.report_error exn

  and rec_definition tenv recdefs =
    let fids, fdefs = List.split recdefs in
    let ftys = List.map (located (extract_function_type_scheme tenv)) fdefs in
    let fids = List.map Position.value fids in
    let tenv = List.fold_right2 bind_value fids ftys tenv in
    List.iter (located (check_function_definition tenv)) fdefs;
    tenv

  and definition tenv pos = function
    | DefineValue (x, e) ->
      bind_value (Position.value x) (
        located (type_scheme_of_expression tenv) e
      ) tenv

    | DefineRecFuns recdefs -> rec_definition tenv recdefs

    | DefineType (t, ts, tdef) ->
      let tyListNoPosition = List.map Position.value ts in
      bind_type_definition (Position.value t) tyListNoPosition tdef tenv

    | DeclareExtern (x, ty) ->
      let x = Position.value x in
      let ty = Position.located aty_of_ty ty in
      bind_value x (mk_type_scheme ty) tenv

  (** [extract_function_type_scheme tenv pos fdef] constructs a type
      scheme from the user type annotations found in the function
      definition [fdef]. This function does not check that the function
      definition actually has the type scheme written by the programmer. *)
  and extract_function_type_scheme tenv pos (FunctionDefinition (ts, ps, e)) =
    let extract_arg_ty tenv arg =
      type_of_monotype (located (type_of_pattern tenv) arg)
    in
    let arg_tys = List.map (extract_arg_ty tenv) ps in
    let ret_ty =
      match Position.value e with
      | TypeAnnotation (_, ty) -> aty_of_ty' ty
      | _ -> assert false    (* By check_program_is_fully_annotated.  *)
    in
    Scheme (List.map Position.value ts, ATyArrow (arg_tys, ret_ty))

  (** [check_function_definition tenv pos fdef] checks that the
      function definition [fdef] is well-typed with respect to the
      type annotations written by the programmer. We assume that
      [tenv] already contains the type scheme of the function [f]
      defined by [fdef] as well as all the functions which are
      mutually recursively defined with [f]. *)
  and check_function_definition tenv pos (FunctionDefinition (ts, ps, e)) =
    let tenv, _ = patterns tenv ps in
    ignore (located (type_scheme_of_expression tenv) e)

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

  and type_scheme_of_expression' tenv e =
    located (type_scheme_of_expression tenv) e

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
      check_expected_type (Position.position ty) (aty_of_ty (Position.value ty))
        pty;
      sigma

    | DefineRec (recdefs, e) ->
      located (type_scheme_of_expression (rec_definition tenv recdefs)) e

    (* Γ ⊢ e : ∀α₁ … αn τ'₁⋆ … ⋆τ'm → τ
       ∀i Γ ⊢ ei: τ'i[αi ↦ τi] … [αn↦ τn]
       —————————————————————————————————————————————————————
       Γ ⊢ e[τ₁, …, τn] (e₁, …, em) : τ[α₁ ↦ τ₁] … [αn ↦ τn] *)
    | Apply (a, types, args) ->
      let t =
        let a, pos = Position.destruct a in
        apply pos tenv (type_scheme_of_expression tenv pos a) types args
      in
      let t =
        match types with
        | [] -> type_of_monotype (check_expression_monotype tenv t a)
        | _ -> t
      in
      monotype (output_type_of_function t)

    (* With else branch:

       Γ ⊢ c : bool    ∀i Γ ⊢ cᵢ : bool
       Γ ⊢ e : σ    ∀i Γ ⊢ eᵢ : σ    Γ ⊢ e' : σ
       —————————————————————————————————————————
       Γ ⊢ if c then e
           elif c₁ then e₁
           …
           elif ci then ei
           else e'         : σ

       and without:

       Γ ⊢ c : bool    ∀i Γ ⊢ cᵢ : bool
       Γ ⊢ e : unit    ∀i Γ ⊢ eᵢ : unit
       ————————————————————————————————
       Γ ⊢ if c then e
           elif c₁ then e₁
           …
           elif ci then ei : unit       *)
    | If (eelist, expr) ->
      let elsety =
        match expr with
        | Some e -> type_of_monotype(located (type_scheme_of_expression tenv) e)
        | None -> hunit
      in
      let f (e1, e2) =
        (
          let e1ty =
            type_of_monotype(located (type_scheme_of_expression tenv) e1)
          in
          check_expected_type (Position.position e1) e1ty hbool;
          let e2ty =
            type_of_monotype(located (type_scheme_of_expression tenv) e2)
          in
          check_expected_type (Position.position e2) e2ty elsety;
        )
      in
      List.iter f eelist; monotype elsety

    (* Γ₀ = Γ(α₁, …, αₖ)    ∀i Γᵢ₋₁ ⊢ pᵢ ⇒ Γᵢ, τᵢ    Γₙ ⊢ e : τ
       —————————————————————————————————————————————————————————
       Γ ⊢ \[α₁, …, αₖ](p₁, …, pₙ) => e : ∀α₁ … αₖ.τ₁⋆ … ⋆τₙ → τ *)
    | Fun (FunctionDefinition (tvs, ps, e)) ->
      let tenv = bind_type_variables pos tenv (List.map Position.value tvs) in
      let tenv, ts =
        List.fold_right (fun p (tenv, ts) ->
            let p, pos = Position.destruct p in
            let tenv, t = pattern tenv pos p in
            let t = type_of_monotype t in
            check_well_formed_type pos tenv t;
            (tenv, t :: ts)
          ) ps (tenv, [])
      in
      let t = type_scheme_of_expression' tenv e in
      mk_type_scheme (ATyArrow (ts, type_of_monotype t))

    (* (K : ∀α₁ … α.τ₁'⋆ … αk⋆τk' → τ) ∈ Γ
       ∀i Γ ⊢ eᵢ : τᵢ'[α₁ ↦ τ₁] … [αk ↦ τk]
       ————————————————————————————————————————————————————
       Γ ⊢ K[τ₁, …, τn] (e₁, …, e) : τ[α₁ ↦ τ₁] … [αk ↦ τk] *)
    | Tagged ({ Position.value = (KId x) as k }, types, args) ->
      let tyFromK = lookup_type_scheme_of_constructor k tenv in
      let rmPosTypes = List.map (fun a -> (aty_of_ty' a)) types in
      let tau = instantiate_type_scheme tyFromK rmPosTypes in
      let tyListFromTau =
        (match tau with
         | ATyArrow(alist, _) -> alist
         | _ -> assert false)
      in
      let f t e =
        begin
          let atyFromE = located (type_scheme_of_expression tenv) e  in
          check_expected_type (Position.position e) (type_of_monotype atyFromE) t
        end in
      List.iter2 f tyListFromTau args;
      monotype (output_type_of_function tau)

    (* Γ ⊢ e : σ'    ∀i Γ ⊢ pᵢ ⇒ Γᵢ, σ'    ∀i Γᵢ ⊢ eᵢ : σ
       ——————————————————————————————————————————————————
       Γ ⊢ e ? p₁ => e₁ | … | pn => en : σ                *)
    | Case (e, bs) ->
      let sigma' = located (type_scheme_of_expression tenv) e in
      branches tenv sigma' None bs

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
      check_expected_type (Position.position e') tau
        (type_of_monotype tyToWrite);
      monotype hunit

    (* Γ ⊢ e : bool    Γ ⊢ e' : unit
       —————————————————————————————
       Γ ⊢ while e { e' } : unit     *)
    | While (e, e') ->
      let expectBool =
        type_of_monotype(located (type_scheme_of_expression tenv) e)
      in
      let expectUnit =
        type_of_monotype(located (type_scheme_of_expression tenv) e')
      in
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
    let t =
      let atypes = List.map (internalize_ty tenv) types in
      instantiate_type_scheme s atypes
    in
    match t with
    | ATyArrow (xtypes, _) ->
      List.iter2 (fun xty arg ->
          let arg, pos = Position.destruct arg in
          let ity = type_scheme_of_expression tenv pos arg in
          (* let ty = internalize_ty tenv ty in *)
          check_expected_type pos xty (type_of_monotype ity)
        ) xtypes args;
      t
    | ATyVar _ | ATyCon _ -> raise (InvalidApp pos)

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
       Γ ⊢ x : σ ⇒ Γ'(x : σ), σ *)
    | PTypeAnnotation ({ Position.value = PVariable x }, ty) ->
      let aty = monotype (aty_of_ty (Position.value ty)) in
      let tenv' = bind_value (Position.value x) aty tenv in
      tenv', aty

    | PWildcard | PVariable _ ->
      assert false (* By check_program_is_fully_annotated.  *)

    (* (K : ∀α₁ … αm.τ₁⋆ … ⋆τn → τ) ∈ Γ    ∀i Γᵢ₋₁ ⊢ pᵢ ⇒ Γᵢ, τᵢ
       ——————————————————————————————————————————————————————————
       Γ₀ ⊢ K (p₁, …, pn) ⇒ Γn, τ

       Note that 'α₁ … αm' do nothing *)
    | PTaggedValue (k, ps) ->
      let (Scheme (_, tau)) as atyScheme = lookup_type_scheme_of_constructor (Position.value k) tenv in
      let checkEachPMatch accu p tvInK =
        (
          let e, pAty = located (pattern accu) p in
          check_expected_type (Position.position p) (type_of_monotype pAty) tvInK;
          e
        ) in
      let tausInK = output_ty_list_of_function tau in
      let tenv' = List.fold_left2 checkEachPMatch tenv ps tausInK in
      tenv', monotype (output_type_of_function tau)

    (* ∀i Γ ⊢ pᵢ ⇒ Γᵢ, σᵢ    σ₁ = … = σn    Γ₁ = … = Γn
       ————————————————————————————————————————————————
       Γ ⊢ p₁ | … | pn ⇒ Γ₁, σ₁
       A verifier avec Idir... difference avec PAnd?
    *)
    | POr ps ->
      let checkEachPEqual (prevEnv, prevAty) p =
        (
          let gammai, pAty = located (pattern tenv) p in
          let prevEnv = match prevEnv with
            | Some x -> if (x<>gammai) then type_error (Position.position p) (Printf.sprintf "Pattern types mismatch!");Some gammai
            | None -> Some gammai in
          match prevAty with
          | Some x -> check_expected_type (Position.position p) (type_of_monotype pAty) (type_of_monotype x); (prevEnv, Some pAty)
          | None -> (prevEnv, Some pAty)
        ) in
      let gamma, sigma = List.fold_left checkEachPEqual (None, None) ps in
      begin
        match gamma, sigma with
        | Some x, Some y -> x, y
        | _ -> assert false (** Never reached case *)
      end
    (* ∀i Γᵢ₋₁ ⊢ pᵢ ⇒ Γᵢ, σᵢ   σ₁ = … = σn
       ———————————————————————————————————
       Γ₀ ⊢ p₁ & … & pn ⇒ Γn, σn           *)
    | PAnd ps ->
      let checkEachPEqual (prevEnv, prevAty) p =
        (
          let e, pAty = located (pattern prevEnv) p in
          match prevAty with
          | Some x -> check_expected_type (Position.position p) (type_of_monotype pAty) (type_of_monotype x); (e, Some pAty)
          | None -> (e, Some pAty)
        ) in
      let gammaN, sigmaN = List.fold_left checkEachPEqual (tenv, None) ps in
      begin
        match sigmaN with
        | Some x -> gammaN, x
        | None -> assert false (** Never reached case *)
      end

    (* Γ ⊢ p ⇒ Γ', σ'    σ' = σ
       ————————————————————————
       Γ ⊢ p : σ ⇒ Γ', σ        *)
    | PTypeAnnotation (p, ty) ->
      let tenv', tau' = located (pattern tenv) p in
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
      let envP, tyP = located (pattern tenv) p in
      let atyE = located (type_scheme_of_expression tenv) e in
      check_expected_type (Position.position p) (type_of_monotype tyP) (type_of_monotype sty);
      begin
        match oty with
        | Some x -> check_expected_type (Position.position e) (type_of_monotype x) (type_of_monotype atyE); Some atyE
        | None -> Some atyE
      end

  in
  program ast

let print_typing_environment =
  HopixTypes.print_typing_environment

let print_new_type_bindings = HopixTypes.print_new_type_bindings
