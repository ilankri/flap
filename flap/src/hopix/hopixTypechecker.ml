(** This module implements a type checker for Hopix. *)
open HopixAST
open HopixTypes

let initial_typing_environment = initial_typing_environment

type typing_environment = HopixTypes.typing_environment

let located f x = f (Position.position x) (Position.value x)

let rec vars_of_pattern' p = vars_of_pattern (Position.value p)

and vars_of_pattern = function
  | PTypeAnnotation (p, _) -> vars_of_pattern' p
  | PVariable x -> [Position.value x]
  | PTaggedValue (_, ps) | PAnd ps ->
    List.fold_left (fun acc p -> vars_of_pattern' p @ acc) [] ps
  | POr (p :: _) -> vars_of_pattern' p
  | POr [] -> assert false      (* By syntax.  *)
  | PWildcard | PLiteral _ -> []

let check_linear_pattern pos p =
  try
    let id = ExtStd.List.find_duplicate (vars_of_pattern p) in
    raise_type_error (NonLinearPattern (pos, id))
  with
  | Not_found -> ()

let instantiate_type_scheme pos s ts =
  try instantiate_type_scheme s ts with
  | InvalidInstantiation (xarity, iarity) ->
    raise_type_error (InvalidTypeInstantiation (pos, xarity, iarity))

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
    raise_type_error (MissingTypeAnnotation pos)
  in
  program ast

(** [typecheck tenv ast] checks that [ast] is a well-formed program
    under the typing environment [tenv]. *)
let typecheck tenv ast : typing_environment =

  let rec program p =
    List.fold_left (fun env x -> located (definition env) x) tenv p

  and rec_definition tenv recdefs =
    let fids, fdefs = List.split recdefs in
    let ftys = List.map (located (extract_function_type_scheme tenv)) fdefs in
    let fids = List.map Position.value fids in
    let tenv =
      try
        List.fold_left2 (fun tenv fid fty ->
            bind_value fid fty tenv
          ) tenv fids ftys
      with
      | Invalid_argument _ -> assert false (* By syntax.  *)
    in
    List.iter (fun fdef ->
        ignore (located (check_function_definition tenv) fdef)) fdefs;
    tenv

  and definition tenv pos = function
    | DefineValue (x, e) ->
      bind_value (Position.value x) (type_scheme_of_expression' tenv e) tenv

    | DefineRecFuns recdefs -> rec_definition tenv recdefs

    | DefineType (t, ts, tdef) ->
      let tyListNoPosition = List.map Position.value ts in
      bind_type_definition (Position.value t) tyListNoPosition tdef tenv

    | DeclareExtern (x, ty) ->
      let x = Position.value x in
      let ty = internalize_ty tenv ty in
      bind_value x (mk_type_scheme ty) tenv

  (** [extract_function_type_scheme tenv pos fdef] constructs a type
      scheme from the user type annotations found in the function
      definition [fdef]. This function does not check that the function
      definition actually has the type scheme written by the programmer. *)
  and extract_function_type_scheme tenv pos (FunctionDefinition (ts, ps, e)) =
    let extract_arg_ty tenv arg = located (type_of_pattern tenv) arg in
    let tenv =
      let tvs = List.map Position.value ts in
      bind_type_variables pos tenv tvs
    in
    let arg_tys = List.map (extract_arg_ty tenv) ps in
    let ret_ty =
      match Position.value e with
      | TypeAnnotation (_, ty) -> internalize_ty tenv ty
      | _ -> assert false    (* By check_program_is_fully_annotated.  *)
    in
    mk_type_scheme (ATyArrow (arg_tys, ret_ty))

  (** [check_function_definition tenv pos fdef] checks that the
      function definition [fdef] is well-typed with respect to the
      type annotations written by the programmer. We assume that
      [tenv] already contains the type scheme of the function [f]
      defined by [fdef] as well as all the functions which are
      mutually recursively defined with [f]. *)
  and check_function_definition tenv pos (FunctionDefinition (tvs, ps, e)) =
    let tenv = bind_type_variables pos tenv (List.map Position.value tvs) in
    let tenv, ts = patterns tenv ps in
    let Scheme (_, t) = type_scheme_of_expression' tenv e in
    mk_type_scheme (ATyArrow (ts, t))

  (** [check_expected_type pos xty ity] verifies that the expected
      type [xty] is syntactically equal to the inferred type [ity]
      and raises an error otherwise. *)
  and check_expected_type pos xty ity =
    if xty <> ity then raise_type_error (TypeMismatch (pos, xty, ity))

  (** [check_expression_monotype tenv xty e] checks if [e] has
      the monotype [xty] under the context [tenv]. *)
  and check_expression_monotype tenv xty e =
    let pos = Position.position e in
    let s = type_scheme_of_expression' tenv e in
    begin match s with
      | Scheme ([], ity) -> check_expected_type pos xty ity; s
      | _ -> raise_type_error (TooPolymorphicType pos)
    end

  and check_pattern_type pos xty ity =
    try ignore (unify_types xty ity) with
    | UnificationFailed (xty, ity) ->
      raise_type_error (TypeMismatch (pos, xty, ity))

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
      let sigma = type_scheme_of_expression' tenv e in
      let tenv' = bind_value (Position.value x) sigma tenv in
      let sigma' = type_scheme_of_expression' tenv' e' in
      sigma'

    (* Γ ⊢ e : σ'    σ' = σ
       ————————————————————
       Γ ⊢ (e : σ) : σ      *)
    | TypeAnnotation (e, ty) ->
      let Scheme (_, t) as s = type_scheme_of_expression' tenv e in
      check_expected_type pos (internalize_ty tenv ty) t;
      s

    | DefineRec (recdefs, e) ->
      type_scheme_of_expression' (rec_definition tenv recdefs) e

    (* Γ ⊢ e : ∀α₁ … αn τ'₁⋆ … ⋆τ'm → τ
       ∀i Γ ⊢ ei: τ'i[αi ↦ τi] … [αn↦ τn]
       —————————————————————————————————————————————————————
       Γ ⊢ e[τ₁, …, τn] (e₁, …, em) : τ[α₁ ↦ τ₁] … [αn ↦ τn] *)
    | Apply (a, types, args) ->
      let t =
        let pos = Position.position a in
        apply pos tenv (type_scheme_of_expression' tenv a) types args
      in
      mk_type_scheme (output_type_of_function t)

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
      let Scheme (_, else_ty) as else_sc =
        match expr with
        | Some e -> type_scheme_of_expression' tenv e
        | None -> monotype hunit
      in
      let check_branch (cond, expr) =
        let Scheme (_, t) = type_scheme_of_expression' tenv expr in
        ignore (check_expression_monotype tenv hbool cond);
        check_expected_type (Position.position expr) else_ty t
      in
      List.iter check_branch eelist;
      else_sc

    (* Γ₀ = Γ(α₁, …, αₖ)    ∀i Γᵢ₋₁ ⊢ pᵢ ⇒ Γᵢ, τᵢ    Γₙ ⊢ e : τ
       —————————————————————————————————————————————————————————
       Γ ⊢ \[α₁, …, αₖ](p₁, …, pₙ) => e : ∀α₁ … αₖ.τ₁⋆ … ⋆τₙ → τ *)
    | Fun fdef -> check_function_definition tenv pos fdef

    (* (K : ∀α₁ … α.τ₁'⋆ … αk⋆τk' → τ) ∈ Γ
       ∀i Γ ⊢ eᵢ : τᵢ'[α₁ ↦ τ₁] … [αk ↦ τk]
       ————————————————————————————————————————————————————
       Γ ⊢ K[τ₁, …, τn] (e₁, …, e) : τ[α₁ ↦ τ₁] … [αk ↦ τk] *)
    | Tagged (k, types, args) ->
      let tyFromK = located lookup_type_scheme_of_constructor k tenv in
      let rmPosTypes = List.map (fun a -> internalize_ty tenv a) types in
      let tau = instantiate_type_scheme pos tyFromK rmPosTypes in
      let tyListFromTau =
        match tau with
        | ATyArrow (alist, _) -> alist
        | _ -> assert false
      in
      let check_args () =
        try
          List.iter2 (fun t e ->
              let Scheme (_, atyFromE) = type_scheme_of_expression' tenv e in
              check_expected_type
                (Position.position e) atyFromE t
            ) tyListFromTau args
        with
        | Invalid_argument _ ->
          let xarity = List.length tyListFromTau in
          let iarity = List.length args in
          let k = Position.value k in
          raise_type_error (WrongArityDataCons (pos, k, xarity, iarity))
      in
      check_args ();
      monotype (output_type_of_function tau)

    (* Γ ⊢ e : σ'    ∀i Γ ⊢ pᵢ ⇒ Γᵢ, σ'    ∀i Γᵢ ⊢ eᵢ : σ
       ——————————————————————————————————————————————————
       Γ ⊢ e ? p₁ => e₁ | … | pn => en : σ                *)
    | Case (e, bs) ->
      let Scheme (_, t') as sigma' = type_scheme_of_expression' tenv e in
      mk_type_scheme (branches tenv t' bs)

    (* Γ ⊢ e : σ
       ——————————————————
       Γ ⊢ ref e : ref(σ) *)
    | Ref e ->
      let Scheme (_, t) = type_scheme_of_expression' tenv e in
      mk_type_scheme (href t)

    (* Γ ⊢ e : ref(σ)
       ——————————————
       Γ ⊢ !e : σ     *)
    | Read e ->
      let Scheme (_, oneRef) = type_scheme_of_expression' tenv e in
      mk_type_scheme (type_of_reference_type oneRef)

    (* Γ ⊢ e : ref(σ)    Γ ⊢ e' : τ
       ————————————————————————————
       Γ ⊢ e := e' : unit           *)
    | Write (e, e') ->
      let Scheme (_, oneRef) = type_scheme_of_expression' tenv e in
      let tau = type_of_reference_type oneRef in
      let Scheme (_, tyToWrite) = type_scheme_of_expression' tenv e' in
      check_expected_type (Position.position e') tau tyToWrite;
      monotype hunit

    (* Γ ⊢ e : bool    Γ ⊢ e' : unit
       —————————————————————————————
       Γ ⊢ while e { e' } : unit     *)
    | While (e, e') ->
      ignore (check_expression_monotype tenv hbool e);
      check_expression_monotype tenv hunit e'

    | Literal l -> monotype (located type_of_literal l)

    (* (x : σ) ∈ Γ
       ———————————
       Γ ⊢ x : σ   *)
    | Variable x -> located lookup_type_scheme_of_value x tenv

  (** [apply pos tenv s types args] computes the instanciation of the
      type scheme [s] with [types] and checks that the resulting type
      is an arrow. The input types of this arrow must correspond to
      the types of the expressions [args]. *)
  and apply pos tenv s types args =
    let t =
      let atypes = List.map (internalize_ty tenv) types in
      instantiate_type_scheme pos s atypes
    in
    match t with
    | ATyArrow (xtypes, _) ->
      let check_args () =
        try
          List.iter2 (fun xty arg ->
              let pos = Position.position arg in
              let Scheme (_, ity) = type_scheme_of_expression' tenv arg in
              check_expected_type pos xty ity;
            ) xtypes args
        with
        | Invalid_argument _ ->
          let xargc = List.length xtypes in
          let iargc = List.length args in
          raise_type_error (WrongArityFunction (pos, xargc, iargc))
      in
      check_args ();
      t
    | ATyVar _ | ATyCon _ -> raise_type_error (InvalidApplication pos)

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
  and pattern tenv pos p =
    check_linear_pattern pos p;
    pattern' tenv pos p

  and pattern' tenv pos = function
    (*
       ———————————————————————
       Γ ⊢ x : τ ⇒ Γ'(x : τ), τ *)
    | PTypeAnnotation ({ Position.value = PVariable x }, ty) ->
      let aty = internalize_ty tenv ty in
      let tenv' = bind_value (Position.value x) (monotype aty) tenv in
      tenv', aty

    (*
       ————————————————
       Γ ⊢ _ : τ ⇒ Γ, τ *)
    | PTypeAnnotation ({ Position.value = PWildcard }, ty) ->
      (tenv, internalize_ty tenv ty)

    | PWildcard | PVariable _ ->
      assert false (* By check_program_is_fully_annotated.  *)

    (* (K : ∀α₁ … αm.τ₁⋆ … ⋆τn → τ) ∈ Γ    ∀i Γᵢ₋₁ ⊢ pᵢ ⇒ Γᵢ, τᵢ
       ——————————————————————————————————————————————————————————
       Γ₀ ⊢ K (p₁, …, pn) ⇒ Γn, τ

       Note that 'α₁ … αm' do nothing *)
    | PTaggedValue (k, ps) as p ->
      let (Scheme (_, tau)) as atyScheme =
        located lookup_type_scheme_of_constructor k tenv
      in
      let tenv', ts =
        List.fold_right (fun p (tenv, ts) ->
            let tenv, t = located (pattern tenv) p in
            (tenv, t :: ts)
          ) ps (tenv, [])
      in
      let Scheme (_, tau) = refresh_type_scheme atyScheme in
      let subst =
        try unify_types tau (ATyArrow (ts, ATyVar (fresh ()))) with
        | UnificationFailed (xty, ity) ->
          raise_type_error (TypeMismatch (pos, xty, ity))
      in
      let tau = substitute subst tau in
      let tausInK = input_types_of_function tau in
      tenv', output_type_of_function tau

    (* ∀i Γ ⊢ pᵢ ⇒ Γᵢ, τᵢ    τ₁ = … = τn    Γ₁ = … = Γn
       ————————————————————————————————————————————————
       Γ ⊢ p₁ | … | pn ⇒ Γ₁, τ₁
       A verifier avec Idir... difference avec PAnd?
    *)
    | POr ps ->
      let checkEachPEqual (prevEnv, prevAty) p =
        (
          let gammai, pAty = located (pattern tenv) p in
          let prevEnv = match prevEnv with
            | Some x ->
              if (x<>gammai) then
                raise_type_error (POrError (Position.position p));
              Some gammai
            | None -> Some gammai in
          match prevAty with
          | Some x ->
            check_pattern_type (Position.position p) pAty x;
            (prevEnv, Some pAty)
          | None -> (prevEnv, Some pAty)
        ) in
      let gamma, sigma = List.fold_left checkEachPEqual (None, None) ps in
      begin
        match gamma, sigma with
        | Some x, Some y -> x, y
        | _ -> assert false (** Never reached case *)
      end

    (* ∀i Γᵢ₋₁ ⊢ pᵢ ⇒ Γᵢ, τᵢ   τ₁ = … = τn
       ———————————————————————————————————
       Γ₀ ⊢ p₁ & … & pn ⇒ Γn, τn           *)
    | PAnd ps as p ->
      let checkEachPEqual (prevEnv, prevAty) p =
        let e, pAty = located (pattern prevEnv) p in
        match prevAty with
        | Some x ->
          check_pattern_type (Position.position p) pAty x;
          (e, Some pAty)
        | None -> (e, Some pAty)
      in
      let gammaN, sigmaN = List.fold_left checkEachPEqual (tenv, None) ps in
      begin
        match sigmaN with
        | Some x -> gammaN, x
        | None -> assert false (** Never reached case *)
      end

    (* Γ ⊢ p ⇒ Γ', τ'    τ' = τ
       ————————————————————————
       Γ ⊢ p : τ ⇒ Γ', τ        *)
    | PTypeAnnotation (p, ty) ->
      let tenv', tau' = located (pattern tenv) p in
      let aty = internalize_ty tenv ty in
      check_pattern_type pos aty tau';
      tenv', aty

    (*
       ——————————————    ———————————————    —————————————————
       Γ ⊢ n ⇒ Γ, int    Γ ⊢ n ⇒ Γ, char    Γ ⊢ n ⇒ Γ, string *)
    | PLiteral l -> tenv, located type_of_literal l

  (** [branches tenv sty bs] checks that the patterns of the
      branches [bs] have type [sty] and that the bodies of these
      branches all have the same type. *)
  and branches tenv sty bs =
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
      let Scheme (_, atyE) = type_scheme_of_expression' envP e in
      check_expected_type (Position.position p) tyP sty;
      match oty with
      | Some x ->
        check_expected_type (Position.position e) x atyE;
        Some atyE
      | None -> Some atyE

  in
  try
    check_program_is_fully_annotated ast;
    program ast
  with
  | TypeError err -> report_error err


let print_typing_environment = print_typing_environment
