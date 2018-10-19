open PPrint
open PPrintCombinators
open PPrintEngine
open ExtPPrint
open HopixAST
open Position

let int i = string (Int32.to_string i)

let gtype_definition sep what ks =
    string "=" ++ group (
      separate_map (break 1 ^^ string sep ^^ break 1) what ks
    )

let rec program p =
  separate_map hardline (located definition) p

and definition = function
  | DefineValue (x, e) ->
    group (value_definition false (x, e))
  | DefineRecFuns rv ->
    rec_function_definition false rv
  | DeclareExtern (x, t) ->
    group (string "extern" ++ located identifier x
	   ++ string ":" ++ located ty t)
  | DefineType (t, ts, tdef) ->
    nest 2 (
      group (group (string "type"
		    ++ located type_constructor t
		    ++ group (type_parameters ts))
	     ++ group (type_definition tdef))
    )

and rec_function_definition paren rv =
  group (string "fun"
	 ^^ space
	 ^^ separate_map (hardline ^^ string "and" ^^ space)
	     (fun (x, d) ->
	       nest 2 (
		 group (located identifier x ^^ break 1 ^^
                        let sep = space ^^ string "=" ^^ break 1 in
			located (function_definition paren sep) d)))
	     rv)

and function_definition paren sep = function
  | FunctionDefinition (ts, ps, e) ->
    group (
      function_type_parameters ts
      ^^ parens (group (separate_map (comma ^^ break 1) (located pattern) ps))
      ^^ sep ^^ located (if_paren_expression paren) e)

and function_type_parameters = function
  | [] ->
    empty
  | ts ->
    group (
      brackets (separate_map (comma ^^ break 1) (located type_variable) ts) ^^ break 1
    )

and type_parameters = function
  | [] ->
    empty
  | ts ->
    parens (separate_map (comma ^^ break 1) (located type_variable) ts)

and type_definition = function
  | DefineSumType ks ->
    gtype_definition "|" dataconstructor_definition ks
  | Abstract ->
    empty

and label (LId s) =
  string s

and dataconstructor_definition (k, tys) =
  match tys with
    | [] ->
      located dataconstructor k
    | _ ->
      located dataconstructor k
      ++ parens (
	separate_map (break 1 ^^ string "," ^^ break 1) (located ty) tys
      )

and dataconstructor (KId k) =
  string k

and value_definition paren (x, e) =
  nest 2 (group (group (string "val" ++ located identifier x ++ string "=")
		 ++ group (located (if_paren_expression paren) e)))

and ty t = match t with
  | TyCon (tcon, []) ->
    type_constructor tcon
  | TyCon (tcon, tys) ->
    group (type_constructor tcon
	   ++ parens(
	     separate_map (string "," ^^ break 1) (located ty) tys
	   ))
  | TyVar tvar ->
    type_variable tvar
  | TyArrow (ins, out) ->
    separate_map
      (break 1 ^^ string "*" ^^ break 1) (located (mayparen_ty t)) ins
    ++ string "->" ++ located ty out

and mayparen_ty ctx a =
  match ctx, a with
    | TyArrow _, TyArrow _ ->
      parens (ty a)
    | _, _ ->
      ty a

and type_constructor (TCon s) =
  string s

and type_variable (TId x) =
  string x

and identifier (Id x) =
  string x

and expression = function
  | Literal l ->
    located literal l

  | Variable x ->
    located identifier x

  | TypeAnnotation (e, t) ->
    parens (located expression e ++ group (string ":" ++ located ty t))

  | Define (x, e1, e2) ->
    nest 2 (
      group (value_definition true (x, e1) ++ string ";"
    ))
    ^^ break 1 ^^ group (located expression e2)

  | DefineRec (vs, e) ->
    rec_function_definition true vs ++ string ";"
    ^^ break 1 ^^ group (located expression e)

  | Fun (fdef) ->
    (* /!\ rigorously, we should use (function_definition true)
       below, but this adds many parenthesis that are almost ever
       superfluous *)
    string "\\" ^^ function_definition false (space ^^ string "=>" ^^ break 1) fdef

  | Apply (a, ts, bs) ->
    group (
      located may_paren_expression a
      ^^ function_type_arguments ts
      ++ parens (separate_map (comma ^^ break 1) (located expression) bs)
    )

  | If ([], _) ->
    assert false (* By parsing. *)

  | If (g :: gs, e) ->
    group (string "if" ++ guarded_expression g)
    ^^ separate_map empty elif_guarded_expression gs
    ^^ else_expression e

  | Tagged (k, ts, []) ->
    located dataconstructor k ^^ function_type_arguments ts

  | Tagged (k, ts, es) ->
    group (
      located dataconstructor k
      ^^ function_type_arguments ts
      ++ parens (separate_map (comma ^^ break 1) (located expression) es)
    )

  | Case (e, bs) ->
    group (
      group (located may_paren_expression e ++ string "?")
      ++ group (
	string "{"
	++ separate_map (break 1) (located branch) bs
	++ string "}")
    )

  | Ref e ->
    string "ref" ++ located may_paren_expression e

  | Write (lhs, rhs) ->
    group (located may_paren_expression lhs
    ++ string ":="
    ++ located expression rhs)

  | Read e ->
    group (string "!" ++ located may_paren_expression e)

  | While (e, b) ->
    nest 2 (group (string "while" ++ located may_paren_expression e
		   ++ string "{" ^^ break 1
		   ++ located expression b
		   ++ break 1 ^^ string "}"))

and guarded_expression (c, t) =
  nest 2 (
    located expression c ++ string "then" ^^ break 1
    ^^ located may_paren_under_if t
  )

and elif_guarded_expression g =
  break 1 ^^ string "elif" ++ guarded_expression g

and else_expression = function
  | None -> empty
  | Some e -> break 1 ^^ string "else" ++ located may_paren_under_if e

and function_type_arguments = function
  | [] ->
    empty
  | ts ->
    brackets (separate_map (comma ^^ break 1) (located ty) ts)

and may_paren_under_if e = match e with
  | If _ ->
    parens (expression e)
  | _ ->
    expression e

and delimited = function
  | Variable _ | Literal _ | Apply _ | Tagged _
  | While _ | TypeAnnotation _ ->
    (* Delimited expressions, no need to put parenthesis around.
       Some inner sub-expressions may need parens, though *)
    true
  | Define _ | DefineRec _ | If _ | Fun _
  | Ref _ | Read _ | Write _ | Case _ ->
    false

and may_paren_expression e =
  if delimited e then expression e else parens (expression e)

and if_paren_expression b e =
  if b && not (delimited e) then parens (expression e) else expression e

and branch (Branch (p, e)) =
  group (nest 2 (group (string "|" ++ located pattern p ++ string "=>") 
		 ++ located expression e))

and patterns ps =
  parens (separate_map (comma ^^ break 1) (located pattern) ps)

and pattern = function
  | PWildcard ->
    string "_"
  | PVariable x ->
    located identifier x
  | PTypeAnnotation (p, t) ->
    located pattern p ++ string ":" ++ located ty t
  | PTaggedValue (k, []) ->
    located dataconstructor k
  | PTaggedValue (k, ps) ->
    located dataconstructor k
    ++ parens (separate_map (comma ^^ break 1) (located pattern) ps)
  | PAnd ps ->
    parens
      (separate_map (break 1 ^^ string "&" ^^ break 1) (located pattern) ps)
  | POr ps ->
    parens
      (separate_map (break 1 ^^ string "|" ^^ break 1) (located pattern) ps)
  | PLiteral l ->
    located literal l

and field_pattern (f, p) =
  located label f ++ string "=" ++ located pattern p

and literal = function
  | LInt x ->
    int x
  | LChar c ->
    char c
  | LString s ->
    string_literal s

and char c =
  group (string "'" ^^ string (Char.escaped c) ^^ string "'")

and string_literal s =
  group (string "\"" ^^ string (String.escaped s) ^^ string "\"")

let to_string f x =
  let b = Buffer.create 13 in
  ToBuffer.pretty 0.8 80 b (f x);
  Buffer.contents b
