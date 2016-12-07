%{
open HopixAST

let list_of_listoption = function
  | None -> []
  | Some l -> l

let prefixid_of_binop b = Id ("`" ^ b)
%}

%token EOF
%token TYPE EXTERN VAL FUN AND IF THEN ELIF ELSE REF WHILE FALSE TRUE
%token PLUS MINUS TIMES DIV LAND LOR EQ LEQ GEQ LT GT
%token LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE
%token COLON COMMA SEMICOLON BACKSLASH QMARK EMARK PIPE AMPERSAND UNDERSCORE
%token ARROW IMPL COLONEQ

%token<Int32.t> INT
%token<char>    CHAR
%token<string>  STRING
%token<string>  PREFIX_ID INFIX_ID BASIC_ID CONSTR_ID TYPE_VAR

%nonassoc THEN IMPL
%nonassoc ELSE ELIF
%left PIPE
%left AMPERSAND
%right COLONEQ
%left LOR
%left LAND
%left LT GT LEQ GEQ EQ
%left INFIX_ID
%left PLUS MINUS
%left TIMES DIV
%nonassoc REF
%nonassoc QMARK
%nonassoc EMARK

%start<HopixAST.t> program

%%

program:
  | p = located(definition)* EOF { p }

definition:
  | TYPE tc = located(type_con)
    tv = paren_comma_nonempty_list(located(type_variable))?
    td = preceded(EQ, tdefinition)?
      {
        let td =
          match td with
          | None -> Abstract
          | Some td -> td
        in
        DefineType (tc, list_of_listoption tv, td)
      }
  | EXTERN ext_id = located(var_id) COLON ty_name = located(ty)
      { DeclareExtern (ext_id, ty_name) }
  | vd = vdefinition(expr) { vd }

tdefinition:
  | PIPE? cdl = separated_nonempty_list(PIPE, constr_definition)
      { DefineSumType cdl }

constr_definition:
  | c = located(constr_id) tl = paren_comma_nonempty_list(located(ty))?
      { (c, list_of_listoption tl) }

(*
 * For
 * vdefinition := val var_id [ :type ] = expr
 *              | fun var_id [ type_variable { , type_variable } ] ]
 *                           ( pattern { , pattern } ) [ : type ] = expr
 *                           { and var_id [[type_variable {, type_variable } ]]
 *                           (pattern { , pattern })[ :type ]=expr }
 *)
vdefinition(X):
  | VAL id = located(var_id) exp = expr_with_return_type(X)
      {
        DefineValue (id, exp)
      }
  | FUN vlist = var_id_list(X) more = option(and_var_id_list(X))
      {
        let l = [vlist] in
        match more with
        | None -> DefineRecFuns(l)
        | Some lmore -> DefineRecFuns(l @ lmore)
      }

(*
 * EXPR := expr
 * For
 * [ : type ] = EXPR
 *)
expr_with_return_type(EXPR):
  | t = preceded(COLON, located(ty))? EQ exp = located(EXPR)
    {
      match t with
      | None -> exp
      | Some t -> Position.unknown_pos (TypeAnnotation(exp, t))
    }

and_var_id_list(X):
  | AND li = separated_nonempty_list(AND, var_id_list(X)) { li }

var_id_list(X):
  | id = located(var_id)
    typ_list = option(bracket_comma_nonempty_list(located(type_variable)))
    pat_list = paren_comma_nonempty_list(located(pattern))
    e = expr_with_return_type(X)
    { (id, Position.unknown_pos
    ( FunctionDefinition( list_of_listoption(typ_list), pat_list, e))) }

(*
 * For
 * pattern ::=
 * | pattern | pattern
 * | pattern & pattern
 * | constr_id ( pattern { , pattern } )
 * | ( pattern )
 *)
pattern:
  | p = simple_pattern { p }
  | p = located(simple_pattern) COLON t = located(ty) { PTypeAnnotation(p, t) }
  | p1 = located(pattern) PIPE p2 = located(pattern)
      {
        match Position.value p1, Position.value p2 with
        | POr l1, POr l2 -> POr (l1 @ l2)
        | POr l1, _ -> POr (l1 @ [p2])
        | _, POr l2 -> POr (p1 :: l2)
        | _ -> POr [p1; p2]
      }

(*
 * For terminals
 * pattern ::=
 * | pattern : type
 *)
simple_pattern:
  | id = located(var_id) { PVariable id }
  | li = located(literal) { PLiteral li }
  | UNDERSCORE { PWildcard }
  | LPAREN p = pattern RPAREN { p }
  | id = located(constr_id)
    pat_list = paren_comma_nonempty_list(located(pattern))?
      { PTaggedValue(id, list_of_listoption pat_list) }
  | p1 = located(simple_pattern) AMPERSAND p2 = located(simple_pattern)
      {
        match Position.value p1, Position.value p2 with
        | PAnd l1, PAnd l2 -> PAnd (l1 @ l2)
        | PAnd l1, _ -> PAnd (l1 @ [p2])
        | _, PAnd l2 -> PAnd (p1 :: l2)
        | _ -> PAnd [p1; p2]
      }

simple_ty:
  | tv = type_variable { TyVar tv }
  | LPAREN t = ty RPAREN { t }
  | tc = type_con tl = paren_comma_nonempty_list(located(ty))?
      { TyCon (tc, list_of_listoption tl) }

ty:
  | t = simple_ty { t }
  (* We force the right associativity of the type operator '->'.  *)
  | t1 = located(simple_ty) ARROW t2 = located(ty)
      { TyCon (TCon "->", [t1; t2]) }

(*
 * For
 * expr := inte
 *       | char
 *       | var_id
 *)
base_simple_expr:
  | li = located(literal) { Literal li }
  | vid = located(var_id) { Variable vid }
  (* (expr) *)
  | LPAREN e = expr RPAREN { e }
  (* ( expr : type ) *)
  | LPAREN e = located(expr) COLON t = located(ty) RPAREN
      { TypeAnnotation (e, t) }
  (* expr [[type {,type }]](expr {,expr }) *)
  | e = located(base_simple_expr) tl = bracket_comma_nonempty_list(located(ty))?
    el = paren_comma_nonempty_list(located(expr))
      { Apply (e, list_of_listoption(tl), el) }

simple_expr:
  | b = base_simple_expr { b }
  (* ! expr *)
  | EMARK e = located(simple_expr) { Read e }
  (* ref expr *)
  | REF e = located(simple_expr) { Ref e }
  (* constr_id [ [type { ,type }] ] [ (expr { ,expr } ) ] *)
  | cid = located(constr_id) tyl = bracket_comma_nonempty_list(located(ty))?
    expl = paren_comma_nonempty_list(located(expr))?
      { Tagged(cid, list_of_listoption tyl, list_of_listoption expl) }
  (* \ [ [type_variable {, type_variable }] ] (pattern {, pattern })=> expr *)
  | BACKSLASH tyvl = bracket_comma_nonempty_list(located(type_variable))?
    patl = paren_comma_nonempty_list(located(pattern)) IMPL
     e = located(simple_expr)
      { Fun ( FunctionDefinition(list_of_listoption(tyvl), patl, e) ) }
  (* expr binop expr *)
  | e = binop_expr(simple_expr) { e }
  (* expr := expr *)
  | e1 = located(base_simple_expr) COLONEQ e2 = located(simple_expr)
      { Write (e1, e2) }
  (* expr ? branches *)
  | e = located(simple_expr) QMARK bl = branches { Case(e, bl) }
  (* while expr { expr } *)
  | WHILE e1 = located(expr) LBRACE e2 = located(expr) RBRACE { While (e1, e2) }
  (* if expr then expr { elif expr then expr } [ else expr ] *)
  | i = cond_expr(inlined_simple_expr) { i }

binop_expr(right_expr):
  | e1 = located(simple_expr) b = located(binop) e2 = located(right_expr)
      { Apply (Position.unknown_pos (Variable b), [], [e1; e2]) }

complex_binop_expr:
  | e = binop_expr(localdef_expr) { e }

expr:
  | e = simple_expr | e = localdef_expr | e = seq_expr | e = complex_binop_expr
  | e = cond_expr(localdef_expr) | e = cond_expr(complex_binop_expr)
      { e }

seq_expr:
  (* expr { ; expr } *)
  | e = located(simple_expr) SEMICOLON
    el = separated_nonempty_list(SEMICOLON, located(simple_expr))
      {
        let el = e :: el in
        let el = List.rev el in
        (* It is risky to use a valid id like "nothing".  *)
        let dummy_id = Position.unknown_pos (Id "nothing") in
        let f e2 e1 = Position.(unknown_pos (Define(dummy_id, e1, e2))) in
        Position.value (List.fold_left f (List.hd el) (List.tl el))
      }

(*
 * vdefinition ; expr
 *)
localdef_expr:
  | vd = vdefinition(simple_expr) SEMICOLON e2 = located(expr)
      {
        match vd with
        | DefineValue(x1, e1) -> Define (x1, e1, e2)
        | DefineRecFuns(li) -> DefineRec(li ,e2)
        | _ ->
          failwith "Error: DefineType and DeclareExtern \
                    should not be in the vdefinition"
      }

%inline inlined_simple_expr:
  | e = simple_expr { e }

(*
 * For
 * expr := if expr then expr { elif expr then expr } [ else expr ]
 *)
cond_expr(rightmost_expr):
  | b1 = if_branch(inlined_simple_expr)
    bl = nonempty_elif_list(inlined_simple_expr) e = else_branch(rightmost_expr)
      { If (b1 :: bl, e) }
  | b = if_branch(inlined_simple_expr) e = else_branch(rightmost_expr)
      { If ([b], e) }
  | b1 = if_branch(inlined_simple_expr) bl = nonempty_elif_list(rightmost_expr)
      { If (b1 :: bl, None) }
  | b = if_branch(rightmost_expr)
      { If ([b], None) }

%inline nonempty_elif_list(right_expr):
  | l = nonempty_list(elif_branch(right_expr)) { l }

else_branch(expr):
  | ELSE e = located(expr) { Some e }

%inline if_branch(right_expr):
  | IF e1 = located(expr) THEN e2 = located(right_expr)  { (e1, e2) }

%inline elif_branch(right_expr):
  | ELIF c = located(expr) THEN e = located(right_expr) { (c, e) }

(*
 * For
 * branches ::= [ | ] branch { | branch }
 *          | { [ | ] branch { | branch } }
 *)
branches:
  | b = multi_branches(simple_expr) { b }
  | LBRACE b = multi_branches(expr) RBRACE { b }

multi_branches(X):
  | PIPE? blist = separated_nonempty_list(PIPE, located(branch(X)))
      { blist }

%inline branch(X):
  | p = located(pattern) IMPL e = located(X) { Branch(p, e) }

%inline var_id:
  | id = BASIC_ID | id = PREFIX_ID { Id id }

%inline constr_id:
  | id = CONSTR_ID { KId id }

%inline type_con:
  | id = BASIC_ID { TCon id }

%inline type_variable:
  | id = TYPE_VAR { TId id }

%inline literal:
  | i = INT { LInt i }
  | MINUS i = INT { LInt (Int32.neg i) }
  | c = CHAR { LChar c }
  | str = STRING { LString str }
  | FALSE { LBool false }
  | TRUE { LBool true }

%inline binop:
  | PLUS { prefixid_of_binop "+" }
  | MINUS { prefixid_of_binop "-" }
  | TIMES { prefixid_of_binop "*" }
  | DIV { prefixid_of_binop "/" }
  | LAND { prefixid_of_binop "&&" }
  | LOR { prefixid_of_binop "||" }
  | EQ { prefixid_of_binop "=" }
  | LEQ { prefixid_of_binop "<=" }
  | GEQ { prefixid_of_binop ">=" }
  | LT { prefixid_of_binop "<" }
  | GT { prefixid_of_binop ">" }
  | s = INFIX_ID { prefixid_of_binop s }

%inline located(X):
  | x = X { Position.with_poss $startpos $endpos x }

%inline delim_sep_nonempty_list(opening, separator, X, closing):
  | opening l = separated_nonempty_list(separator, X) closing { l }

%inline delim_comma_nonempty_list(opening, X, closing):
  | l = delim_sep_nonempty_list(opening, COMMA, X, closing) { l }

%inline paren_comma_nonempty_list(X):
  | l = delim_comma_nonempty_list(LPAREN, X, RPAREN) { l }

%inline bracket_comma_nonempty_list(X):
  | l = delim_comma_nonempty_list(LBRACKET, X, RBRACKET) { l }
