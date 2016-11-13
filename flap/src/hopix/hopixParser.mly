%{
open HopixAST

let list_of_listoption = function
  | None -> []
  | Some l -> l
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
      { (c, list_of_listoption tl)}

vdefinition(X):
  | VAL id = located(var_id) t = preceded(COLON, located(ty))? EQ
    exp = located(X)
      {
        let exp =
          match t with
          | None -> exp
          | Some t ->
            let pos = Position.position exp in
            Position.with_pos pos (TypeAnnotation(exp, t))
        in
        DefineValue (id, exp)
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

(** 
 * For 
 * expr := int
 *       | char
 *       | var_id
 *       | ( expr : type )
 *       | ( expr )
 *       | ref expr
 *       | ! expr
 * **)
simple_expr:
  | li = located(literal) { Literal li }
  | vid = located(var_id) { Variable vid }
  | LPAREN e = located(expr) COLON t = located(ty) RPAREN
      { TypeAnnotation (e, t) }
  | LPAREN e = expr RPAREN { e }
  | REF e = located(simple_expr) { Ref e }
  | EMARK e = located(simple_expr) { Read e }

(** 
 * For
 * expr := { simple_expr }
 *       | vdefinition ; expr
 *       | constr_id / [ [type { ,type }] ] [ (expr { ,expr } ) ]
 *       | expr := expr
 * **)
expr:
  | e = unseq_expr { e }
  | e = seq_expr { e }

unseq_expr:
  | e = simple_expr { e }
  | cid = located(constr_id) tyl = bracket_comma_nonempty_list(located(ty))?
    expl = paren_comma_nonempty_list(located(expr))?
      { Tagged(cid, list_of_listoption tyl, list_of_listoption expl) }
  | e1 = located(simple_expr) COLONEQ e2 = located(unseq_expr)
      { Write (e1, e2) }
  | WHILE e1 = located(expr) LBRACE e2 = located(expr) RBRACE { While (e1, e2) }
  | e = located(simple_expr) tl = bracket_comma_nonempty_list(located(ty))?
    el = paren_comma_nonempty_list(located(expr))
      { Apply (e, list_of_listoption tl, el) }

seq_expr:
  | vd = vdefinition(unseq_expr) SEMICOLON e2 = located(expr)
      {
        match vd with
        | DefineValue (x1, e1) -> Define (x1, e1, e2)
      }
  | e = located(unseq_expr) SEMICOLON
    el = separated_nonempty_list(SEMICOLON, located(unseq_expr))
      {
        let f e1 e2 =
          Position.(unknown_pos (Define (unknown_pos (Id "_"), e1, e2))) in
        Position.value (List.fold_left f e el)
      }

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
  | c = CHAR { LChar c }
  | str = STRING { LString str }
  | FALSE { LBool false }
  | TRUE { LBool true }

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
