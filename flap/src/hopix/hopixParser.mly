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
  | EXTERN ext_id = located(var_id) COLON ty_name = located(ty)
    {
      DeclareExtern (ext_id, ty_name)
    }
  | vd = vdefinition { vd }

vdefinition:
  | VAL id = located(var_id) t = preceded(COLON, located(ty))? EQ
    exp = located(expression)
    {
      let exp =
        match t with
        | None -> exp
        | Some t ->
          Position.with_pos (Position.position exp) (TypeAnnotation(exp, t)) in
      DefineValue (id, exp)
    }

simple_ty:
  | tv = type_variable { TyVar tv }
  | LPAREN t = ty RPAREN { t }
  | tc = type_con tl = ty_list(LPAREN, RPAREN)?
    {
      TyCon (tc, list_of_listoption tl)
    }

ty:
  | t = simple_ty { t }
  (* We force the right associativity of the type operator '->'.  *)
  | t1 = located(simple_ty) ARROW t2 = located(ty)
    {
      TyCon (TCon "->", [t1; t2])
    }

ty_list(START_SEP, END_SEP):
  | START_SEP tl = separated_nonempty_list(COMMA, located(ty)) END_SEP { tl }

expression:
  | li = located(literal) { Literal li }
  | vid = located(var_id) { Variable vid }
  | cid = located(constr_id) tyl = ty_list(LBRACKET, RBRACKET)?
    expl = expr_list?
    {
      Tagged(cid, list_of_listoption tyl, list_of_listoption expl)
    }

expr_list:
  | LPAREN li = separated_nonempty_list(COMMA, located(expression)) RPAREN
  {
    li
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
