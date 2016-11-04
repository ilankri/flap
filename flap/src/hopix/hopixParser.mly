%{
  open HopixAST
%}

%token EOF
%token TYPE EXTERN VAL FUN AND IF THEN ELIF ELSE REF WHILE FALSE TRUE
%token PLUS MINUS TIMES DIV LAND LOR EQUAL LEQ GEQ LT GT
%token LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE
%token COLON COMMA BACKSLASH QMARK EMARK PIPE AMPERSAND UNDERSCORE
%token ARROW IMPL COLONEQ

%token<Int32.t> INT
%token<char>    CHAR
%token<string>  STRING
%token<string>  PREFIX_ID INFIX_ID VAR_ID CONSTR_ID TYPE_CON TYPE_VAR

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
  | VAL id = located(var_id) EQUAL exp = located(expression)
    {
      DefineValue (id, exp)
    }

ty:
  | tv = type_variable { TyVar tv }

expression:
  | li = located(literal) { Literal li }
  | id = located(var_id) { Variable id }

%inline var_id:
  | id = VAR_ID { Id id }

%inline constr_id:
  | id = CONSTR_ID { KId id }

%inline type_con:
  | id = TYPE_CON { TCon id }

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
