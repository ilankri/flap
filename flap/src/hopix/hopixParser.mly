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
%token<string>  BASIC_ID PREFIX_ID INFIX_ID KID TID

%start<HopixAST.t> program

%%

program:
| p = located(definition)* EOF { p }

definition:
| EXTERN ext_id = located(identifier) COLON ty_name = located(ty)
  {
    DeclareExtern (ext_id, ty_name) 
  }
| vd = vdefinition { vd }

vdefinition:
| VAL i = located(identifier) EQUAL e = located(expression)
  {
    DefineValue (i, e)
  }

ty:
| tv = type_variable { TyVar tv }

expression:
| l = located(literal) { Literal l }
| id = located(identifier) { Variable id }

%inline identifier:
| basic = BASIC_ID {Id basic}
| prefix = PREFIX_ID {Id prefix}

%inline type_constructor:
| typecon = BASIC_ID {TCon typecon}

%inline type_variable:
| typevar = TID {TId typevar}

%inline literal:
| i = INT { LInt i }
| c = CHAR { LChar c }
| str = STRING { LString str }
| FALSE { LBool false }
| TRUE { LBool true }

%inline located(X):
| x = X { Position.with_poss $startpos $endpos x }
