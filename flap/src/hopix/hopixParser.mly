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
%token<char> CHAR
%token<string> STRING
%token<string> BASIC_ID PREFIX_ID INFIX_ID KID TID

%start<HopixAST.t> program

%%

program:
  | p = located(definition)* EOF {p}

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
  | l = located(literal) {Literal l}
  | id = located(identifier) {Variable id}

%inline identifier:
  | i = BASIC_ID {Id i}
  | i = PREFIX_ID {Id i}

%inline type_constructor:
  | tc = BASIC_ID {TCon tc}

%inline type_variable:
  | tid = TID {TId tid}

%inline literal:
  | i = INT {LInt i}
  | c = CHAR {LChar c}
  | s = STRING {LString s}
  | FALSE {LBool false}
  | TRUE {LBool true}

%inline located(X):
  | x = X {Position.with_poss $startpos $endpos x}
