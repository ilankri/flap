%{

  open HobixAST

%}

%token VAL
%token PLUS MINUS STAR SLASH
%token FUN WHILE
%token LTE LT GT GTE EQUAL LAND LOR
%token IF THEN ELSE FI NEWBLOCK
%token AND EXTERN
%token LBRACKET RBRACKET COMMA BACKSLASH DRARROW
%token LBRACE RBRACE
%token<string> LSTRING
%token<char> LCHAR
%token LPAREN RPAREN
%token SEMICOLON DEQUAL EOF
%token<Int32.t> INT
%token<string> ID INFIXID
%type <HobixAST.expression> expression

%right SEMICOLON
%nonassoc FUN AND
%nonassoc DEQUAL
%nonassoc DRARROW
%left LOR
%left LAND
%nonassoc LTE LT GT GTE EQUAL
%left INFIXID
%left PLUS MINUS
%left STAR SLASH

%start<HobixAST.t> program

%%

program: ds=definition* EOF
{
  ds
}

definition:
VAL d=value_def
{
  let (x, e) = d in
  DefineValue (x, e)
}
| FUN d=function_definition ds=mutfun
{
  DefineRecFuns (d :: ds)
}
| EXTERN x=identifier
{
  DeclareExtern (x)
}
| error {
  let pos = Position.lex_join $startpos $endpos in
  Error.error "parsing" pos "Syntax error."
}

%inline value_def:
x=identifier EQUAL e=expression
{
  (x, e)
}

%inline function_definition:
x=identifier
LPAREN xs=separated_list(COMMA, identifier) RPAREN
EQUAL e=expression
{
  (x, Fun (xs, e))
}

mutfun:
/* empty */ %prec AND { [] }
| AND d=function_definition ds=mutfun
{ d::ds }

expression:
s=simple_expression
{
      s
}
| e1=expression SEMICOLON e2=expression
{
  Define (Id "nothing", e1, e2)
}
| VAL vdef=value_def SEMICOLON e2=expression
{
  let (id,e1) = vdef in Define (id,e1,e2)
}
| FUN d=function_definition ds=mutfun SEMICOLON e=expression %prec FUN
{
  DefineRec (d::ds, e)
}
| WHILE e=expression LBRACE b=expression RBRACE
{
  While (e, b)
}
| NEWBLOCK LPAREN e=expression RPAREN
{
  AllocateBlock e
}
| b=simple_expression LBRACKET i=expression RBRACKET DEQUAL rhs=expression
{
  WriteBlock (b, i, rhs)
}
| lhs=expression b=binop rhs=expression
{
  Apply (Variable (Id b), [lhs; rhs])
}
| IF c=expression THEN t=expression ELSE e=expression FI
{
  IfThenElse (c, t, e)
}
| BACKSLASH
    LPAREN xs=separated_list(COMMA, identifier) RPAREN
    DRARROW e=expression
{
  Fun (xs, e)
}

simple_expression:
| a=simple_expression
  LPAREN bs=separated_list(COMMA, expression) RPAREN
{
  Apply (a, bs)
}
| b=simple_expression LBRACKET i=expression RBRACKET
{
  ReadBlock (b, i)
}

| e=very_simple_expression
{
  e
}

very_simple_expression:
  l=literal
{
  Literal l
}
| x=identifier
{
  HobixAST.Variable x
}
| LPAREN e=expression RPAREN
{
  e
}

%inline binop:
  x=INFIXID { String.(sub x 0 (length x - 1)) }
| PLUS  { "`+"  }
| MINUS { "`-"  }
| STAR  { "`*"  }
| SLASH { "`/"  }
| GT    { "`>"  }
| GTE   { "`>=" }
| LT    { "`<"  }
| LTE   { "`<=" }
| EQUAL { "`="  }
| LAND  { "`&&" }
| LOR   { "`||" }

%inline literal:
  x=INT
{
  LInt x
}
| MINUS x=INT
{
  LInt (Int32.neg x)
}
| s=LSTRING
{
  LString s
}
| c=LCHAR
{
  LChar c
}

%inline identifier: x=ID {
  Id x
}
