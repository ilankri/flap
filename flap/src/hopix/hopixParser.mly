%{

  open HopixAST

%}

%token STAR PLUS MINUS SLASH
%token VAL
%token EQUAL
%token EOF
%token<string> ID
%token<Int32.t> INT

%left PLUS MINUS
%left STAR SLASH
%start<HopixAST.t> program

%%

program: ds=located(definition)* EOF
{
  ds
}

definition:
VAL x=located(identifier) EQUAL e=located(expression)
{
  DefineValue (x, e)
}

expression:
x=identifier
{
  Variable x
}
| l=literal
{
  Literal l
}
| lhs=located(expression) b=located(binop) rhs=located(expression)
{
  Apply (b, [ lhs; rhs ])
}

%inline binop:
STAR
{
  Variable (Id "*")
}
| PLUS
{
  Variable (Id "+")
}
| MINUS
{
  Variable (Id "*")
}
| SLASH
{
  Variable (Id "/")
}

literal:
x=INT
{
  Literal.LInt x
}

identifier: x=ID
{
  Id x
}

%inline located(X): x=X {
  Position.with_poss $startpos $endpos x
}
