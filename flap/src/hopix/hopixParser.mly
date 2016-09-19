%{

  open HopixAST

%}

%token VAL
%token EQUAL
%token EOF
%token<string> ID
%token<Int32.t> INT

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
x=located(identifier)
{
  Variable x
}
| l=located(literal)
{
  Literal l
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
