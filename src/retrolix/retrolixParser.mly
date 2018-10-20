%{

open RetrolixAST

%}

%token SEMICOLON COLON COMMA EOF DEF EXTERNAL CODE END LPAREN RPAREN
%token LOCAL CALL RET LARROW RARROW EXIT UPPERSAND
%token JUMP JUMPIF GT LT GTE LTE EQ
%token ADD MUL DIV SUB LOAD
%token<Int32.t> INT
%token<string> ID RID COMMENT
%type<lvalue> lvalue
%type<rvalue> rvalue
%start<RetrolixAST.t> program

%%

program: ds=definition* EOF
{
  ds
}
| error {
   let pos = Position.lex_join $startpos $endpos in
   Error.error "parsing" pos "Syntax error."
 }

definition: CODE LPAREN x=identifier RPAREN b=block END {
  DValue (x, b)
}
| DEF f=function_identifier
  LPAREN xs=separated_list(COMMA, identifier) RPAREN
  b=block
  END
{
  DFunction (f, xs, b)
}
| EXTERNAL f=function_identifier
{
  DExternalFunction f
}

locals: LOCAL xs=separated_nonempty_list(COMMA, identifier)
{
  xs
}
| /* empty word */
{
  []
}

block: xs=locals COLON ls=labelled_instruction*
{
  (xs, ls)
}

identifier: x=ID {
  Id x
}

labelled_instruction: l=label COLON i=instruction SEMICOLON {
  (l, i)
}

label: l=ID {
  Label l
}

instruction:
  l=lvalue
  LARROW CALL f=rvalue
  LPAREN xs=separated_list(COMMA, rvalue) RPAREN
{
  Call (l, f, xs)
}
| l=lvalue
  LARROW f=function_identifier
  LPAREN xs=separated_list(COMMA, rvalue) RPAREN
{
  Call (l, `Immediate (LFun f), xs)
}
| RET r=rvalue
{
  Ret r
}
| l=lvalue LARROW o=op xs=separated_list(COMMA, rvalue)
{
  Assign (l, o, xs)
}
| JUMP l=label
{
  Jump l
}
| JUMPIF c=condition xs=separated_list(COMMA, rvalue)
  RARROW l1=label COMMA l2=label
{
  ConditionalJump (c, xs, l1, l2)
}
| c=COMMENT
{
  Comment c
}
| EXIT
{
  Exit
}

condition:
  GT  { GT }
| LT  { LT }
| GTE { GTE }
| LTE { LTE }
| EQ  { EQ }

op:
  ADD { Add }
| MUL { Mul }
| DIV { Div }
| SUB { Sub }
| LOAD { Load }
| c=condition { Bool c }

lvalue:
 v=identifier
{
  `Variable v
}
| r=register
{
  `Register r
}

register: r=RID
{
  RId r
}

rvalue:
  l=lvalue
{
  (l :> rvalue)
}
| l=literal
{
  `Immediate l
}

literal: x=INT
{
  LInt x
}
| UPPERSAND x=function_identifier
{
  LFun x
}

%inline located(X): x=X {
  Position.with_poss $startpos $endpos x
}

function_identifier: x=ID
{
  FId x
}
