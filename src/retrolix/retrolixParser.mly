%token SEMICOLON COLON COMMA EOF DEF EXTERNAL CODE END LPAREN RPAREN
%token LOCAL CALL RET LARROW RARROW EXIT UPPERSAND
%token JUMP JUMPIF GT LT GTE LTE EQ
%token ADD MUL DIV SUB LOAD
%token<Int32.t> INT
%token<string> ID RID COMMENT
%type<RetrolixAST.lvalue> lvalue
%type<RetrolixAST.rvalue> rvalue
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
  RetrolixAST.DValue (x, b)
}
| DEF f=function_identifier
  LPAREN xs=separated_list(COMMA, identifier) RPAREN
  b=block
  END
{
  RetrolixAST.DFunction (f, xs, b)
}
| EXTERNAL f=function_identifier
{
  RetrolixAST.DExternalFunction f
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
  RetrolixAST.Id x
}

labelled_instruction: l=label COLON i=instruction SEMICOLON {
  (l, i)
}

label: l=ID {
  RetrolixAST.Label l
}

instruction:
  l=lvalue
  LARROW CALL f=rvalue
  LPAREN xs=separated_list(COMMA, rvalue) RPAREN
{
  RetrolixAST.Call (l, f, xs)
}
| l=lvalue
  LARROW f=function_identifier
  LPAREN xs=separated_list(COMMA, rvalue) RPAREN
{
  RetrolixAST.Call (l, `Immediate (RetrolixAST.LFun f), xs)
}
| RET r=rvalue
{
  RetrolixAST.Ret r
}
| l=lvalue LARROW o=op xs=separated_list(COMMA, rvalue)
{
  RetrolixAST.Assign (l, o, xs)
}
| JUMP l=label
{
  RetrolixAST.Jump l
}
| JUMPIF c=condition xs=separated_list(COMMA, rvalue)
  RARROW l1=label COMMA l2=label
{
  RetrolixAST.ConditionalJump (c, xs, l1, l2)
}
| c=COMMENT
{
  RetrolixAST.Comment c
}
| EXIT
{
  RetrolixAST.Exit
}

condition:
  GT  { RetrolixAST.GT }
| LT  { RetrolixAST.LT }
| GTE { RetrolixAST.GTE }
| LTE { RetrolixAST.LTE }
| EQ  { RetrolixAST.EQ }

op:
  ADD { RetrolixAST.Add }
| MUL { RetrolixAST.Mul }
| DIV { RetrolixAST.Div }
| SUB { RetrolixAST.Sub }
| LOAD { RetrolixAST.Load }
| c = condition { RetrolixAST.Bool c }

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
  RetrolixAST.RId r
}

rvalue:
  l=lvalue
{
  (l :> RetrolixAST.rvalue)
}
| l=literal
{
  `Immediate l
}

literal: x=INT
{
  RetrolixAST.LInt x
}
| UPPERSAND x=function_identifier
{
  RetrolixAST.LFun x
}

%inline located(X): x=X {
  Position.with_poss $startpos $endpos x
}

function_identifier: x=ID
{
  RetrolixAST.FId x
}
