%token SEMICOLON COLON COMMA EOF DEF EXTERNAL CODE END LPAREN RPAREN
%token LOCAL CALL RET LARROW RARROW EXIT UPPERSAND
%token JUMP JUMPIF GT LT GTE LTE EQ
%token ADD MUL DIV SUB LOAD
%token<Int32.t> INT
%token<string> ID RID COMMENT
%type<Ast.lvalue> lvalue
%type<Ast.rvalue> rvalue
%start<Ast.t> program

%%

program: ds=definition* EOF
{
  ds
}
| error {
   let pos = Util.Position.lex_join $startpos $endpos in
   Util.Error.error "parsing" pos "Syntax error."
 }

definition: CODE LPAREN x=identifier RPAREN b=block END {
  Ast.DValue (x, b)
}
| DEF f=function_identifier
  LPAREN xs=separated_list(COMMA, identifier) RPAREN
  b=block
  END
{
  Ast.DFunction (f, xs, b)
}
| EXTERNAL f=function_identifier
{
  Ast.DExternalFunction f
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
  Ast.Id x
}

labelled_instruction: l=label COLON i=instruction SEMICOLON {
  (l, i)
}

label: l=ID {
  Ast.Label l
}

instruction:
  l=lvalue
  LARROW CALL f=rvalue
  LPAREN xs=separated_list(COMMA, rvalue) RPAREN
{
  Ast.Call (l, f, xs)
}
| l=lvalue
  LARROW f=function_identifier
  LPAREN xs=separated_list(COMMA, rvalue) RPAREN
{
  Ast.Call (l, `Immediate (Ast.LFun f), xs)
}
| RET r=rvalue
{
  Ast.Ret r
}
| l=lvalue LARROW o=op xs=separated_list(COMMA, rvalue)
{
  Ast.Assign (l, o, xs)
}
| JUMP l=label
{
  Ast.Jump l
}
| JUMPIF c=condition xs=separated_list(COMMA, rvalue)
  RARROW l1=label COMMA l2=label
{
  Ast.ConditionalJump (c, xs, l1, l2)
}
| c=COMMENT
{
  Ast.Comment c
}
| EXIT
{
  Ast.Exit
}

condition:
  GT  { Ast.GT }
| LT  { Ast.LT }
| GTE { Ast.GTE }
| LTE { Ast.LTE }
| EQ  { Ast.EQ }

op:
  ADD { Ast.Add }
| MUL { Ast.Mul }
| DIV { Ast.Div }
| SUB { Ast.Sub }
| LOAD { Ast.Load }
| c = condition { Ast.Bool c }

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
  Ast.RId r
}

rvalue:
  l=lvalue
{
  (l :> Ast.rvalue)
}
| l=literal
{
  `Immediate l
}

literal: x=INT
{
  Ast.LInt x
}
| UPPERSAND x=function_identifier
{
  Ast.LFun x
}

%inline located(X): x=X {
  Position.with_poss $startpos $endpos x
}

function_identifier: x=ID
{
  Ast.FId x
}
