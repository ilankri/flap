%token ADD MUL DIV SUB REM IFEQ IFNE IFLT IFLE IFGT IFGE
%token PUSH POP SWAP BOX UNBOX DUP
%token ASTORE AASTORE
%token ALOAD AALOAD
%token JUMP ANEWARRAY IRETURN TABLESWITCH DEFAULT
%token COLON EOF
%token<int> INT
%token<string> ID COMMENT
%token<string> STRING

%start<Ast.t> program

%%

program: p=labelled_instruction* EOF
{
  { Ast.classname = "Flap";
    Ast.code = p;
    Ast.varsize = 1000;
    Ast.stacksize = 10000
  }
}
| error {
  let pos = Util.Position.lex_join $startpos $endpos in
  Util.Error.error "parsing" pos "Syntax error."
}


labelled_instruction: l=label? i=located(instruction) {
    (l, i)
  }

label: l=ID COLON {
    Ast.Label l
  }

instruction:
  PUSH i=INT           { Ast.Bipush i }
| ADD                  { Ast.Binop Ast.Add }
| SUB                  { Ast.Binop Ast.Sub }
| MUL                  { Ast.Binop Ast.Mul }
| DIV                  { Ast.Binop Ast.Div }
| REM                  { Ast.Binop Ast.Rem }
| IFEQ l=ID            { Ast.If_icmp (Ast.Eq, Ast.Label l) }
| IFNE l=ID            { Ast.If_icmp (Ast.Ne, Ast.Label l) }
| IFLE l=ID            { Ast.If_icmp (Ast.Le, Ast.Label l) }
| IFLT l=ID            { Ast.If_icmp (Ast.Lt, Ast.Label l) }
| IFGE l=ID            { Ast.If_icmp (Ast.Ge, Ast.Label l) }
| IFGT l=ID            { Ast.If_icmp (Ast.Gt, Ast.Label l) }
| JUMP l=ID            { Ast.Goto (Ast.Label l) }
| POP                  { Ast.Pop }
| SWAP                 { Ast.Swap }
| DUP                  { Ast.Dup }
| ASTORE i=INT         { Ast.Astore (Ast.Var i) }
| AASTORE              { Ast.AAstore }
| ALOAD i=INT          { Ast.Aload (Ast.Var i) }
| AALOAD               { Ast.AAload }
| ANEWARRAY ID         { Ast.Anewarray }
| IRETURN              { Ast.Ireturn }
| BOX                  { Ast.Box }
| UNBOX                { Ast.Unbox }
| TABLESWITCH i=INT l=ID* DEFAULT COLON lab=ID
    { let l = List.map (fun lab -> Ast.Label lab) l in
      Ast.Tableswitch (i,l,Ast.Label lab) }
| x=COMMENT            { Ast.Comment x }
| s=STRING             { Ast.Print s }

%inline located(X): x=X {
    (*Position.with_poss $startpos $endpos*) x
  }
