%token ADD MUL DIV SUB REM IFEQ IFNE IFLT IFLE IFGT IFGE
%token PUSH POP SWAP BOX UNBOX DUP
%token ASTORE AASTORE
%token ALOAD AALOAD
%token JUMP ANEWARRAY IRETURN TABLESWITCH DEFAULT
%token COLON EOF
%token<int> INT
%token<string> ID COMMENT
%token<string> STRING

%start<JavixAST.t> program

%%

program: p=labelled_instruction* EOF
{
  { JavixAST.classname = "Flap";
    JavixAST.code = p;
    JavixAST.varsize = 1000;
    JavixAST.stacksize = 10000
  }
}
| error {
  let pos = Position.lex_join $startpos $endpos in
  Error.error "parsing" pos "Syntax error."
}


labelled_instruction: l=label? i=located(instruction) {
  (l, i)
}

label: l=ID COLON {
  JavixAST.Label l
}

instruction:
  PUSH i=INT           { JavixAST.Bipush i }
| ADD                  { JavixAST.Binop JavixAST.Add }
| SUB                  { JavixAST.Binop JavixAST.Sub }
| MUL                  { JavixAST.Binop JavixAST.Mul }
| DIV                  { JavixAST.Binop JavixAST.Div }
| REM                  { JavixAST.Binop JavixAST.Rem }
| IFEQ l=ID            { JavixAST.If_icmp (JavixAST.Eq, JavixAST.Label l) }
| IFNE l=ID            { JavixAST.If_icmp (JavixAST.Ne, JavixAST.Label l) }
| IFLE l=ID            { JavixAST.If_icmp (JavixAST.Le, JavixAST.Label l) }
| IFLT l=ID            { JavixAST.If_icmp (JavixAST.Lt, JavixAST.Label l) }
| IFGE l=ID            { JavixAST.If_icmp (JavixAST.Ge, JavixAST.Label l) }
| IFGT l=ID            { JavixAST.If_icmp (JavixAST.Gt, JavixAST.Label l) }
| JUMP l=ID            { JavixAST.Goto (JavixAST.Label l) }
| POP                  { JavixAST.Pop }
| SWAP                 { JavixAST.Swap }
| DUP                  { JavixAST.Dup }
| ASTORE i=INT         { JavixAST.Astore (JavixAST.Var i) }
| AASTORE              { JavixAST.AAstore }
| ALOAD i=INT          { JavixAST.Aload (JavixAST.Var i) }
| AALOAD               { JavixAST.AAload }
| ANEWARRAY ID         { JavixAST.Anewarray }
| IRETURN              { JavixAST.Ireturn }
| BOX                  { JavixAST.Box }
| UNBOX                { JavixAST.Unbox }
| TABLESWITCH i=INT l=ID* DEFAULT COLON lab=ID
                       { let l = List.map (fun lab -> JavixAST.Label lab) l in
                         JavixAST.Tableswitch (i,l,JavixAST.Label lab) }
| x=COMMENT            { JavixAST.Comment x }
| s=STRING             { JavixAST.Print s }

%inline located(X): x=X {
  (*Position.with_poss $startpos $endpos*) x
}
