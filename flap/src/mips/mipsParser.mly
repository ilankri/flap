%{

  open MipsAST

%}

%token EOF

%start<MipsAST.t> program

%%

program: EOF
{
  failwith "No parser for Mips"
}
