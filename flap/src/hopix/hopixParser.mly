%{

  open HopixAST


%}

%token EOF
%token<Int32.t> INT


%start<HopixAST.t> program

%%

program: EOF
{
   []
}


