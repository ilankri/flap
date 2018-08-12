%{

open KontixAST

type expr_or_tuple =
  | Expr of basicexpr
  | Kont
  | Env
  | Tuple of expr_or_tuple list

let mkcall f l startpos endpos =
  match f, l with
  | None, [Env; Expr res] -> TContCall res
  | Some e, (Kont::Env::args) ->
     (try
        let a = List.map (function Expr e -> e | _ -> raise Not_found) args in
        TFunCall (e, a)
      with Not_found ->
        let pos = Position.lex_join startpos endpos in
        Error.error "parsing" pos
        "bad application (missing or badly shaped arguments)")
  | _ ->
     let pos = Position.lex_join startpos endpos in
     Error.error "parsing" pos
     "bad application (missing or badly shaped arguments)"

%}

%token DEF LET IN IF THEN ELSE EVAL UPPERSAND QMARK NEW K E
%token PLUS MINUS STAR SLASH GT GTE LT LTE EQUAL PERCENT
%token LPAREN RPAREN LBRACKET RBRACKET ASSIGNS COMMA SEMICOLON EOF
%token<int> INT
%token<string> ID
%token<string> STRING

%nonassoc ELSE IN
%right SEMICOLON
%nonassoc ASSIGNS
%nonassoc GT GTE LT LTE EQUAL
%left PLUS MINUS
%left STAR SLASH PERCENT
%nonassoc LBRACKET

%start<KontixAST.t> program

%%

program: ds=definition* EVAL e=tailexpr EOF
{
  ds,e
}
| error {
  let pos = Position.lex_join $startpos $endpos in
  Error.error "parsing" pos "Syntax error."
}

definition:
| DEF f=ID LPAREN K COMMA E ids=list(preceded(COMMA,ID)) RPAREN EQUAL e=tailexpr
    { DefFun (f,ids,e) }
| DEF f=ID LPAREN ids=tuple COMMA res=ID RPAREN EQUAL e=tailexpr
    { DefCont (f,ids,res,e) }

tuple:
 LBRACKET K COMMA E ids=list(preceded(COMMA,ID)) RBRACKET   { ids }

basicexpr:
| x=INT                                            { Num x }
| UPPERSAND f=ID                                   { FunName f }
| x=ID                                             { Var x }
| LET x=ID EQUAL e1=basicexpr IN e2=basicexpr      { Let (x, e1, e2) }
| IF c=basicexpr THEN t=basicexpr ELSE f=basicexpr { IfThenElse (c, t, f) }
| l=basicexpr b=binop r=basicexpr                  { BinOp (b, l, r) }
| e=basicexpr LBRACKET i=basicexpr RBRACKET        { BlockGet (e, i) }
| e=basicexpr LBRACKET i=basicexpr RBRACKET ASSIGNS v=basicexpr
                                                   { BlockSet (e, i, v) }
| NEW LBRACKET e=basicexpr RBRACKET                { BlockNew (e) }
| e1=basicexpr SEMICOLON e2=basicexpr              { Let ("_", e1, e2) }
| LPAREN e=basicexpr RPAREN                        { e }
| s=STRING                                         { Print s }

tailexpr:
| LET K COMMA E EQUAL UPPERSAND f=ID COMMA t=tuple IN e=tailexpr
                                                   { TPushCont(f,t,e) }
| LET x=ID EQUAL e1=basicexpr IN e2=tailexpr       { TLet (x, e1, e2) }
| e1=basicexpr SEMICOLON e2=tailexpr               { TLet ("_", e1, e2) }
| IF c=basicexpr THEN t=tailexpr ELSE f=tailexpr   { TIfThenElse (c, t, f) }
| f=fun_head LPAREN l=separated_list(COMMA,basicexpr_or_tuple) RPAREN
                                                   { mkcall f l $startpos $endpos }

fun_head:
| QMARK LPAREN e=basicexpr RPAREN { Some e }
| f=ID { Some (FunName f) }
| K { None }

basicexpr_or_tuple:
| LBRACKET es=separated_list(COMMA, basicexpr_or_tuple) RBRACKET { Tuple es }
| e=basicexpr { Expr e }
| K { Kont }
| E { Env }

%inline binop:
  PLUS  { Add }
| MINUS { Sub }
| STAR  { Mul }
| SLASH { Div }
| PERCENT { Mod }
| GT    { Gt }
| GTE   { Ge }
| LT    { Lt }
| LTE   { Le }
| EQUAL { Eq }
