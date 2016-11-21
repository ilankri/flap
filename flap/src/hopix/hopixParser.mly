%{
open HopixAST

let list_of_listoption = function
  | None -> []
  | Some l -> l

%}

%token EOF
%token TYPE EXTERN VAL FUN AND IF THEN ELIF ELSE REF WHILE FALSE TRUE
%token PLUS MINUS TIMES DIV LAND LOR EQ LEQ GEQ LT GT
%token LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE
%token COLON COMMA SEMICOLON BACKSLASH QMARK EMARK PIPE AMPERSAND UNDERSCORE
%token ARROW IMPL COLONEQ

%token<Int32.t> INT
%token<char>    CHAR
%token<string>  STRING
%token<string>  PREFIX_ID INFIX_ID BASIC_ID CONSTR_ID TYPE_VAR

%nonassoc REF
%right COLONEQ
%left LOR
%left LAND
%left LT GT LEQ GEQ EQ
%left INFIX_ID
%left PLUS MINUS
%left TIMES DIV
%left PIPE
%nonassoc EMARK

%start<HopixAST.t> program

%%

program:
  | p = located(definition)* EOF { p }

definition:
  | TYPE tc = located(type_con)
    tv = paren_comma_nonempty_list(located(type_variable))?
    td = preceded(EQ, tdefinition)?
      {
        let td =
          match td with
          | None -> Abstract
          | Some td -> td
        in
        DefineType (tc, list_of_listoption tv, td)
      }
  | EXTERN ext_id = located(var_id) COLON ty_name = located(ty)
      { DeclareExtern (ext_id, ty_name) }
  | vd = vdefinition(expr) { vd }

tdefinition:
  | PIPE? cdl = separated_nonempty_list(PIPE, constr_definition)
      { DefineSumType cdl }

constr_definition:
  | c = located(constr_id) tl = paren_comma_nonempty_list(located(ty))?
      { (c, list_of_listoption tl)}

(**
 * For
 * vdefinition := val var_id [ :type ] = expr
 *              | fun var_id [ type_variable { , type_variable } ] ]
 *                           ( pattern { , pattern } ) [ : type ] = expr
 *                           { and var_id [[type_variable {, type_variable } ]]
 *                           (pattern { , pattern })[ :type ]=expr }
 * **)
vdefinition(X):
  | VAL id = located(var_id) exp = expr_with_return_type(X)
      {
        DefineValue (id, exp)
      }
  | FUN vlist = var_id_list(X) more = option(and_var_id_list(X))
      {
        let l = [vlist] in
        match more with
        | None -> DefineRecFuns(l)
        | Some lmore -> DefineRecFuns(l @ lmore)
      }

(**
 * EXPR := expr
 * For
 * [ : type ] = EXPR
 * **)
expr_with_return_type(EXPR):
  | t = preceded(COLON, located(ty))? EQ exp = located(EXPR)
    {
      match t with
      | None -> exp
      | Some t -> Position.with_poss $startpos $endpos (TypeAnnotation(exp, t))
    }

and_var_id_list(X):
  | AND li = separated_nonempty_list(AND, var_id_list(X)) { li }

var_id_list(X):
  | id = located(var_id)
    typ_list = option(bracket_comma_nonempty_list(located(type_variable)))
    pat_list = paren_comma_nonempty_list(located(simple_pattern))
    e = expr_with_return_type(X)
    { (id, Position.with_poss $startpos $endpos
    ( FunctionDefinition( list_of_listoption(typ_list), pat_list, e))) }

(**
 * For
 * pattern ::=
 * | ( pattern )
 * | pattern | pattern
 * | pattern & pattern
 * | constr_id ( pattern { , pattern } )
 **)
pattern:
  | LPAREN p = simple_pattern RPAREN { p }
  | p1 = located(pattern) PIPE p2 = located(pattern) { POr([p1;p2]) }
  | p1 = located(pattern) AMPERSAND p2 = located(pattern) { PAnd([p1;p2]) }
  | id = located(constr_id) LPAREN pat_list =
          paren_comma_nonempty_list(located(simple_pattern)) RPAREN
    { PTaggedValue(id, pat_list) }

(**
 * For
 * pattern ::=
 * pattern : type
 * **)
simple_pattern:
  | b = base_pattern { b }
  | p = located(base_pattern) COLON t = located(ty) { PTypeAnnotation(p, t) }

(**
 * For
 * pattern ::=
 * | var_id
 * | int
 * | char
 * | string
 * | _
 * **)
base_pattern:
  | id = located(var_id) { PVariable id }
  | li = located(literal) { PLiteral li }
  | UNDERSCORE { PWildcard }

simple_ty:
  | tv = type_variable { TyVar tv }
  | LPAREN t = ty RPAREN { t }
  | tc = type_con tl = paren_comma_nonempty_list(located(ty))?
      { TyCon (tc, list_of_listoption tl) }

ty:
  | t = simple_ty { t }
  (* We force the right associativity of the type operator '->'.  *)
  | t1 = located(simple_ty) ARROW t2 = located(ty)
      { TyCon (TCon "->", [t1; t2]) }

(**
 * For
 * expr := int
 *       | char
 *       | var_id
 *       | ( expr : type )
 *       | ( expr )
 *       | ref expr
 *       | ! expr
 * **)
very_simple_expr:
  | li = located(literal) { Literal li }
  | vid = located(var_id) { Variable vid }
  | LPAREN e = located(expr) COLON t = located(ty) RPAREN
      { TypeAnnotation (e, t) }
  | LPAREN e = expr RPAREN { e }

simple_expr:
  | e = very_simple_expr { e }
  | EMARK e = located(simple_expr) { Read e }
  | REF e = located(simple_expr) { Ref e }
  | e1 = located(very_simple_expr) COLONEQ e2 = located(simple_expr)
      { Write (e1, e2) }
  | e1 = located(simple_expr) b = located(binop) e2 = located(simple_expr)
      {
        Apply( (Position.with_poss $startpos $endpos (Variable b)), [], [e1; e2] )
      }
  (* We explicit the two rules for application because without this
     (i.e.  when we use the nonterminal option for the type list), an
     unsolvable reduce/reduce conflict appears.  *)
  | e = located(very_simple_expr) el = paren_comma_nonempty_list(located(expr))
      { Apply (e, [], el) }
  | e = located(very_simple_expr) tl = bracket_comma_nonempty_list(located(ty))
    el = paren_comma_nonempty_list(located(expr))
      { Apply (e, tl, el) }

expr:
  | e = unseq_expr { e }
  | e = seq_expr { e }

(**
 * For
 * expr := { simple_expr }
 *       | constr_id / [ [type { ,type }] ] [ (expr { ,expr } ) ]
 *       | \ [ [type_variable {, type_variable }] ] (pattern {, pattern })=> expr
 *       | while expr { expr }
 *       | vdefinition ; expr
 *       | if expr then expr { elif expr then expr } [ else expr ]
 **)
unseq_expr:
  | e = simple_expr { e }
  | cid = located(constr_id) tyl = bracket_comma_nonempty_list(located(ty))?
    expl = paren_comma_nonempty_list(located(expr))?
      { Tagged(cid, list_of_listoption tyl, list_of_listoption expl) }
  | BACKSLASH tyvl = bracket_comma_nonempty_list(located(type_variable))? 
              patl = paren_comma_nonempty_list(located(simple_pattern)) IMPL 
                e = located(expr)
    { Fun ( FunctionDefinition(list_of_listoption(tyvl), patl, e) ) }
  | WHILE e1 = located(expr) LBRACE e2 = located(expr) RBRACE { While (e1, e2) }
  | e = localdef_expr { e }
  | e = cond_expr { e }

localdef_expr:
  | vd = vdefinition(simple_expr) SEMICOLON e2 = located(noncond_expr)
      {
        match vd with
        | DefineValue(x1, e1) -> Define (x1, e1, e2)
        | DefineRecFuns(li) -> DefineRec(li ,e2)
        | _ ->
          failwith "Error: DefineType and DeclareExtern \
                    should not be in the vdefinition"
      }

noncond_expr:
  | e = simple_expr { e }
  | e = localdef_expr { e }

cond_expr:
  | IF c1 = located(expr) THEN e1 = located(noncond_expr)
    l = list(elif_expr) e = preceded(ELSE, located(noncond_expr))?
      { If ((c1, e1) :: l, e) }

elif_expr:
  | ELIF c = located(expr) THEN e = located(noncond_expr) { (c, e) }

(**
 * For
 * expr := { simple_expr }
 *       | vdefinition ; expr
 *       | expr ? branches
 **)
seq_expr:
  | e = located(unseq_expr) SEMICOLON
    el = separated_nonempty_list(SEMICOLON, located(unseq_expr))
      {
        let el = e :: el in
        let el = List.rev el in
        (* It is risky to use a valid id like "nothing". *)
        let dummy_id = Position.unknown_pos (Id "nothing") in
        let f e2 e1 =
          Position.(unknown_pos (Define (dummy_id, e1, e2))) in
        Position.value (List.fold_left f (List.hd el) (List.tl el))
      }
  | e = located(unseq_expr) QMARK bl = branches { Case(e, bl)  }


(** 
 * For
 * branches ::= [ | ] branch { | branch }
 *          | { [ | ] branch { | branch } }
 * **)
branches: 
  | b = multi_branches { b }
  | LBRACE b = multi_branches RBRACE { b }

multi_branches:
  | option(PIPE) blist = separated_nonempty_list(PIPE, located(branch)) { blist }

branch:
  | p = located(simple_pattern) IMPL e = located(unseq_expr) { Branch(p, e) }

%inline var_id:
  | id = BASIC_ID | id = PREFIX_ID { Id id }

%inline constr_id:
  | id = CONSTR_ID { KId id }

%inline type_con:
  | id = BASIC_ID { TCon id }

%inline type_variable:
  | id = TYPE_VAR { TId id }

%inline literal:
  | i = INT { LInt i }
  | MINUS i = INT { LInt (Int32.neg i) }
  | c = CHAR { LChar c }
  | str = STRING { LString str }
  | FALSE { LBool false }
  | TRUE { LBool true }

%inline binop:
  | PLUS { Id "`+" }
  | MINUS { Id "`-" }
  | TIMES { Id "`*" }
  | DIV { Id "`/" }
  | LAND { Id "`&&" }
  | LOR { Id "`||" }
  | EQ { Id "`=" }
  | LEQ { Id "`<=" }
  | GEQ { Id "`>=" }
  | LT { Id "`<" }
  | GT { Id "`>" }
  | s = INFIX_ID { Id s }

%inline located(X):
  | x = X { Position.with_poss $startpos $endpos x }

%inline delim_sep_nonempty_list(opening, separator, X, closing):
  | opening l = separated_nonempty_list(separator, X) closing { l }

%inline delim_comma_nonempty_list(opening, X, closing):
  | l = delim_sep_nonempty_list(opening, COMMA, X, closing) { l }

%inline paren_comma_nonempty_list(X):
  | l = delim_comma_nonempty_list(LPAREN, X, RPAREN) { l }

%inline bracket_comma_nonempty_list(X):
  | l = delim_comma_nonempty_list(LBRACKET, X, RBRACKET) { l }
