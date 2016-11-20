%{
open HopixAST

let list_of_listoption = function
  | None -> []
  | Some l -> l

let prefix_id x = Id ("`" ^ x)
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
%left PLUS MINUS
%left TIMES DIV
%left INFIX_ID
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
  | VAL id = located(var_id) t = preceded(COLON, located(ty))? EQ
    exp = located(X)
      {
        let exp =
          match t with
          | None -> exp
          | Some t ->
            let pos = Position.position exp in
            Position.with_pos pos (TypeAnnotation(exp, t))
        in
        DefineValue (id, exp)
      }
  | FUN vlist = var_id_list more = option(and_var_id_list)
      {
        let l = [vlist] in
        match more with
        | None -> DefineRecFuns(l)
        | Some lmore -> DefineRecFuns(l @ lmore)
      }

and_var_id_list:
  | li = separated_nonempty_list(AND, var_id_list) { li }

var_id_list:
  | id = located(var_id) fd = located(function_definition) { (id, fd) }

function_definition:
  | typ_list = bracket_comma_nonempty_list(located(type_variable))
    pat_list = paren_comma_nonempty_list(located(pattern)) EQ
    e = located(expr)
      { FunctionDefinition(typ_list, pat_list, e) }

(**
 * For
 * pattern ::= constr_id
 * | ( pattern )
 * | constr_id ( pattern { , pattern } ) | pattern | pattern
 * | pattern & pattern
 **)
pattern:
  | p = simple_pattern { p }
  | id = located(constr_id)
    pat_list = paren_comma_nonempty_list(located(simple_pattern))
      { PTaggedValue(id, pat_list) }
  | p1 = located(pattern) PIPE p2 = located(pattern) { POr [p1; p2] }
  | p1 = located(pattern) AMPERSAND p2 = located(pattern) { PAnd [p1; p2] }

(**
 * For
 * pattern ::= constr_id
 * | var_id
 * | int
 * | char
 * | string
 * | _
 * | pattern : type
 * **)
simple_pattern:
  | LPAREN p = pattern RPAREN { p }
  | id = located(constr_id) { PTaggedValue(id, []) }
  | id = located(var_id) { PVariable id }
  | li = located(literal) { PLiteral li }
  | UNDERSCORE { PWildcard }
  | p = located(simple_pattern) COLON t = located(ty)
      { PTypeAnnotation(p, t)     }

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
        let f x = Variable (Position.map (fun x -> x) b)  in
        Apply (Position.map f b, [], [e1; e2])
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
 *       | expr := expr
 **)
unseq_expr:
  | e = simple_expr { e }
  | cid = located(constr_id) tyl = bracket_comma_nonempty_list(located(ty))?
    expl = paren_comma_nonempty_list(located(expr))?
      { Tagged(cid, list_of_listoption tyl, list_of_listoption expl) }
  | WHILE e1 = located(expr) LBRACE e2 = located(expr) RBRACE { While (e1, e2) }

(**
 * For
 * expr := { simple_expr }
 *       | vdefinition ; expr
 **)
seq_expr:
  | vd = vdefinition(unseq_expr) SEMICOLON e2 = located(expr)
      {
        match vd with
        | DefineValue(x1, e1) -> Define (x1, e1, e2)
        | DefineRecFuns(li) -> DefineRec(li ,e2)
        | _ ->
          failwith "Error: DefineType and DeclareExtern \
                    should not be in the vdefinition"
      }
  | e = located(unseq_expr) SEMICOLON
    el = separated_nonempty_list(SEMICOLON, located(unseq_expr))
      {
        let f e1 e2 =
          Position.(unknown_pos (Define (unknown_pos (Id "_"), e1, e2))) in
        Position.value (List.fold_left f e el)
      }

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
  | PLUS { prefix_id "+" }
  | MINUS { prefix_id "-" }
  | TIMES { prefix_id "*" }
  | DIV { prefix_id "/" }
  | LAND { prefix_id "&&" }
  | LOR { prefix_id "||" }
  | EQ { prefix_id "=" }
  | LEQ { prefix_id "<=" }
  | GEQ { prefix_id ">=" }
  | LT { prefix_id "<" }
  | GT { prefix_id ">" }
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
