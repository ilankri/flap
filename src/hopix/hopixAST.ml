(** The abstract syntax tree for hopix programs. *)

open Position

(** A program is a list of definitions. *)
type program = definition located list

and definition =
  | DefineType of type_constructor located * type_variable located list *
                  type_definition
  (** A type definition.
      e.g : type aTypeCon ( oneTypeVar, twoTypeVar... ) = TypeDefinition(ty)  *)
  | DeclareExtern of identifier located * ty located
  (** A toplevel declaration for an external value.
      e.g : extern aVarId : aType *)
  | DefineValue of identifier located * expression located
  (** A toplevel definition for a value.
      e.g : var aVarId = expr *)
  | DefineRecFuns of (identifier located * function_definition located) list
  (** A toplevel definition for mutually recursive values.
      e.g :
      fun varId1 ['typeVar1, 'typeVar2] (pattern1, pattern2) : expr1Type = expr1
      and varId2 ['typeVar3] (pattern) : expr2Type = expr2... *)

and type_definition =
  | DefineSumType of (constructor located * ty located list) list
  (** A sum type for tagged values
      [{ K₁ : ty₁₁ * ... * ty₁n| ... | Km: tym1 * ... * tymn].
      e.g : ATypeCon (ty1) | BTypeCon (ty2 -> ty3) | CTypeCon (int)
  *)
  | Abstract
  (** A type with no visible definition. *)

and function_definition =
  | FunctionDefinition of
      type_variable located list
      * pattern located list
      * expression located
  (** A function definition [['a₁, ⋯, 'an] (p₁, ⋯, pm) = e].
      e.g : ['oneTypeVar, 'twoTypeVar] (pattern1, pattern2) : expr1Type = expr1
  *)

and expression =
  | Literal of literal located
  (** A literal is a constant written "as is". *)
  | Variable of identifier located
  (** A variable identifies a value. *)
  | Define of identifier located * expression located * expression located
  (** A local definition e.g : val x₁ = e₁ in e₂ *)
  | DefineRec of (identifier located * function_definition located) list *
                 expression located
  (** Local mutually recursive values. *)
  | Apply of expression located * ty located list * expression located list
  (** A function application e.g : e [ty₁, ⋯, tyn] (e₁, ⋯, em) *)
  | If of (expression located * expression located) list *
          expression located option
  (** A conditional expression of the form e.g :
      if ... then ... elif ... else .... *)
  | Fun of function_definition
  (** An anonymous function e.g : \ [ty₁, ⋯, tyn] (p₁, ⋯, pm) => e  *)
  | Tagged of constructor located * ty located list * expression located list
  (** A tagged value e.g : K [ty₁, ⋯, tyn] (e_1, ..., e_n) *)
  | Case of expression located * branch located list
  (** A pattern matching e.g : e ? p_1 => e_1 | ... | p_n => e_n *)
  | TypeAnnotation of expression located * ty located
  (** A type annotation e.g : (e : ty) *)
  | Ref of expression located
  (** A reference creation e.g : ref e *)
  | Read of expression located
  (** A reference dereference e.g : !e *)
  | Write of expression located * expression located
  (** A reference assignment e.g : x := e *)
  | While of expression located * expression located
  (** A loop e.g : while e1 \{ e2 \} *)

and pattern =
  | PTypeAnnotation of pattern located * ty located
  (** A pattern with a type annotation of type form e.g : p : ty *)
  | PVariable of identifier located
  (** A pattern which is simply an identifier. *)
  | PTaggedValue of constructor located * pattern located list
  (** A pattern for a tagged value e.g : K (p_1, ..., p_n) *)
  | PWildcard
  (** A wildcard pattern _ *)
  | PLiteral of literal located
  (** A literal pattern. *)
  | POr of pattern located list
  (** A disjunctive pattern e.g : p₁ | ... | pn *)
  | PAnd of pattern located list
  (** A conjunctive pattern e.g : p₁ & ... & pn *)

and branch =
  | Branch of pattern located * expression located
  (** A branch in a pattern matching e.g : p => e *)

and ty =
  | TyCon of type_constructor * ty located list
  (** An instantiated type constructor e.g : tcon [ty_1, .., ty_2] *)
  | TyVar of type_variable
  (** A type variable e.g : 'a. *)
  | TyArrow of ty located list * ty located
  (** A function type e.g : (ty_1, .., ty_N) -> ty *)

and literal =
  | LInt    of Int32.t
  | LString of string
  | LChar   of char

and identifier =
  | Id of string

and type_constructor =
  | TCon of string

and type_variable =
  | TId of string

and constructor =
  | KId of string

and label =
  | LId of string

and t = program
