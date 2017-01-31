(** The abstract syntax tree for hopix programs. *)

open Position

(** A program is a list of definitions. *)
type program = definition located list

and definition =
  (** A type definition. *)
  | DefineType of type_constructor located * type_variable located list * type_definition
  (** A toplevel declaration for an external value. *)
  | DeclareExtern of identifier located * ty located
  (** A toplevel definition for a value. *)
  | DefineValue of identifier located * expression located
  (** A toplevel definition for mutually recursive values. *)
  | DefineRecFuns of (identifier located * function_definition located) list

and type_definition =
  (** A sum type for tagged values [{ K₁ : ty₁₁ * ... * ty₁ₙ | ... | Kₙ : tyₙ₁ * ... * tyₘₖ}]. *)
  | DefineSumType of (constructor located * ty located list) list
  (** A type with no visible definition. *)
  | Abstract

and function_definition =
  (** A function definition [['a₁, ⋯, 'aₙ] (p₁, ⋯, pₙ) = e]. *)
  | FunctionDefinition of
      type_variable located list
    * pattern located list
    * expression located

and expression =
  (** A literal is a constant written "as is". *)
  | Literal of literal located
  (** A variable identifies a value. *)
  | Variable of identifier located
  (** A local definition [val x₁ = e₁ in e₂]. *)
  | Define of identifier located * expression located * expression located
  (** Local mutually recursive values. *)
  | DefineRec of (identifier located * function_definition located) list * expression located
  (** A function application [a [ty₁, ⋯, tyₙ] (b₁, ⋯, bₙ)]. *)
  | Apply of expression located * ty located list * expression located list
  (** A conditional expression of the form [if ... then ... elif ... else ...]. *)
  | If of (expression located * expression located) list * expression located option
  (** An anonymous function [ \ [ty₁, ⋯, ty₂] (p₁, ⋯, pₙ) => e ]. *)
  | Fun of function_definition
  (** A tagged value [K [ty₁, ⋯, tyₙ] (e_1, ..., e_n)]. *)
  | Tagged of constructor located * ty located list * expression located list
  (** A pattern matching [e ? p_1 => e_1 | ... | p_n => e_n]. *)
  | Case of expression located * branch located list
  (** A type annotation [(e : ty)]. *)
  | TypeAnnotation of expression located * ty located
  (** A reference creation [ref e]. *)
  | Ref of expression located
  (** A reference dereference [!e]. *)
  | Read of expression located
  (** A reference assignment [x := e]. *)
  | Write of expression located * expression located
  (** A loop [while e { b }] *)
  | While of expression located * expression located

and pattern =
  (** A pattern with a type annotation of type form [p : ty] *)
  | PTypeAnnotation of pattern located * ty located
  (** A pattern which is simply an identifier. *)
  | PVariable of identifier located
  (** A pattern for a tagged value [K (p_1, ..., p_n)]. *)
  | PTaggedValue of constructor located * pattern located list
  (** A wildcard pattern [_]. *)
  | PWildcard
  (** A literal pattern. *)
  | PLiteral of literal located
  (** A disjunctive pattern [ p₁ | ... | pₙ ]. *)
  | POr of pattern located list
  (** A conjunctive pattern [ p₁ & ... & pₙ ]. *)
  | PAnd of pattern located list

and branch =
  (** A branch in a pattern matching [p => e]. *)
  | Branch of pattern located * expression located

and ty =
  (** An instantiated type constructor [t [ty_1, .., ty_2]]. *)
  | TyCon of type_constructor * ty located list
  (** A type variable ['a]. *)
  | TyVar of type_variable
  (** A function type [(ty_1, .., ty_N) -> ty]. *)
  | TyArrow of ty located list * ty located

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

