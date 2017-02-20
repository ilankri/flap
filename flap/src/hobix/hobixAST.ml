(** The abstract syntax tree for hobix programs. *)

(** A program is a list of definitions. *)
type program = definition list

and definition =
  (** A toplevel declaration for an external value. *)
  | DeclareExtern of identifier
  (** A toplevel definition for a value. *)
  | DefineValue of identifier * expression
  (** A toplevel definition for mutually recursive values. *)
  | DefineRecFuns of (identifier * expression) list

and expression =
  (** A literal is a constant written "as is". *)
  | Literal of literal
  (** A variable identifies a value. *)
  | Variable of identifier
  (** A local definition [val x₁ := e₁ ; e₂]. *)
  | Define of identifier * expression * expression
  (** Local mutually recursive values [rec x₁ := e₁ and ... and xₙ := eₙ; e]. *)
  | DefineRec of (identifier * expression) list * expression
  (** A function application [a (b_1, ..., b_N)]. *)
  | Apply of expression * expression list
  (** A conditional expression of the form [if ... then ... else ... fi]. *)
  | IfThenElse of expression * expression * expression
  (** An anonymous function [ \ x => e ]. *)
  | Fun of identifier list * expression
  (** Allocate a block of size n [ alloc_block n ]. *)
  | AllocateBlock of expression
  (** Write a value v at offset i of block b [ alloc_write b i v ]. *)
  | WriteBlock of expression * expression * expression
  (** Read a value at offset i of block b [ alloc_read b i ]. *)
  | ReadBlock of expression * expression
  (** Jump to the i-th branch if i < |bs|, jump to default otherwise
      if it is present. [switch i in bs orelse default] *)
  | Switch of expression * expression array * expression option
  (** While-loop *)
  | While of expression * expression

and literal =
  | LInt    of Int32.t
  | LString of string
  | LChar   of char

and identifier =
  | Id of string

and t = program
