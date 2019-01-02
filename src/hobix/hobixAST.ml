(** The abstract syntax tree for hobix programs. *)

(** A program is a list of definitions. *)
type program = definition list

and definition =
  | DeclareExtern of identifier
  (** A toplevel declaration for an external value. *)
  | DefineValue of identifier * expression
  (** A toplevel definition for a value. *)
  | DefineRecFuns of (identifier * expression) list
  (** A toplevel definition for mutually recursive values. *)

and expression =
  | Literal of literal
  (** A literal is a constant written "as is". *)
  | Variable of identifier
  (** A variable identifies a value. *)
  | Define of identifier * expression * expression
  (** A local definition [val x₁ := e₁ ; e₂]. *)
  | DefineRec of (identifier * expression) list * expression
  (** Local mutually recursive values [rec x₁ := e₁ and ... and xₙ := eₙ; e]. *)
  | Apply of expression * expression list
  (** A function application [a (b_1, ..., b_N)]. *)
  | IfThenElse of expression * expression * expression
  (** A conditional expression of the form [if ... then ... else ... fi]. *)
  | Fun of identifier list * expression
  (** An anonymous function [ \ x => e ]. *)
  | AllocateBlock of expression
  (** Allocate a block of size n [ alloc_block n ]. *)
  | WriteBlock of expression * expression * expression
  (** Write a value v at offset i of block b [ alloc_write b i v ]. *)
  | ReadBlock of expression * expression
  (** Read a value at offset i of block b [ alloc_read b i ]. *)
  | Switch of expression * expression array * expression option
  (** Jump to the i-th branch if i < |bs|, jump to default otherwise
      if it is present. [switch i in bs orelse default] *)
  | While of expression * expression
  (** While-loop *)

and literal =
  | LInt    of Int32.t
  | LString of string
  | LChar   of char

and identifier =
  | Id of string

and t = program
