(** The abstract syntax tree for MIPS programs. *)

open MipsArch

type label = Label of string

type 'a labelled = {
  label : label;
  value : 'a;
}

type t = {
  code        : block list;
  globals     : label list;
}

and block = instruction list labelled

and instruction =
  | Comment of string
  | Li  of register * immediate
  | Lui of register * immediate
  | Jal of label
  | J of label
  | Sw of register * address
  | Add of register * register * register
  | Addiu of register * register * immediate
  | Mul of register * register * register
  | Div of register * register * register
  | Sub of register * register * register
  | Move of register * register
  | Lw of register * address
  | Beqz of register * label
  | Bnez of register * label
  | Bge of register * register * label
  | Bgt of register * register * label
  | La of register * address
  | Slt of register * register * register
  | Sgt of register * register * register
  | Sle of register * register * register
  | Sge of register * register * register
  | Seq of register * register * register
  | Jr of register
  | Jalr of register
  | Syscall

and address =
  | RegisterAddress of register
  | RegisterOffsetAddress of register * immediate
  | LabelAddress of label

and immediate =
  | Literal of Int16.t
  | LabelAddressHi  of label
  | LabelAddressLow of label
